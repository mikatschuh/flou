use crate::{
    error::{ErrorCode, Errors, Position, Span},
    parser::intern::{Internalizer, Symbol},
    tree::{Bracket, Jump, NodeId, NodeWrapper, NodeWrapping, Path, Tree},
    typing::TypeParser,
    utilities::{Rc, Ref},
};
use tokenizing::{
    token::{Token, TokenKind},
    Tokenizer,
};

pub mod binary_op;
mod binding_pow;
pub mod intern;
#[allow(dead_code)]
pub mod item;
#[allow(dead_code)]
pub mod keyword;
pub mod num;
mod rules;
#[cfg(test)]
mod test;
pub mod tokenizing;
pub mod unary_op;

#[macro_export]
macro_rules! unpack {
    ($pat:pat = $expr:expr => $body:expr) => {
        if let $pat = $expr {
            $body
        } else {
            unreachable!()
        }
    };
    ($expr:expr => $pat:pat => $body:expr) => {
        if let $pat = $expr {
            $body
        } else {
            unreachable!()
        }
    };
}

pub fn parse<'src>(
    text: &'src str,
    path: &'static std::path::Path,
) -> (
    (NodeId<'src>, Tree<'src, NodeWrapper<'src>>),
    Rc<Internalizer<'src>>,
    Rc<Errors<'src>>,
) {
    let errors = Rc::new(Errors::empty(path));
    let internalizer = Rc::new(Internalizer::new());

    let (root, tree) = Parser::<NodeWrapper>::new(
        Tokenizer::new(text, errors.clone()),
        internalizer.clone(),
        errors.clone(),
    )
    .parse();

    ((root, tree), internalizer, errors)
}

struct Parser<'src, W: NodeWrapping<'src>> {
    tokenizer: Tokenizer<'src>,
    errors: Rc<Errors<'src>>,
    internalizer: Rc<Internalizer<'src>>,
    type_parser: TypeParser,
    tree: Tree<'src, W>,
    dash_slot: Option<NodeId<'src>>,
}
impl<'src, W: NodeWrapping<'src> + 'src> Parser<'src, W> {
    #[inline]
    fn new(
        tokenizer: Tokenizer<'src>,
        internalizer: Rc<Internalizer<'src>>,
        errors: Rc<Errors<'src>>,
    ) -> Self {
        Self {
            tokenizer,
            errors,
            internalizer,
            type_parser: TypeParser::new(),
            tree: Tree::new(),
            dash_slot: None,
        }
    }
    #[inline]
    fn parse(mut self) -> (NodeId<'src>, Tree<'src, W>) {
        (
            self.pop_path(Position::beginning(), 0, None).content,
            self.tree,
        )
    }

    fn parse_expr<'caller>(
        &mut self,
        min_bp: u8,
        flags: Flags<'src, 'caller>,
    ) -> Option<NodeId<'src>> {
        let mut lhs = self.tokenizer.next()?.nud(self, min_bp, flags)?;

        while let Some(tok) = self.tokenizer.peek() {
            let bp = tok.binding_pow(flags.comma_override());
            if flags.in_brackets.is_none() {
                if let TokenKind::Closed(closed) = tok.kind {
                    let Token { span, .. } = self.tokenizer.next().unwrap();
                    self.errors
                        .push(span, ErrorCode::NoOpenedBracket { closed });
                    return Some(lhs);
                }
            }
            if tok.is_terminator() || bp < min_bp {
                return Some(lhs);
            } else {
                lhs = self.tokenizer.next().unwrap().led(lhs, self, flags);
            }
        }
        // EOF
        Some(lhs)
    }

    fn pop_path(
        &mut self,
        pos: Position,
        min_bp: u8,
        in_brackets: Option<Bracket>,
    ) -> Path<NodeId<'src>> {
        let mut jump = None;
        Path {
            content: self.pop_expr(
                pos,
                min_bp,
                Flags {
                    in_brackets,
                    loc: Location::Path(Ref::new(&mut jump)),
                },
            ),
            jump,
        }
    }

    fn parse_path(
        &mut self,
        pos: Position,
        min_bp: u8,
        in_brackets: Option<Bracket>,
    ) -> Path<NodeId<'src>> {
        let mut jump = None;
        Path {
            content: self
                .parse_expr(
                    min_bp,
                    Flags {
                        in_brackets,
                        loc: Location::Path(Ref::new(&mut jump)),
                    },
                )
                .unwrap_or_else(|| self.tree.add(W::new(pos.into()))),
            jump,
        }
    }

    fn handle_terminator<'caller>(
        &mut self,
        pos: Position,
        own_span: Span,
        open_bracket: Bracket,
        flags: Flags<'src, 'caller>,
    ) -> Span {
        use TokenKind::*;
        if let Some(Token {
            kind: Closed(closed_bracket),
            ..
        }) = self.tokenizer.peek()
        {
            let found = *closed_bracket;
            let span = self.tokenizer.next().unwrap().span;
            if found != open_bracket {
                self.errors.push(
                    span,
                    ErrorCode::WrongClosedBracket {
                        expected: open_bracket,
                        found,
                    },
                );
            }
            if let Some(dash_slot) = self.dash_slot {
                let mut pos = span.end + 1;
                if let Some(tok) = self.tokenizer.next_if(|tok| matches!(tok.kind, Dash(..))) {
                    pos = tok.span.end + 1
                }

                if !self.tokenizer.next_is(|tok| tok.is_terminator()) {
                    self.dash_slot = None;
                    let node = self.pop_expr(pos, u8::MAX, flags);
                    self.tree.move_to(node, dash_slot);
                }
            }
            span
        } else {
            self.errors.push(
                own_span,
                ErrorCode::NoClosedBracket {
                    opened: open_bracket,
                },
            );
            pos.into()
        }
    }

    fn clean_up_after_jump(&mut self) {
        if let Some(Token { span, .. }) = self.tokenizer.next_if(|tok| !tok.is_terminator()) {
            self.errors.push(span, ErrorCode::CodeAfterJump);
        }
        self.tokenizer.consume_while(|tok| !tok.is_terminator());
    }

    /// Pops an identifier if the next token is one. If not it generates the correct error message
    /// and leaves the token there. The position indicates were the identifier is expected to go.
    fn pop_identifier(&mut self, pos: Position) -> (Span, Symbol<'src>) {
        if let Some(tok) = self
            .tokenizer
            .next_if(|tok| matches!(tok.kind, TokenKind::Ident))
        {
            (tok.span, self.internalizer.get(tok.src))
        } else {
            self.errors.push(pos.into(), ErrorCode::ExpectedIdent);
            (pos.into(), self.internalizer.empty())
        }
    }

    /// Parses a value, if no value can be generated it makes an error message and returns an
    /// empty node.
    fn pop_expr<'caller>(
        &mut self,
        pos: Position,
        min_bp: u8,
        flags: Flags<'src, 'caller>,
    ) -> NodeId<'src> {
        self.parse_expr(min_bp, flags).unwrap_or_else(|| {
            dbg!(self.tokenizer.clone().collect::<Vec<_>>());
            self.errors.push(pos.into(), ErrorCode::ExpectedValue);
            self.tree.add(W::new(pos.into()))
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Flags<'src, 'caller> {
    pub in_brackets: Option<Bracket>,
    pub loc: Location<'src, 'caller>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Location<'src, 'caller> {
    Path(Ref<'caller, Option<Box<Jump<NodeId<'src>>>>>),
    FuncArgType,
}

impl<'src, 'caller> Flags<'src, 'caller> {
    #[inline]
    const fn new(jump: Ref<'caller, Option<Box<Jump<NodeId<'src>>>>>) -> Self {
        Self {
            in_brackets: None,
            loc: Location::Path(jump),
        }
    }
    fn in_brackets(mut self, bracket: Bracket) -> Self {
        self.in_brackets = Some(bracket);
        self
    }
    fn comma_override(self) -> bool {
        matches!(self.in_brackets, Some(Bracket::Round | Bracket::Squared))
    }
    fn outside_of_brackets(mut self) -> Self {
        self.in_brackets = None;
        self
    }
    fn location(mut self, loc: Location<'src, 'caller>) -> Self {
        self.loc = loc;
        self
    }
}
