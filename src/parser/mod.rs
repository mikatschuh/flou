use crate::{
    error::{ErrorCode, Errors, Position, Span},
    parser::intern::{Internalizer, Symbol},
    tree::{Bracket, Jump, NodeId, NodeWrapper, NodeWrapping, Path, Tree},
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

pub fn parse<'src>(text: &'src str, path: &'static std::path::Path) -> (String, Rc<Errors<'src>>) {
    let errors = Rc::new(Errors::empty(path));
    let internalizer = Rc::new(Internalizer::new());

    let (root, tree) = Parser::<NodeWrapper>::new(
        Tokenizer::new(text, errors.clone()),
        internalizer.clone(),
        errors.clone(),
    )
    .parse();

    (tree.to_string(root, &internalizer), errors)
}

struct Parser<'src, W: NodeWrapping<'src>> {
    tokenizer: Tokenizer<'src>,
    errors: Rc<Errors<'src>>,
    internalizer: Rc<Internalizer<'src>>,
    tree: Tree<'src, W>,
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
            tree: Tree::new(),
        }
    }
    #[inline]
    fn parse(mut self) -> (NodeId<'src>, Tree<'src, W>) {
        (
            self.parse_path(Position::beginning(), 0, false).content,
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
            let bp = if tok.kind == TokenKind::Comma && flags.in_brackets {
                0
            } else {
                tok.left_bp()
            };
            if tok.is_terminator() || bp < min_bp {
                return Some(lhs);
            } else {
                lhs = self.tokenizer.next().unwrap().led(lhs, self, flags);
            }
        }
        // EOF
        Some(lhs)
    }

    #[inline]
    fn parse_path(&mut self, pos: Position, min_bp: u8, in_brackets: bool) -> Path<NodeId<'src>> {
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

    fn handle_closed_bracket(
        &mut self,
        pos: Position,
        own_span: Span,
        own_bracket: Bracket,
    ) -> Span {
        use TokenKind::*;
        if matches!(self.tokenizer.peek(), Some(Token { kind: Closed(bracket), .. }) if *bracket == own_bracket)
        {
            self.tokenizer.next().unwrap().span
        } else if let Some(Token {
            kind: Closed(found),
            ..
        }) = self.tokenizer.peek()
        {
            let found = *found;
            let span = self.tokenizer.next().unwrap().span;
            self.errors.push(
                own_span - span,
                ErrorCode::WrongClosedBracket {
                    expected: own_bracket,
                    found,
                },
            );
            span
        } else {
            self.errors.push(
                own_span,
                ErrorCode::NoClosedBracket {
                    opened: own_bracket,
                },
            );
            pos.into()
        }
    }

    /// Pops an identifier if the next token is one. If not it generates the correct error message
    /// and leaves the token there. The position indicates were the identifier is expected to go.
    fn pop_identifier(&mut self, pos: Position) -> Option<(Span, Symbol<'src>)> {
        if let Some(Token {
            kind, span, src, ..
        }) = self.tokenizer.peek()
        {
            if let TokenKind::Ident = kind {
                let Token { src, span, .. } = self.tokenizer.next().unwrap();
                Some((span, self.internalizer.get(src)))
            } else {
                self.errors
                    .push(*span, ErrorCode::ExpectedIdent { found: src });
                None
            }
        } else {
            self.errors
                .push(pos.into(), ErrorCode::ExpectedIdentFoundEOF);
            None
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
            self.errors.push(pos.into(), ErrorCode::ExpectedValue);
            self.tree.add(W::new(pos.into()))
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Flags<'src, 'caller> {
    pub in_brackets: bool,
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
            in_brackets: false,
            loc: Location::Path(jump),
        }
    }
    fn in_brackets(mut self) -> Self {
        self.in_brackets = true;
        self
    }
    fn outside_of_brackets(mut self) -> Self {
        self.in_brackets = false;
        self
    }
    fn in_brackets_if(mut self, condition: bool) -> Self {
        self.in_brackets = condition;
        self
    }
    fn location(mut self, loc: Location<'src, 'caller>) -> Self {
        self.loc = loc;
        self
    }
}
