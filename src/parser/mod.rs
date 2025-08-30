use std::marker::PhantomData;

use crate::{
    comp,
    error::{ErrorCode, Errors, Position, Span},
    parser::intern::{Internalizer, Symbol},
    tree::{Bracket, EveryJump, Jump, NoJump, Node, NodeBox, NodeWrapper, Path},
    typing::TypeParser,
    utilities::{Rc, Ref},
};
use bumpalo::{boxed::Box as BumpBox, Bump};
use tokenizing::{
    token::{Token, TokenKind},
    Tokenizer,
};

pub mod binary_op;
mod binding_pow;
pub mod intern;
#[allow(dead_code)]
pub mod keyword;
pub mod num;
mod rules;
#[cfg(test)]
mod test;
pub mod tokenizing;
pub mod unary_op;

pub fn parse<'src>(
    text: &'src str,
    arena: &'src Bump,
    path: &'static std::path::Path,
) -> (
    NodeBox<'src, NoJump>,
    Rc<Internalizer<'src>>,
    Rc<Errors<'src>>,
) {
    let errors = Rc::new(Errors::empty(path));
    let internalizer = Rc::new(Internalizer::new());
    let root = Parser::new(
        Tokenizer::new(text, errors.clone()),
        internalizer.clone(),
        errors.clone(),
        arena,
    )
    .parse();

    (root, internalizer, errors)
}

struct Parser<'src> {
    tokenizer: Tokenizer<'src>,
    errors: Rc<Errors<'src>>,
    internalizer: Rc<Internalizer<'src>>,
    type_parser: TypeParser,
    arena: &'src Bump,

    dash_slot: Option<Ref<'src, NodeBox<'src, EveryJump<'src>>>>,
}

struct Tree<'src, J: Jump<'src>> {
    node: NodeBox<'src, J>,
    jump: J,
}

impl<'src> Parser<'src> {
    #[inline]
    fn new(
        tokenizer: Tokenizer<'src>,
        internalizer: Rc<Internalizer<'src>>,
        errors: Rc<Errors<'src>>,
        arena: &'src Bump,
    ) -> Self {
        Self {
            tokenizer,
            errors,
            internalizer,
            type_parser: TypeParser::new(),
            arena,

            dash_slot: None,
        }
    }

    #[inline]
    fn make_node<J: Jump<'src>>(&self, node: NodeWrapper<'src, J>) -> NodeBox<'src, J> {
        NodeBox::new(BumpBox::new_in(node, self.arena))
    }

    #[inline]
    fn parse(mut self) -> NodeBox<'src, NoJump> {
        let root = self.pop_path(Position::beginning(), 0, None).content;
        root
    }

    fn parse_expr<'caller, J: Jump<'src>>(
        &mut self,
        min_bp: u8,
        flags: StackData<'src, 'caller, J>,
    ) -> Option<NodeBox<'src, J>> {
        let mut lhs = self.tokenizer.next()?.nud(self, min_bp, flags)?;

        while let Some(tok) = self.tokenizer.peek() {
            if flags.in_brackets.is_none() {
                if let TokenKind::Closed(closed) = tok.kind {
                    let Token { span, .. } = self.tokenizer.next().unwrap();
                    self.errors
                        .push(span, ErrorCode::NoOpenedBracket { closed });
                    continue;
                }
            } // Generate missing opening bracket error

            let Some(bp) = tok.binding_pow() else {
                return Some(lhs);
            };
            if bp < min_bp {
                return Some(lhs);
            }
            lhs = self.tokenizer.next().unwrap().led(lhs, self, flags);
        }
        // EOF
        Some(lhs)
    }

    fn parse_path<J: Jump<'src>>(
        &mut self,
        pos: Position,
        min_bp: u8,
        in_brackets: Option<InBrackets>,
    ) -> Path<'src, J> {
        let mut jump = J::NONE;
        Path {
            content: self
                .parse_expr(
                    min_bp,
                    StackData::new(Ref::new(&mut jump)).in_brackets_if(in_brackets),
                )
                .unwrap_or_else(|| self.make_node(NodeWrapper::new(pos.into()))),
            jump,
        }
    }

    fn pop_path<J: Jump<'src>>(
        &mut self,
        pos: Position,
        min_bp: u8,
        in_brackets: Option<InBrackets>,
    ) -> Path<'src, J> {
        let mut jump = J::NONE;
        Path {
            content: self.pop_expr(
                pos,
                min_bp,
                StackData::new(Ref::new(&mut jump)).in_brackets_if(in_brackets),
            ),
            jump,
        }
    }

    fn handle_closed_bracket<'caller, J: Jump<'src>>(
        &mut self,
        pos: Position,
        open_pos: Span,
        open_bracket: Bracket,
        flags: StackData<'src, 'caller, J>,
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

                if !self.tokenizer.next_is(|tok| matches!(tok.kind, Closed(..))) {
                    self.dash_slot = None;
                    let node = self.pop_expr(pos, binding_pow::SINGLE_VALUE, flags);
                    dash_slot.write(node);
                }
            }
            span
        } else {
            let mut left_over_tokens = self
                .tokenizer
                .consume_while(|tok| !matches!(tok.kind, TokenKind::Closed(..)));

            if let Some(tok) = left_over_tokens.next() {
                self.errors.push(
                    tok.span.start
                        - left_over_tokens
                            .last()
                            .map_or(tok.span.end, |last| last.span.end),
                    ErrorCode::ExpectedClosedBracket {
                        opened: open_bracket,
                    },
                )
            }

            self.errors.push(
                pos.into(),
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
    fn pop_expr<'caller, J: Jump<'src>>(
        &mut self,
        pos: Position,
        min_bp: u8,
        flags: StackData<'src, 'caller, J>,
    ) -> NodeBox<'src, J> {
        self.parse_expr(min_bp, flags).unwrap_or_else(|| {
            self.errors.push(pos.into(), ErrorCode::ExpectedValue);
            self.make_node(NodeWrapper::new(pos.into()))
        })
    }

    fn parse_list<'caller, J: Jump<'src>>(
        &mut self,
        first: &mut NodeBox<'src, J>,
        flags: StackData<'src, 'caller, J>,
    ) {
        let Some(Token { span, .. }) = self.tokenizer.next_if(|tok| tok.kind == TokenKind::Comma)
        else {
            return;
        };
        let mut args = comp::Vec::new([first.clone(), self.pop_expr(span.end + 1, 0, flags)]);

        while let Some(Token { span, .. }) =
            self.tokenizer.next_if(|tok| tok.kind == TokenKind::Comma)
        {
            args.push(self.pop_expr(span.end + 1, 0, flags))
        }

        *first = self
            .make_node(NodeWrapper::new(first.span - args.last().span).with_node(Node::List(args)));
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum InBrackets {
    Container,
    Application,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct StackData<'src, 'caller, J: Jump<'src>> {
    pub in_brackets: Option<InBrackets>,
    pub loc: Location<'src, 'caller, J>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Location<'src, 'caller, J: Jump<'src>> {
    Path(PhantomData<&'src ()>, &'caller mut J),
    FuncArg,
}

impl<'src, 'caller, J: Jump<'src>> StackData<'src, 'caller, J> {
    #[inline]
    fn new(jump: Ref<'caller, J>) -> Self {
        Self {
            in_brackets: None,
            loc: Location::Path(PhantomData::default(), jump),
        }
    }

    #[inline]
    fn in_brackets(mut self, bracket: InBrackets) -> Self {
        self.in_brackets = Some(bracket);
        self
    }

    #[inline]
    fn in_brackets_if(mut self, bracket: Option<InBrackets>) -> Self {
        self.in_brackets = bracket;
        self
    }

    #[inline]
    fn outside_of_brackets(mut self) -> Self {
        self.in_brackets = None;
        self
    }

    #[inline]
    fn location(mut self, loc: Location<'src, 'caller, J>) -> Self {
        self.loc = loc;
        self
    }
}
