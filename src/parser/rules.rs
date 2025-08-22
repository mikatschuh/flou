use std::num::NonZeroU32;

use num::Integer;

use crate::{
    comp,
    error::ErrorCode,
    parser::{
        binary_op::BinaryOp,
        binding_pow::{self},
        keyword::Keyword,
        tokenizing::{
            resolve_escape_sequences,
            token::{Token, TokenKind::*},
        },
        unary_op::UnaryOp,
        Flags,
        Location::*,
        Parser,
    },
    tree::{Bracket, Jump, Node, NodeId, NodeWrapping, Note},
};

impl<'src> Token<'src> {
    pub fn is_terminator(self) -> bool {
        matches!(self.kind, Closed(..))
    }

    pub(super) fn nud<'caller, W: NodeWrapping<'src> + 'src>(
        self,
        state: &mut Parser<'src, W>,
        min_bp: u8,
        flags: Flags<'src, 'caller>,
    ) -> Option<NodeId<'src>> {
        Some(match self.kind {
            Ident => {
                if let Some(src) = self.src.strip_prefix('.') {
                    if let "." = src {
                        state
                            .tree
                            .add(W::new(self.span).with_node(Node::Placeholder))
                    } else if !src.is_empty() {
                        state.tree.add(
                            W::new(self.span).with_node(Node::Field(state.internalizer.get(src))),
                        )
                    } else {
                        state.errors.push(self.span, ErrorCode::IdentWithJustDot);
                        state.tree.add(W::new(self.span))
                    }
                } else {
                    state.convert_to_num_if_possible(self.span, self.src, flags)
                }
            }
            Quote => {
                let (string, confusions) = resolve_escape_sequences(self.src);
                state.tree.add(
                    W::new(self.span).with_node(Node::Quote(string)).with_notes(
                        confusions
                            .into_iter()
                            .map(Note::EscapeSequenceConfusion)
                            .collect(),
                    ),
                )
            }
            Keyword(keyword) => {
                use Keyword::*;
                match keyword {
                    If | Loop => {
                        let condition =
                            state.pop_expr(self.span.end + 1, 4, flags.outside_of_brackets());
                        let then_body = state.pop_path(
                            state.tree[condition].span().end + 1,
                            4,
                            flags.in_brackets,
                        );
                        let else_body = if let Some(Token { span, .. }) =
                            state.tokenizer.next_if(|x| x.kind == Keyword(Else))
                        {
                            Some(state.pop_path(span.end + 1, 4, flags.in_brackets))
                        } else {
                            None
                        };
                        state
                            .tree
                            .add(W::new(self.span).with_node(Node::Conditional {
                                condition,
                                looping: keyword == Loop,
                                then_body,
                                else_body,
                            }))
                    }
                    Else => {
                        state.errors.push(self.span, ErrorCode::LonelyElse);
                        state.parse_expr(min_bp, flags)?
                    }
                    Continue => match flags.loc {
                        Path(jump) => {
                            let layers = 1 + state // +1 because we already saw continue ones
                                .tokenizer
                                .consume_while(|token| token.kind == Keyword(Continue))
                                .count();

                            jump.write(Some(Box::new(Jump::Continue { layers })));
                            state.clean_up_after_jump();
                            return None;
                        }
                        FuncArgType => {
                            state.errors.push(
                                self.span,
                                ErrorCode::JumpInsideFuncArg { keyword: self.src },
                            );
                            state.parse_expr(min_bp, flags)?
                        }
                    },
                    Exit => match flags.loc {
                        Path(jump) => {
                            let layers = 1 + state
                                .tokenizer
                                .consume_while(|token| token.kind == Keyword(Exit))
                                .count();
                            jump.write(Some(Box::new(Jump::Exit {
                                layers,
                                val: state.parse_path(self.span.end + 1, 4, flags.in_brackets),
                            })));
                            state.clean_up_after_jump();
                            return None;
                        }
                        FuncArgType => {
                            state.errors.push(
                                self.span,
                                ErrorCode::JumpInsideFuncArg { keyword: self.src },
                            );
                            state.parse_expr(min_bp, flags)?
                        }
                    },
                    Break => todo!(),
                    Return => match flags.loc {
                        Path(jump) => {
                            jump.write(Some(Box::new(Jump::Return {
                                val: state.parse_path(self.span.end + 1, 4, flags.in_brackets),
                            })));
                            state.clean_up_after_jump();
                            return None;
                        }
                        FuncArgType => {
                            state.errors.push(
                                self.span,
                                ErrorCode::JumpInsideFuncArg { keyword: self.src },
                            );
                            state.parse_expr(min_bp, flags)?
                        }
                    },
                }
            }
            Tick => {
                let (ident_span, symbol) = state.pop_identifier(self.span.end + 1);
                if let Some(tok) = state
                    .tokenizer
                    .next_if(|tok| matches!(tok.kind, RightArrow))
                {
                    let rhs = state.pop_expr(tok.span.end + 1, binding_pow::SINGLE_VALUE, flags);
                    state
                        .tree
                        .add(
                            W::new(ident_span - state.tree[rhs].span()).with_node(Node::Ref {
                                lifetime: Some(symbol),
                                val: rhs,
                            }),
                        )
                } else {
                    state
                        .tree
                        .add(W::new(self.span.start - ident_span).with_node(Node::Lifetime(symbol)))
                }
            }
            Open(own_bracket) => {
                if let Some(content) = state.parse_expr(0, flags.in_brackets(own_bracket)) {
                    let end =
                        state.handle_terminator(self.span.end + 1, self.span, own_bracket, flags);
                    state.tree[content].span_mut().end = end.end;
                    content
                } else {
                    let end =
                        state.handle_terminator(self.span.end + 1, self.span, own_bracket, flags);
                    state
                        .tree
                        .add(W::new(self.span.start - end).with_node(Node::Unit))
                }
            }
            Closed(..) if flags.in_brackets.is_some() => {
                state.tokenizer.buffer(self);
                return None;
            }
            Closed(bracket) => {
                state
                    .errors
                    .push(self.span, ErrorCode::NoOpenedBracket { closed: bracket });
                return None;
            }
            Dash(count) => {
                if state
                    .tokenizer
                    .next_is(|tok| matches!(tok.kind, Closed(..)))
                {
                    let lhs = state.tree.add(W::new(self.span));
                    state.dash_slot = Some(lhs);
                    lhs
                } else if count.get().is_odd() {
                    let op = UnaryOp::Neg;
                    let operand = state.pop_expr(self.span.end + 1, op.binding_pow(), flags);
                    state
                        .tree
                        .add(W::new(self.span).with_node(Node::Unary { op, val: operand }))
                } else {
                    state.pop_expr(self.span.end + 1, UnaryOp::Neg.binding_pow(), flags)
                }
            }
            RightArrow => {
                let val = state.pop_expr(self.span.end + 1, min_bp, flags);
                state.tree.add(W::new(self.span).with_node(Node::Ref {
                    lifetime: None,
                    val,
                }))
            }
            Plus | PlusPlus | NotNot => {
                state.pop_expr(self.span.end + 1, UnaryOp::Neg.binding_pow(), flags)
            }
            kind => match kind.as_prefix() {
                Some(op) => {
                    let val = state.pop_expr(self.span.end + 1, op.binding_pow(), flags);
                    state
                        .tree
                        .add(W::new(self.span).with_node(Node::Unary { op, val }))
                }
                _ => {
                    state.tokenizer.buffer(self);
                    state.tree.add(W::new(self.span.start.into()))
                }
            },
        })
    }

    pub(super) fn led<'caller, W: NodeWrapping<'src> + 'src>(
        mut self,
        lhs: NodeId<'src>,
        state: &'caller mut Parser<'src, W>,
        flags: Flags<'src, 'caller>,
    ) -> NodeId<'src> {
        match self.kind {
            Open(Bracket::Round | Bracket::Squared) => {
                let bracket = match self.kind {
                    Open(bracket) => bracket,
                    _ => unreachable!(),
                };
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    _ => unreachable!(),
                };
                let rhs = state
                    .parse_expr(0, flags.in_brackets(bracket))
                    .unwrap_or_else(|| {
                        state
                            .tree
                            .add(W::new(self.span - (self.span + 1)).with_node(Node::Unit))
                    });
                let end = state.handle_terminator(self.span.end + 1, self.span, bracket, flags);
                state.tree[rhs].span_mut().end = end.end;
                state.tree.add(
                    W::new(state.tree[lhs].span() - end).with_node(Node::Binary { op, lhs, rhs }),
                )
            }
            Comma => {
                let bp = binding_pow::comma(flags.comma_override());

                let rhs = state.pop_expr(self.span.end + 1, bp, flags);
                let mut chain = comp::Vec::new([lhs, rhs]);

                while let Some(Token { kind: Comma, .. }) = state.tokenizer.peek() {
                    let Token { span, .. } = state.tokenizer.next().unwrap();
                    chain.push(state.pop_expr(span.end + 1, bp, flags));
                }

                state.tree.add(
                    W::new(state.tree[lhs].span() - state.tree[*chain.last()].span())
                        .with_node(Node::List(chain)),
                )
            }
            Equal => {
                let rhs = state.pop_expr(self.span.end + 1, binding_pow::BINDING, flags);
                state.tree.add(
                    W::new(state.tree[lhs].span() - state.tree[rhs].span())
                        .with_node(Node::Binding { lhs, rhs }),
                )
            }
            Colon => {
                let rhs = state.pop_expr(self.span.end + 1, binding_pow::COLON, flags);
                let mut chain = comp::Vec::new([lhs, rhs]);
                while state
                    .tokenizer
                    .peek()
                    .is_some_and(|token| token.kind == Colon)
                {
                    let Token { span, .. } = state.tokenizer.next().unwrap();
                    let rhs = state.pop_expr(span.end + 1, binding_pow::COLON, flags);
                    chain.push(rhs)
                }
                state.tree.add(
                    W::new(state.tree[lhs].span() - state.tree[*chain.last()].span())
                        .with_node(Node::ColonStruct(chain)),
                )
            }
            EqualPipe => {
                state.tokenizer.buffer(Token {
                    span: self.span.end(),
                    src: &self.src[1..2],
                    kind: Pipe,
                });
                let rhs = state.pop_expr(self.span.end + 1, binding_pow::BINDING, flags);
                state.tree.add(
                    W::new(state.tree[lhs].span() - state.tree[rhs].span())
                        .with_node(Node::Binding { lhs, rhs }),
                )
            }
            Dash(count) if count.get() > 1 => {
                if let Some(new_count) = NonZeroU32::new(count.get() - 2) {
                    self.kind = Dash(new_count);
                    state.tokenizer.buffer(self);
                }
                state.tree.add(
                    W::new(state.tree[lhs].span() - self.span).with_node(Node::Unary {
                        op: UnaryOp::Dec,
                        val: lhs,
                    }),
                )
            }
            Dash(..) => {
                let op = BinaryOp::Sub;
                let rhs = state.pop_expr(self.span.end + 1, op.binding_pow(), flags);
                state.tree.add(
                    W::new(state.tree[lhs].span() - state.tree[rhs].span())
                        .with_node(Node::Binary { op, lhs, rhs }),
                )
            }
            _ => {
                if let Some(op) = self.kind.as_postfix() {
                    state.tree.add(
                        W::new(state.tree[lhs].span() - self.span)
                            .with_node(Node::Unary { op, val: lhs }),
                    )
                } else if let Some(op) = self.kind.as_infix() {
                    let rhs = state.pop_expr(self.span.end + 1, op.binding_pow(), flags);
                    if op.is_chained() {
                        let mut chain = comp::Vec::new([(op, rhs)]);
                        while let Some(op) =
                            state.tokenizer.peek().and_then(|op| op.kind.as_infix())
                        {
                            if op.is_chained() {
                                let Token { span, .. } = state.tokenizer.next().unwrap();
                                let rhs = state.pop_expr(span.end + 1, op.binding_pow(), flags);
                                chain.push((op, rhs));
                                continue;
                            }
                            break;
                        }
                        state.tree.add(
                            W::new(state.tree[lhs].span() - state.tree[chain.last().1].span())
                                .with_node(Node::Chain {
                                    first: lhs,
                                    additions: chain,
                                }),
                        )
                    } else {
                        state.tree.add(
                            W::new(state.tree[lhs].span() - state.tree[rhs].span())
                                .with_node(Node::Binary { op, lhs, rhs }),
                        )
                    }
                } else {
                    state.tokenizer.buffer(self);

                    let Some(rhs) = state.parse_expr(binding_pow::STATEMENT, flags) else {
                        return lhs;
                    };
                    let mut chain = comp::Vec::new([lhs, rhs]);
                    while let Some(rhs) = state.parse_expr(binding_pow::STATEMENT, flags) {
                        chain.push(rhs)
                    }
                    state.tree.add(
                        W::new(state.tree[lhs].span() - state.tree[*chain.last()].span())
                            .with_node(Node::Statements(chain)),
                    )
                }
            }
        }
    }
}
