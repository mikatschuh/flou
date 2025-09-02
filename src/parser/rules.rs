use std::num::NonZeroU32;

use num::Integer;

use crate::{
    comp,
    error::ErrorCode,
    parser::{
        binary_op::BinaryOp,
        binding_pow,
        keyword::Keyword,
        tokenizing::{
            resolve_escape_sequences,
            token::{Token, TokenKind::*},
        },
        tree::{Bracket, Jump, Node, NodeBox, Note},
        unary_op::UnaryOp,
        NodeWrapper, Parser,
    },
    typing::Type,
};

impl<'src> Token<'src> {
    pub(super) fn nud(
        self,
        state: &mut Parser<'src>,
        min_bp: u8,
        jump: &mut Option<Jump<'src>>,
    ) -> Option<NodeBox<'src>> {
        Some(match self.kind {
            Ident => {
                if self.src == ".." {
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Placeholder))
                } else if min_bp <= 1 && state.tokenizer.next_is(|tok| tok.binding_pow() == Some(0))
                {
                    let content = state.pop_expr(1, jump, self.span.end + 1);
                    let label = state.internalizer.get(self.src);
                    state.make_node(
                        NodeWrapper::new(self.span - content.span.end)
                            .with_node(Node::Label { label, content }),
                    )
                } else if let Some(primitive_type) =
                    state.type_parser.parse_number_type(self.src.as_bytes())
                {
                    state.make_node(
                        NodeWrapper::new(self.span)
                            .with_node(Node::PrimitiveType(Type::Number(primitive_type))),
                    )
                } else {
                    match state.try_to_make_number(self.span, self.src, jump) {
                        Ok(node) => node,

                        Err(Some(suffix)) => {
                            let sym = state.internalizer.get(self.src);
                            state.make_node(
                                NodeWrapper::new(self.span)
                                    .with_node(Node::Ident(sym))
                                    .with_note(Note::NumberParsingNote {
                                        invalid_suffix: suffix,
                                    }),
                            )
                        }
                        Err(None) => {
                            let sym = state.internalizer.get(self.src);
                            state.make_node(NodeWrapper::new(self.span).with_node(Node::Ident(sym)))
                        }
                    }
                }
            }
            Quote => {
                let (string, confusions) = resolve_escape_sequences(self.src);
                state.make_node(
                    NodeWrapper::new(self.span)
                        .with_node(Node::Quote(string))
                        .with_notes(
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
                            state.pop_expr(binding_pow::STATEMENT, jump, self.span.end + 1);

                        let then_body = state.pop_path(4, condition.span.end + 1);
                        let else_body = if let Some(Token { span, .. }) =
                            state.tokenizer.next_if(|x| x.kind == Keyword(Else))
                        {
                            Some(state.pop_path(4, span.end + 1))
                        } else {
                            None
                        };
                        state.make_node(NodeWrapper::new(self.span).with_node(if keyword == If {
                            Node::If {
                                condition,
                                then_body,
                                else_body,
                            }
                        } else {
                            Node::Loop {
                                condition,
                                then_body,
                                else_body,
                            }
                        }))
                    }
                    Else => {
                        state.errors.push(self.span, ErrorCode::LonelyElse);
                        state.parse_expr(min_bp, jump)?
                    }
                    Continue => {
                        *jump = Some(Jump::Continue);
                        state.clean_up_after_jump();
                        return None;
                    }
                    Break => {
                        let val = state.parse_expr(binding_pow::STATEMENT, jump);
                        if jump.is_some() {
                            return None;
                        }
                        *jump = Some(Jump::Break {
                            val: val.map(Box::new),
                        });
                        state.clean_up_after_jump();
                        return None;
                    }
                    Return => {
                        let val = state.parse_expr(binding_pow::STATEMENT, jump);
                        if jump.is_some() {
                            return None;
                        }
                        *jump = Some(Jump::Return {
                            val: val.map(Box::new),
                        });
                        state.clean_up_after_jump();
                        return None;
                    }
                }
            }
            Tick => {
                let (ident_span, symbol) = state.pop_identifier(self.span.end + 1);
                if let Some(tok) = state
                    .tokenizer
                    .next_if(|tok| matches!(tok.kind, RightArrow))
                {
                    let rhs = state.pop_expr(binding_pow::SINGLE_VALUE, jump, tok.span.end + 1);
                    state.make_node(
                        NodeWrapper::new(ident_span - rhs.span).with_node(Node::Ref {
                            lifetime: Some(symbol),
                            val: rhs,
                        }),
                    )
                } else {
                    state.make_node(
                        NodeWrapper::new(self.span.start - ident_span)
                            .with_node(Node::Lifetime(symbol)),
                    )
                }
            }
            Open(own_bracket) => {
                state.brackets += 1;
                let Some(content) = state.parse_expr(0, jump) else {
                    let end = state.handle_closed_bracket(self.span.end + 1, own_bracket);
                    return Some(
                        state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Unit)),
                    );
                };

                let mut content = state.parse_list(content, jump);

                let end = state.handle_closed_bracket(self.span.end + 1, own_bracket);
                content.span = self.span - end;
                content
            }
            Closed(..) if state.brackets > 0 => {
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
                /*if state
                    .tokenizer
                    .next_is(|tok| matches!(tok.kind, Closed(..)))
                {
                    let lhs = state.make_node(NodeWrapper::new(self.span));
                    state.dash_slot = Some(lhs);
                    lhs
                } else*/
                if count.get().is_odd() {
                    let op = UnaryOp::Neg;
                    let operand = state.pop_expr(op.binding_pow(), jump, self.span.end + 1);
                    state.make_node(
                        NodeWrapper::new(self.span).with_node(Node::Unary { op, val: operand }),
                    )
                } else {
                    state.pop_expr(UnaryOp::Neg.binding_pow(), jump, self.span.end + 1)
                }
            }
            RightArrow => {
                let val = state.pop_expr(min_bp, jump, self.span.end + 1);
                state.make_node(NodeWrapper::new(self.span).with_node(Node::Ref {
                    lifetime: None,
                    val,
                }))
            }
            Plus | PlusPlus | NotNot => {
                state.pop_expr(UnaryOp::Neg.binding_pow(), jump, self.span.end + 1)
            }
            kind => match kind.as_prefix() {
                Some(op) => {
                    let val = state.pop_expr(op.binding_pow(), jump, self.span.end + 1);
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Unary { op, val }))
                }
                _ => {
                    state.tokenizer.buffer(self);
                    return None;
                }
            },
        })
    }

    pub(super) fn led(
        mut self,
        lhs: NodeBox<'src>,
        state: &mut Parser<'src>,
        jump: &mut Option<Jump<'src>>,
    ) -> Result<NodeBox<'src>, NodeBox<'src>> {
        Ok(match self.kind {
            Open(bracket) if matches!(bracket, Bracket::Round | Bracket::Squared) => {
                state.brackets += 1;
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    _ => unreachable!(),
                };
                let content = state.parse_expr(0, jump).unwrap_or_else(|| {
                    state.make_node(NodeWrapper::new(self.span.end() + 1).with_node(Node::Unit))
                });

                let content = state.parse_list(content, jump);
                let end = state.handle_closed_bracket(self.span.end + 1, bracket);
                state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Binary {
                    op,
                    lhs,
                    rhs: content,
                }))
            }
            Equal => state.parse_dynamic_arg_op(
                lhs,
                Equal,
                binding_pow::BINDING,
                |exprs| Node::Binding { exprs },
                self.span.end + 1,
                jump,
            ),
            Colon => {
                let rhs = state.pop_expr(binding_pow::COLON, jump, self.span.end + 1);
                state.make_node(
                    NodeWrapper::new(lhs.span - rhs.span).with_node(Node::Contract { lhs, rhs }),
                )
            }
            EqualPipe => {
                state.tokenizer.buffer(Token {
                    span: self.span.end(),
                    src: &self.src[1..2],
                    kind: Pipe,
                });

                state.parse_dynamic_arg_op(
                    lhs,
                    Equal,
                    binding_pow::BINDING,
                    |exprs| Node::Binding { exprs },
                    self.span.end + 1,
                    jump,
                )
            }
            Dash(count) if count.get() > 1 => {
                if let Some(new_count) = NonZeroU32::new(count.get() - 2) {
                    self.kind = Dash(new_count);
                    state.tokenizer.buffer(self);
                }
                state.make_node(
                    NodeWrapper::new(lhs.span - self.span).with_node(Node::Unary {
                        op: UnaryOp::Dec,
                        val: lhs,
                    }),
                )
            }
            Dash(..) => {
                let op = BinaryOp::Sub;
                let rhs = state.pop_expr(op.binding_pow(), jump, self.span.end + 1);
                state.make_node(
                    NodeWrapper::new(lhs.span - rhs.span).with_node(Node::Binary { op, lhs, rhs }),
                )
            }
            _ => {
                if let Some(op) = self.kind.as_postfix() {
                    state.make_node(
                        NodeWrapper::new(lhs.span - self.span)
                            .with_node(Node::Unary { op, val: lhs }),
                    )
                } else if let Some(op) = self.kind.as_infix() {
                    let rhs = state.pop_expr(op.binding_pow(), jump, self.span.end + 1);
                    if op.is_chained() {
                        let mut chain = comp::Vec::new([(op, rhs)]);
                        while let Some(op) =
                            state.tokenizer.peek().and_then(|op| op.kind.as_infix())
                        {
                            if op.is_chained() {
                                let Token { span, .. } = state.tokenizer.next().unwrap();
                                let rhs = state.pop_expr(op.binding_pow(), jump, span.end + 1);
                                chain.push((op, rhs));
                                continue;
                            }
                            break;
                        }
                        state.make_node(NodeWrapper::new(lhs.span - chain.last().1.span).with_node(
                            Node::Chain {
                                first: lhs,
                                additions: chain,
                            },
                        ))
                    } else {
                        state.make_node(
                            NodeWrapper::new(lhs.span - rhs.span).with_node(Node::Binary {
                                op,
                                lhs,
                                rhs,
                            }),
                        )
                    }
                } else {
                    return Err(lhs);
                }
            }
        })
    }
}
