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
        tree::{Bracket, Jump, Node, NodeBox, Note, Path},
        unary_op::UnaryOp,
        BindingPow, NodeWrapper, Parser,
    },
    typing::Type,
};

impl<'src> Token<'src> {
    pub(super) fn nud(self, state: &mut Parser<'src>, min_bp: BindingPow) -> Option<NodeBox<'src>> {
        Some(match self.kind {
            Ident => {
                if self.src == ".." {
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Placeholder))
                } else if min_bp <= 1 && state.tokenizer.next_is(|tok| tok.binding_pow() == Some(0))
                {
                    let content = state.pop_expr(1, self.span.end);
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
                    match state.try_to_make_number(self.span, self.src) {
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
                        let condition = state.pop_expr(binding_pow::STATEMENT, self.span.end);

                        let then_body = state.pop_path(4, condition.span.end);
                        let else_body = if let Some(Token { span, .. }) =
                            state.tokenizer.next_if(|x| x.kind == Keyword(Else))
                        {
                            Some(state.pop_path(4, span.end))
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
                    Proc => match state.parse_return() {
                        None => {
                            let convention_or_body = state.pop_expr(1, self.span.end);
                            match state.parse_return() {
                                None => {
                                    let convention = convention_or_body;
                                    let body = state.pop_expr(1, self.span.end);
                                    if let Some(jump) = state.parse_return() {
                                        state.make_node(NodeWrapper::new(self.span).with_node(
                                            Node::Proc {
                                                convention: Some(convention),
                                                body: Path {
                                                    node: Some(body),
                                                    jump: Some(jump),
                                                },
                                            },
                                        ))
                                    } else {
                                        state
                                            .errors
                                            .push(body.span.end(), ErrorCode::ExpectedReturn);
                                        state.make_node(NodeWrapper::new(self.span).with_node(
                                            Node::Proc {
                                                convention: Some(convention),
                                                body: Path {
                                                    node: Some(body),
                                                    jump: Some(Jump::Return { val: None }),
                                                },
                                            },
                                        ))
                                    }
                                }
                                Some(jump) => state.make_node(
                                    NodeWrapper::new(self.span).with_node(Node::Proc {
                                        convention: None,
                                        body: Path {
                                            node: Some(convention_or_body),
                                            jump: Some(jump),
                                        },
                                    }),
                                ),
                            }
                        }
                        Some(jump) => {
                            state.make_node(NodeWrapper::new(self.span).with_node(Node::Proc {
                                convention: None,
                                body: Path {
                                    node: None,
                                    jump: Some(jump),
                                },
                            }))
                        }
                    },
                    Else => {
                        state.errors.push(self.span, ErrorCode::LonelyElse);
                        state.pop_expr(min_bp, self.span.end); // consume else body - could lead to better error messages
                        state.parse_expr(min_bp)?
                    }
                    _ => {
                        state.tokenizer.buffer(self);
                        return None;
                    }
                }
            }
            Tick => {
                let (ident_span, symbol) = state.next_identifier(self.span.end);
                if let Some(tok) = state
                    .tokenizer
                    .next_if(|tok| matches!(tok.kind, RightArrow))
                {
                    let rhs = state.pop_expr(binding_pow::SINGLE_VALUE, tok.span.end);
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
                let Some(content) = state.parse_expr(0) else {
                    let end = state.handle_closed_bracket(self.span.end, own_bracket);
                    return Some(
                        state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Unit)),
                    );
                };

                let mut content = state.parse_list(content);

                let end = state.handle_closed_bracket(self.span.end, own_bracket);
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
            RightArrow => {
                let val = state.pop_expr(min_bp, self.span.end);
                state.make_node(NodeWrapper::new(self.span).with_node(Node::Ref {
                    lifetime: None,
                    val,
                }))
            }
            Plus | PlusPlus | DashDash => state.pop_expr(UnaryOp::Neg.binding_pow(), self.span.end),
            _ => match self.as_prefix() {
                Some(op) => {
                    let val = state.pop_expr(op.binding_pow(), self.span.end);
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
        self,
        lhs: NodeBox<'src>,
        state: &mut Parser<'src>,
    ) -> Result<NodeBox<'src>, NodeBox<'src>> {
        Ok(match self.kind {
            Open(bracket) if matches!(bracket, Bracket::Round | Bracket::Squared) => {
                state.brackets += 1;
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    _ => unreachable!(),
                };
                let content = state.parse_expr(0).unwrap_or_else(|| {
                    state.make_node(NodeWrapper::new(self.span.end() + 1).with_node(Node::Unit))
                });

                let content = state.parse_list(content);
                let end = state.handle_closed_bracket(self.span.end, bracket);
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
            ),
            Colon => {
                let rhs = state.pop_expr(binding_pow::COLON, self.span.end);
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
                )
            }

            _ => {
                if let Some(op) = self.as_postfix() {
                    state.make_node(
                        NodeWrapper::new(lhs.span - self.span)
                            .with_node(Node::Unary { op, val: lhs }),
                    )
                } else if let Some(op) = self.as_infix() {
                    let rhs = state.pop_expr(op.binding_pow(), self.span.end);
                    if op.is_chained() {
                        let mut chain = comp::Vec::new([(op, rhs)]);
                        while let Some(op) = state.tokenizer.peek().and_then(|op| op.as_infix()) {
                            if op.is_chained() {
                                let Token { span, .. } = state.tokenizer.next().unwrap();
                                let rhs = state.pop_expr(op.binding_pow(), span.end);
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
