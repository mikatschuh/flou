use crate::{
    comp,
    error::ErrorCode,
    parser::{
        binary_op::BinaryOp,
        keyword::Keyword,
        tokenizing::{
            resolve_escape_sequences,
            token::{Token, TokenKind::*},
        },
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
                if self.src.starts_with('.') {
                    if let ".." = self.src {
                        state
                            .tree
                            .add(W::new(self.span).with_node(Node::Placeholder))
                    } else {
                        state.tree.add(W::new(self.span).with_node(Node::Field(
                            state.internalizer.get(self.src.strip_prefix('.').unwrap()),
                        )))
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
                        let then_body = state.parse_path(self.span.end + 1, 4, flags.in_brackets);
                        let else_body = if state
                            .tokenizer
                            .peek()
                            .is_some_and(|x| x.kind == Keyword(Else))
                        {
                            let Token { span, .. } = state.tokenizer.next().unwrap();
                            Some(state.parse_path(span.end + 1, 4, flags.in_brackets))
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
                let Some((ident_span, symbol)) = state.pop_identifier(self.span.end + 1) else {
                    return state.parse_expr(min_bp, flags);
                };
                state
                    .tree
                    .add(W::new(self.span.start - ident_span).with_node(Node::Lifetime(symbol)))
            }
            Open(own_bracket) => {
                if let Some(content) =
                    state.parse_expr(0, flags.in_brackets_if(own_bracket != Bracket::Curly))
                {
                    let end =
                        state.handle_closed_bracket(self.span.end + 1, self.span, own_bracket);
                    state.tree[content].span_mut().end = end.end;
                    content
                } else {
                    let end =
                        state.handle_closed_bracket(self.span.end + 1, self.span, own_bracket);
                    state
                        .tree
                        .add(W::new(self.span.start - end).with_node(Node::Unit))
                }
            }
            Closed(..) => return None,
            kind => match kind.as_prefix() {
                Some(op) => {
                    let operand = state.pop_expr(self.span.end + 1, op.bp(), flags);
                    state
                        .tree
                        .add(W::new(self.span).with_node(Node::Unary { op, val: operand }))
                }
                _ => {
                    state.tokenizer.buffer(self);
                    state.tree.add(W::new(self.span.start.into()))
                }
            },
        })
    }

    pub(super) fn led<'caller, W: NodeWrapping<'src> + 'src>(
        self,
        lhs: NodeId<'src>,
        state: &mut Parser<'src, W>,
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
                let rhs = state.parse_expr(0, flags.in_brackets()).unwrap_or_else(|| {
                    state
                        .tree
                        .add(W::new(self.span - (self.span + 1)).with_node(Node::Unit))
                });
                let end = state.handle_closed_bracket(self.span.end + 1, self.span, bracket);
                state.tree[rhs].span_mut().end = end.end;
                state.tree.add(
                    W::new(state.tree[lhs].span() - end).with_node(Node::Binary {
                        op,
                        left: lhs,
                        right: rhs,
                    }),
                )
            }
            Comma => {
                let bp = if flags.in_brackets {
                    1
                } else {
                    self.right_bp()
                };

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
                let rhs = state.pop_expr(self.span.end + 1, self.right_bp(), flags);
                state.tree.add(
                    W::new(state.tree[lhs].span() - state.tree[rhs].span()).with_node(
                        Node::Binding {
                            left: lhs,
                            right: rhs,
                        },
                    ),
                )
            }
            Colon => {
                let rhs = state.pop_expr(self.span.end + 1, self.right_bp(), flags);
                let mut chain = comp::Vec::new([lhs, rhs]);
                while state
                    .tokenizer
                    .peek()
                    .is_some_and(|token| token.kind == Colon)
                {
                    let Token { span, .. } = state.tokenizer.next().unwrap();
                    let rhs = state.pop_expr(span.end + 1, self.right_bp(), flags);
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
                let rhs = state.pop_expr(self.span.end + 1, self.right_bp(), flags);
                state.tree.add(
                    W::new(state.tree[lhs].span() - state.tree[rhs].span()).with_node(
                        Node::Binding {
                            left: lhs,
                            right: rhs,
                        },
                    ),
                )
            }
            _ => {
                if let Some(op) = self.kind.as_infix() {
                    let rhs = state.pop_expr(self.span.end + 1, self.right_bp(), flags);
                    if op.is_chained() {
                        let mut chain = comp::Vec::new([(op, rhs)]);
                        while let Some(op) =
                            state.tokenizer.peek().and_then(|op| op.kind.as_infix())
                        {
                            if op.is_chained() {
                                let Token { span, .. } = state.tokenizer.next().unwrap();
                                let rhs = state.pop_expr(span.end + 1, self.right_bp(), flags);
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
                            W::new(state.tree[lhs].span() - state.tree[rhs].span()).with_node(
                                Node::Binary {
                                    op,
                                    left: lhs,
                                    right: rhs,
                                },
                            ),
                        )
                    }
                } else if let Some(op) = self.kind.as_postfix() {
                    state.tree.add(
                        W::new(state.tree[lhs].span() - self.span)
                            .with_node(Node::Unary { op, val: lhs }),
                    )
                } else {
                    state.tokenizer.buffer(self);

                    let Some(rhs) = state.parse_expr(self.right_bp(), flags) else {
                        return lhs;
                    };
                    let mut chain = comp::Vec::new([lhs, rhs]);
                    while let Some(rhs) = state.parse_expr(self.right_bp(), flags) {
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
