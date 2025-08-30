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
        unary_op::UnaryOp,
        InBrackets,
        Location::{self, *},
        NodeWrapper, Parser, StackData,
    },
    tree::{Bracket, EveryJump, Jump, Node, NodeBox, Note},
};

impl<'src> Token<'src> {
    pub(super) fn nud<'caller, J: Jump<'src>>(
        self,
        state: &mut Parser<'src>,
        min_bp: u8,
        flags: StackData<'src, 'caller, J>,
    ) -> Option<NodeBox<'src, J>> {
        Some(match self.kind {
            Ident => {
                if let Some(src) = self.src.strip_prefix('.') {
                    if let "." = src {
                        state.make_node(NodeWrapper::new(self.span).with_node(Node::Placeholder))
                    } else if !src.is_empty() {
                        state.make_node(
                            NodeWrapper::new(self.span)
                                .with_node(Node::Field(state.internalizer.get(src))),
                        )
                    } else {
                        state.errors.push(self.span, ErrorCode::IdentWithJustDot);
                        state.make_node(NodeWrapper::new(self.span))
                    }
                } else {
                    state.convert_to_num_if_possible(self.span, self.src, flags)
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
                            state.pop_expr(self.span.end + 1, 4, flags.outside_of_brackets());
                        let then_body =
                            state.pop_path(condition.span.end + 1, 4, flags.in_brackets);
                        let else_body = if let Some(Token { span, .. }) =
                            state.tokenizer.next_if(|x| x.kind == Keyword(Else))
                        {
                            Some(state.pop_path(span.end + 1, 4, flags.in_brackets))
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
                        state.parse_expr(min_bp, flags)?
                    }
                    Continue => match flags.loc {
                        Path(jump) => {
                            let layers = 1 + state // +1 because we already saw continue ones
                                .tokenizer
                                .consume_while(|token| token.kind == Keyword(Continue))
                                .count();

                            jump.write(Some(Box::new(EveryJump::Continue { layers })));
                            state.clean_up_after_jump();
                            return None;
                        }
                        FuncArg => {
                            state.errors.push(
                                self.span,
                                ErrorCode::JumpInsideFuncArg { keyword: self.src },
                            );
                            state.parse_expr(min_bp, flags)?
                        }
                    },
                    Break => match flags.loc {
                        Path(jump) => {
                            let layers = 1 + state
                                .tokenizer
                                .consume_while(|token| token.kind == Keyword(Exit))
                                .count();
                            jump.write(Some(Box::new(EveryJump::Break {
                                layers,
                                val: Box::new(state.parse_path(
                                    self.span.end + 1,
                                    4,
                                    flags.in_brackets,
                                )),
                            })));
                            state.clean_up_after_jump();
                            return None;
                        }
                        FuncArg => {
                            state.errors.push(
                                self.span,
                                ErrorCode::JumpInsideFuncArg { keyword: self.src },
                            );
                            state.parse_expr(min_bp, flags)?
                        }
                    },
                    Return => match flags.loc {
                        Path(jump) => {
                            jump.write(Some(Box::new(EveryJump::Return {
                                val: Box::new(state.parse_path(
                                    self.span.end + 1,
                                    binding_pow::STATEMENT,
                                    flags.in_brackets,
                                )),
                            })));
                            state.clean_up_after_jump();
                            return None;
                        }
                        FuncArg => {
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
                if let Some(mut content) =
                    state.parse_expr(0, flags.in_brackets(InBrackets::Container))
                {
                    state.parse_list(&mut content, flags);
                    let end = state.handle_closed_bracket(
                        self.span.end + 1,
                        self.span,
                        own_bracket,
                        flags,
                    );
                    content.span.end = end.end;
                    content
                } else {
                    let end = state.handle_closed_bracket(
                        self.span.end + 1,
                        self.span,
                        own_bracket,
                        flags,
                    );
                    state.make_node(NodeWrapper::new(self.span.start - end).with_node(Node::Unit))
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
                    let lhs = state.make_node(NodeWrapper::new(self.span));
                    state.dash_slot = Some(lhs);
                    lhs
                } else if count.get().is_odd() {
                    let op = UnaryOp::Neg;
                    let operand = state.pop_expr(self.span.end + 1, op.binding_pow(), flags);
                    state.make_node(
                        NodeWrapper::new(self.span).with_node(Node::Unary { op, val: operand }),
                    )
                } else {
                    state.pop_expr(self.span.end + 1, UnaryOp::Neg.binding_pow(), flags)
                }
            }
            RightArrow => {
                let val = state.pop_expr(self.span.end + 1, min_bp, flags);
                state.make_node(NodeWrapper::new(self.span).with_node(Node::Ref {
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
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Unary { op, val }))
                }
                _ => {
                    state.tokenizer.buffer(self);
                    return None;
                }
            },
        })
    }

    pub(super) fn led<'caller, J: Jump<'src>>(
        mut self,
        lhs: NodeBox<'src, J>,
        state: &'caller mut Parser<'src>,
        flags: StackData<'src, 'caller, J>,
    ) -> NodeBox<'src, J> {
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
                let flags = flags.in_brackets(InBrackets::Application);

                let Some(mut content) = state.parse_expr(binding_pow::STATEMENT, flags) else {
                    let content = state
                        .make_node(NodeWrapper::new(self.span.end() + 1).with_node(Node::Unit));
                    let end =
                        state.handle_closed_bracket(self.span.end + 1, self.span, bracket, flags);
                    return state.make_node(NodeWrapper::new(lhs.span - end).with_node(
                        Node::Binary {
                            op,
                            lhs,
                            rhs: content,
                        },
                    ));
                };

                if let Some(arg_type) =
                    state.parse_expr(binding_pow::STATEMENT, flags.location(Location::FuncArg))
                {
                    let (func_name, generics) = if let Some(Node::Binary {
                        op: BinaryOp::Index,
                        lhs,
                        rhs,
                    }) = lhs.node
                    {
                        if let Some(Node::Ident(name)) = lhs.node {
                            name
                        } else {
                            state.errors.push(lhs.span, ErrorCode::ExpectedFunctionName);
                            state.internalizer.empty()
                        };
                        todo!()
                    } else if let Some(Node::Ident(name)) = lhs.node {
                        (name, Generics::new())
                    } else {
                        state.errors.push(lhs.span, ErrorCode::ExpectedFunctionName);
                        (state.internalizer.empty(), Generics::new())
                    };

                    let mut args = vec![(content, arg_type)];
                    while let Some(Token { span, .. }) =
                        state.tokenizer.next_if(|tok| tok.kind == Comma)
                    {
                        let arg = state.pop_expr(span.end + 1, binding_pow::STATEMENT, flags);
                        let arg_type =
                            state.pop_expr(arg.span.end + 1, binding_pow::STATEMENT, flags);

                        args.push((arg, arg_type));
                    }
                    let end =
                        state.handle_closed_bracket(self.span.end + 1, self.span, bracket, flags);

                    let return_type = state.pop_expr(end.end + 1, binding_pow::STATEMENT, flags);
                    let body = if state
                        .tokenizer
                        .next_is(|tok| tok.kind == Closed(Bracket::Curly))
                    {
                        Some(state.parse_path(
                            return_type.span.end + 1,
                            binding_pow::STATEMENT,
                            flags.in_brackets,
                        ))
                    } else {
                        None
                    };
                    let end = body
                        .as_ref()
                        .map_or_else(|| return_type.span, |body| body.content.span)
                        .end;
                    let None = state.items.insert(
                        func_name,
                        Function {
                            is_indexing: op == BinaryOp::Index,
                            generics,
                            args,
                            return_type,
                            body,
                        },
                    ) else {
                        todo!()
                    };
                    state.make_node(NodeWrapper::new(lhs.span - end))
                } else {
                    state.parse_list(&mut content, flags);
                    let end =
                        state.handle_closed_bracket(self.span.end + 1, self.span, bracket, flags);
                    state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Binary {
                        op: BinaryOp::App,
                        lhs,
                        rhs: content,
                    }))
                }
            }
            Equal => {
                let rhs = state.pop_expr(self.span.end + 1, binding_pow::BINDING, flags);
                state.make_node(
                    NodeWrapper::new(lhs.span - rhs.span).with_node(Node::Binding { lhs, rhs }),
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
                state.make_node(
                    NodeWrapper::new(lhs.span - chain.last().span)
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
                state.make_node(
                    NodeWrapper::new(lhs.span - state.arena[rhs].span)
                        .with_node(Node::Binding { lhs, rhs }),
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
                let rhs = state.pop_expr(self.span.end + 1, op.binding_pow(), flags);
                state.make_node(
                    NodeWrapper::new(lhs.span - state.arena[rhs].span).with_node(Node::Binary {
                        op,
                        lhs,
                        rhs,
                    }),
                )
            }
            _ => {
                if let Some(op) = self.kind.as_postfix() {
                    state.make_node(
                        NodeWrapper::new(lhs.span - self.span)
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
                    state.tokenizer.buffer(self);

                    let Some(rhs) = state.parse_expr(binding_pow::STATEMENT, flags) else {
                        return lhs;
                    };
                    let mut chain = comp::Vec::new([lhs, rhs]);
                    while let Some(rhs) = state.parse_expr(binding_pow::STATEMENT, flags) {
                        chain.push(rhs)
                    }
                    state.make_node(
                        NodeWrapper::new(lhs.span - chain.last().span)
                            .with_node(Node::Statements(chain)),
                    )
                }
            }
        }
    }
}
