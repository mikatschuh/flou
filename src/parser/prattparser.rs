pub struct PrattParser<'a, Wrapper: NodeWrapping> {
    formatter: Formatter,
    tree: Tree<Wrapper>,
    errors: Rc<Errors<'a>>,
    parse_stack: ParseStack,
}
impl<'a, Wrapper: NodeWrapping> PrattParser<'a, Wrapper> {
    fn new(formatter: Formatter, errors: Rc<Errors<'a>>, path: &'static Path) -> Self {
        let mut tree = Tree::<Wrapper>::new();
        let root = tree.add_root(Wrapper::new(Span::beginning()));
        Self {
            parse_stack: ParseStack::new(root),
            formatter,
            errors,
            tree,
        }
    }
    fn tree(self) -> Tree<Wrapper> {
        self.tree
    }
}
pub trait Getting<'a, Wrapper: NodeWrapping> {
    fn formatter(&self) -> Formatter;
    fn errors(&mut self) -> &mut Errors<'a>;
}
pub trait Parser<'a, Wrapper: NodeWrapping>: TreeNavi<Wrapper> + Getting<'a, Wrapper> {
    fn add(&mut self, val: Wrapper) -> NodeId;
    fn set(&mut self, id: NodeId, val: Wrapper);
    fn reallocate(&mut self, id: NodeId) -> NodeId;
    fn value_to_node(&mut self, string: &'a str, pos: Span) -> Wrapper;
    fn add_val(&mut self, val: Wrapper);
    /// Moves up until its at the right height. It will only ever move into nodes.
    fn go_to_binding_pow(&mut self, binding_pow: i8);
    /// Makes the current node into a binary operator.
    /// Tree before:
    /// ```
    /// \
    ///  \
    ///   ` Node            <--
    /// ```
    /// Tree after:
    /// ```
    /// \
    ///  \
    ///   ` Binary
    ///     /    \
    ///  Node    New Node   <--
    /// ```
    fn make_binary_operator(&mut self, pos: Span, binary_operator: impl Fn(NodeId, NodeId) -> Node);
    fn handle_closed_bracket(&mut self, pos: Span, bracket: Bracket);
}
pub fn process<'a, Wrapper: NodeWrapping, State: Parser<'a, Wrapper>>(
    state: &mut State,
    token_stream: &mut Tokenizer<'a>,
) {
    let token = token_stream.next(state.errors());
    state.formatter().input(&token);
    use TokenKind::*;
    match token.kind {
        UnknownOp => {
            split_operator(state.errors(), token.src, token.span)
                .rev()
                .for_each(|(pos, token)| token_stream.buffer((pos, token)));
            return;
        }
        Prefix(op) => {
            let op: UnaryOp = op.into();
            if matches!(op, UnaryOp::Neg | UnaryOp::Pos) && state.points_to_some_node() {
                token_stream.buffer((
                    pos,
                    Infix(if op == UnaryOp::Pos {
                        BinaryOp::Add
                    } else {
                        BinaryOp::Sub
                    }),
                ));
                return; // a -/+
                        //   ¯¯¯
            } else if let UnaryOp::Not = op {
                let mut invertion = true;
                loop {
                    let (next_pos, next) = token_stream.next(state.errors());
                    match next {
                        Prefix(PrefixUnaryOp::Not) => {
                            invertion = !invertion;
                            continue;
                        }
                        Infix(BinaryOp::Or(inverted)) => token_stream
                            .buffer((pos | next_pos, Infix(BinaryOp::Or(invertion != inverted)))),
                        Infix(BinaryOp::Xor(inverted)) => token_stream
                            .buffer((pos | next_pos, Infix(BinaryOp::Xor(invertion != inverted)))),
                        Infix(BinaryOp::And(inverted)) => token_stream
                            .buffer((pos | next_pos, Infix(BinaryOp::And(invertion != inverted)))),
                        Infix(BinaryOp::BitOr(inverted)) => token_stream.buffer((
                            pos | next_pos,
                            Infix(BinaryOp::BitOr(invertion != inverted)),
                        )),
                        Infix(BinaryOp::BitXor(inverted)) => token_stream.buffer((
                            pos | next_pos,
                            Infix(BinaryOp::BitXor(invertion != inverted)),
                        )),
                        Infix(BinaryOp::BitAnd(inverted)) => token_stream.buffer((
                            pos | next_pos,
                            Infix(BinaryOp::BitAnd(invertion != inverted)),
                        )),
                        Infix(BinaryOp::OrAssign(inverted)) => token_stream.buffer((
                            pos | next_pos,
                            Infix(BinaryOp::OrAssign(invertion != inverted)),
                        )),
                        Infix(BinaryOp::XorAssign(inverted)) => token_stream.buffer((
                            pos | next_pos,
                            Infix(BinaryOp::XorAssign(invertion != inverted)),
                        )),
                        Infix(BinaryOp::AndAssign(inverted)) => token_stream.buffer((
                            pos | next_pos,
                            Infix(BinaryOp::AndAssign(invertion != inverted)),
                        )),
                        _ => {
                            token_stream.buffer((next_pos, next));
                            let operand = state.add(Wrapper::new(pos.only_end() + 1));
                            let unary_op = Wrapper::new(pos).with_node(Node::UnaryOp {
                                op: UnaryOp::Not,
                                operand,
                            });
                            state.add_val(unary_op);
                            state.move_down(operand);
                        }
                    }
                    return;
                }
            }
            let operand = state.add(Wrapper::new(pos.only_end() + 1));
            let unary_op = Wrapper::new(pos).with_node(Node::UnaryOp { op, operand });
            state.add_val(unary_op);
            state.move_down(operand)
        }
        Postfix(op) => {
            let op: UnaryOp = op.into();
            if state.points_to_some_node() {
                state.go_to_binding_pow(op.binding_pow());
                let operand = state.current();
                let operand = state.reallocate(operand);
                state.set(
                    operand,
                    Wrapper::new(pos).with_node(Node::UnaryOp { op, operand }),
                );
            } else {
                let unary_op = match op {
                    UnaryOp::Increment => PrefixUnaryOp::Pos,
                    _ => PrefixUnaryOp::Neg,
                };
                (0..2).for_each(|_| token_stream.buffer((pos, Prefix(unary_op))));
                return; // { ++/--
                        //   ¯¯¯¯¯
            }
        }
        Infix(op) => {
            if !state.points_to_some_node() {
                state.errors().push(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: op.as_str().into(),
                    },
                );
                return;
            }
            state.go_to_binding_pow(op.binding_pow());
            state.make_binary_operator(pos, |left, right| Node::BinaryOp { op, left, right });
        }
        ChainedOp(op) => {
            if !state.points_to_some_node() {
                // { op }
                //  ¯¯¯
                state.errors().push(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: op.as_str().into(),
                    },
                );
                return;
            }
            state.go_to_binding_pow(op.binding_pow()); // binding power of comma
            if let Some(Node::ChainedOp { .. }) = state.current_node() {
                let right = state.add(Wrapper::new(pos.only_end() + 1));
                unpack!(state.current_node_mut() => Some(Node::ChainedOp{additions, ..}) => additions.push((op, right)));
                state.move_down(right)
            } else {
                state.make_binary_operator(pos, |left, right| Node::ChainedOp {
                    first: left,
                    additions: [(op, right)].into(),
                });
            }
        }
        Quote { quote, confusions } => {
            let quote = Wrapper::new(pos).with_node(Node::Quote(quote)).add_notes(
                confusions
                    .into_iter()
                    .map(|confusion| Note::EscapeSequenceConfusion(confusion))
                    .collect(),
            );
            state.add_val(quote)
        }
        Ident(val) => {
            if let Some(keyword) = Keyword::from_str(&val) {
                todo!()
            } else {
                let val = state.value_to_node(val, pos);
                state.add_val(val)
            }
        }
        OpenBracket { kind } if matches!(kind, Bracket::Round | Bracket::Squared) => {
            let content = state.add(Wrapper::new(pos.only_end() + 1));
            let brackets = Wrapper::new(pos).with_node(Node::Brackets { kind, content });
            match state.current_node() {
                Some(..) => {
                    state.go_to_binding_pow(19); // application binding power
                    let node = state.current();
                    let left = state.reallocate(node);
                    state.set(
                        node,
                        Wrapper::new(pos).with_node(Node::BinaryOp {
                            op: if kind == Bracket::Round {
                                BinaryOp::App
                            } else {
                                BinaryOp::Index
                            },
                            left,
                            right: content,
                        }),
                    );
                }
                None => state.set(state.current(), brackets),
            }
            state.move_down(content)
        }
        OpenBracket {
            kind: Bracket::Curly,
        } => {
            let content = state.add(Wrapper::new(pos.only_end() + 1));
            let brackets = Wrapper::new(pos).with_node(Node::Brackets {
                kind: Bracket::Curly,
                content,
            });
            state.add_val(brackets);
            state.move_down(content)
        }
        ClosedBracket { kind } => state.handle_closed_bracket(pos, kind),
        Comma => {
            if !state.points_to_some_node() {
                // { , }
                //  ¯¯
                state
                    .errors()
                    .push(pos, ErrorCode::ExpectedValue { found: ",".into() });
                return;
            }
            state.go_to_binding_pow(3); // binding power of comma
            if let Some(Node::List(..)) = state.current_node() {
                let right = state.add(Wrapper::new(pos.only_end() + 1));
                unpack!(state.current_node_mut() => Some(Node::List(list)) => list.push(right));
                state.move_down(right)
            } else {
                state.make_binary_operator(pos, |left, right| Node::List([left, right].into()));
            }
        }
        Colon => {
            state.go_to_binding_pow(0); // impossibly low binding power, making it exit everything
            if let Some(Node::ColonStruct(..)) = state.current_node() {
                let right = state.add(Wrapper::new(pos.only_end() + 1));
                unpack!(state.current_node_mut() => Some(Node::ColonStruct(list)) => list.push(right));
                state.move_down(right)
            } else if state.points_to_some_node() {
                state.make_binary_operator(pos, |left, right| {
                    Node::ColonStruct([left, right].into())
                });
            }
        }
        _ => {}
    }
}
