use std::{
    fmt::{Debug, Display},
    path::Path,
};

use crate::{
    error::{ErrorCode, Errors, Position},
    parser::tokenizing::{
        binary_op::{
            BinaryOp::{self},
            BindingPow,
        },
        chained_op::ChainedOp,
        into_op::split_operator,
        keyword::Keyword,
        unary_op::{
            PostfixUnaryOp,
            PrefixUnaryOp::{self},
            UnaryOp,
        },
        EscapeSequenceConfusion, Tokenizer,
    },
    tree::{
        Node::{self},
        NodeId, NodeWrapper, NodeWrapping, Note, ScopeId, Tree,
    },
    unpack,
    utilities::{NonEmptyVec, Rc},
    Formatter,
};
use colored::{ColoredString, Colorize};

mod lib;
pub mod num;

pub mod tokenizing;
mod tree_navigation;
use tree_navigation::*;

pub fn parse(text: &str, path: &'static Path) -> (String, Rc<Errors>) {
    let mut errors = Rc::new(Errors::empty());
    let mut parser = PrattParser::<NodeWrapper>::new(Formatter { enabled: true }, errors.clone());
    let mut tokens: Vec<(Position, Token)> = vec![];
    let mut push_token = |pos: Position, token: Token| tokens.push((pos, token));
    let mut tokenizer = Tokenizer::new(path);
    text.chars()
        .for_each(|c| tokenizer.process_char(&mut errors, c, &mut push_token));
    tokenizer.end_of_file(&mut errors, &mut push_token);
    tokens.reverse();
    while let Some(token) = tokens.pop() {
        process(&mut parser, token, &mut tokens);
    }
    (parser.tree().to_string(), errors)
}
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    UnknownOp(String),
    Prefix(PrefixUnaryOp),
    Postfix(PostfixUnaryOp),
    Infix(BinaryOp),
    ChainedOp(ChainedOp),

    Quote {
        quote: String,
        confusions: Vec<EscapeSequenceConfusion>,
    },
    Val(String),

    OpenBracket {
        squared: bool,
    },
    ClosedBracket {
        squared: bool,
    },
    OpenCurly,
    ClosedCurly,

    Comma,
    Colon,

    EoF,
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        write!(
            f,
            "{}",
            match self {
                UnknownOp(op) => op.bold(),
                Prefix(op) => format!("{}", op).bold(),
                Postfix(op) => format!("{}", op).bold(),
                Infix(op) => format!("{}", op).bold(),
                ChainedOp(op) => format!("{}", op).bold(),

                Quote { quote, .. } => format!("\"{}\"", quote).bold(),
                Val(val) => format!("{}", val).bold(),

                OpenBracket { squared } => match squared {
                    true => "[".bold(),
                    false => "(".bold(),
                },
                ClosedBracket { squared } => match squared {
                    true => "]".bold(),
                    false => ")".bold(),
                },
                OpenCurly => "{".bold(),
                ClosedCurly => "}".bold(),

                Comma => ",".bold(),
                Colon => ":".bold(),

                EoF => "EoF".bold(),
            }
        )
    }
}
impl Token {
    pub fn bold(&self) -> ColoredString {
        format!("{}", self).bold()
    }
}
pub struct PrattParser<Wrapper: NodeWrapping> {
    formatter: Formatter,
    tree: Tree<Wrapper>,
    errors: Rc<Errors>,
    parse_stack: ParseStack,
}
impl<Wrapper: NodeWrapping> PrattParser<Wrapper> {
    fn new(formatter: Formatter, errors: Rc<Errors>) -> Self {
        let mut tree = Tree::<Wrapper>::new();
        let root = tree.add_scope();
        Self {
            parse_stack: ParseStack::new(Pointer::Scope(root)),
            formatter,
            errors,
            tree,
        }
    }
    fn tree(self) -> Tree<Wrapper> {
        self.tree
    }
}
pub trait Getting<Wrapper: NodeWrapping> {
    fn formatter(&self) -> Formatter;
    fn errors(&mut self) -> &mut Errors;
}
pub trait Parser<Wrapper: NodeWrapping>: TreeNavi<Wrapper> + Getting<Wrapper> {
    fn add(&mut self, val: Wrapper) -> NodeId;
    fn push(&mut self, scope: ScopeId, val: Wrapper) -> NodeId;
    fn set(&mut self, id: NodeId, val: Wrapper);
    fn get(&self, id: NodeId) -> &Wrapper;
    fn reallocate(&mut self, id: NodeId) -> NodeId;
    fn add_scope(&mut self) -> ScopeId;
    fn value_to_node(&mut self, string: String, pos: Position) -> Wrapper;
    fn add_val(&mut self, val: Wrapper);
    /// Moves up until its at the right height. It will only ever move into nodes.
    fn go_to_binding_pow(&mut self, binding_pow: f32);
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
    fn make_binary_operator(
        &mut self,
        pos: Position,
        binary_operator: impl Fn(NodeId, NodeId) -> Node,
    );
    fn handle_closed_bracket(&mut self, pos: Position, bracket: &str);
}
pub fn process<Wrapper: NodeWrapping>(
    state: &mut impl Parser<Wrapper>,
    (pos, token): (Position, Token),
    token_stream: &mut Vec<(Position, Token)>,
) {
    state.formatter().input(&token);
    use Token::*;
    match token {
        UnknownOp(op) => {
            split_operator(state.errors(), op, pos)
                .for_each(|(pos, token)| token_stream.push((pos, token)));
            return;
        }
        Prefix(op) => {
            let op: UnaryOp = op.into();
            if matches!(op, UnaryOp::Neg | UnaryOp::Pos) && state.points_to_some_node() {
                process(
                    state,
                    (
                        pos,
                        Infix(if op == UnaryOp::Pos {
                            BinaryOp::Add
                        } else {
                            BinaryOp::Sub
                        }),
                    ),
                    token_stream,
                );
                return; // a -/+
                        //   ¯¯¯
            } else if let UnaryOp::Not = op {
                let mut invertion = true;
                while let Some((next_pos, next)) = token_stream.pop() {
                    match next {
                        Prefix(PrefixUnaryOp::Not) => {
                            invertion = !invertion;
                            continue;
                        }
                        Infix(BinaryOp::Or(inverted)) => process(
                            state,
                            (pos | next_pos, Infix(BinaryOp::Or(invertion != inverted))),
                            token_stream,
                        ),
                        Infix(BinaryOp::Xor(inverted)) => process(
                            state,
                            (pos | next_pos, Infix(BinaryOp::Xor(invertion != inverted))),
                            token_stream,
                        ),
                        Infix(BinaryOp::And(inverted)) => process(
                            state,
                            (pos | next_pos, Infix(BinaryOp::And(invertion != inverted))),
                            token_stream,
                        ),
                        Infix(BinaryOp::BitOr(inverted)) => process(
                            state,
                            (
                                pos | next_pos,
                                Infix(BinaryOp::BitOr(invertion != inverted)),
                            ),
                            token_stream,
                        ),
                        Infix(BinaryOp::BitXor(inverted)) => process(
                            state,
                            (
                                pos | next_pos,
                                Infix(BinaryOp::BitXor(invertion != inverted)),
                            ),
                            token_stream,
                        ),
                        Infix(BinaryOp::BitAnd(inverted)) => process(
                            state,
                            (
                                pos | next_pos,
                                Infix(BinaryOp::BitAnd(invertion != inverted)),
                            ),
                            token_stream,
                        ),
                        Infix(BinaryOp::OrAssign(inverted)) => process(
                            state,
                            (
                                pos | next_pos,
                                Infix(BinaryOp::OrAssign(invertion != inverted)),
                            ),
                            token_stream,
                        ),
                        Infix(BinaryOp::XorAssign(inverted)) => process(
                            state,
                            (
                                pos | next_pos,
                                Infix(BinaryOp::XorAssign(invertion != inverted)),
                            ),
                            token_stream,
                        ),
                        Infix(BinaryOp::AndAssign(inverted)) => process(
                            state,
                            (
                                pos | next_pos,
                                Infix(BinaryOp::AndAssign(invertion != inverted)),
                            ),
                            token_stream,
                        ),
                        _ => {
                            let operand = state.add(Wrapper::new(pos.only_end() + 1));
                            let unary_op = Wrapper::new(pos).with_node(Node::UnaryOp {
                                op: UnaryOp::Not,
                                operand,
                            });
                            state.add_val(unary_op);
                            state.move_down(operand);
                            process(state, (next_pos, next), token_stream);
                            return;
                        }
                    }
                    return;
                }
                return;
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
                let operand = state.current_node_id().unwrap();
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
                (0..2).for_each(|_| process(state, (pos, Prefix(unary_op)), token_stream));
                return; // { ++/--
                        //   ¯¯¯¯¯
            }
        }
        Infix(op) => {
            if !state.points_to_some_node() {
                state.errors().push(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: op.to_string(),
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
                        found: op.to_string(),
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
                    additions: NonEmptyVec::new((op, right)),
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
        Val(val) => {
            if let Some(keyword) = Keyword::from_str(&val) {
                todo!()
            } else {
                let val = state.value_to_node(val, pos);
                state.add_val(val)
            }
        }
        OpenBracket { squared } => {
            let content = state.add(Wrapper::new(pos.only_end() + 1));
            let brackets = Wrapper::new(pos).with_node(Node::Brackets { squared, content });

            match state.current() {
                Pointer::Node(node) => match state.get(node).node() {
                    Some(..) => {
                        state.go_to_binding_pow(10.0); // application binding power
                        let node = state.current().unwrap();
                        let left = state.reallocate(node);
                        state.set(
                            node,
                            Wrapper::new(pos).with_node(Node::BinaryOp {
                                op: if squared {
                                    BinaryOp::Index
                                } else {
                                    BinaryOp::App
                                },
                                left,
                                right: content,
                            }),
                        );
                    }
                    None => state.set(node, brackets),
                },
                Pointer::Scope(scope) => {
                    let id = state.push(scope, brackets);
                    state.move_down(id);
                }
            }
            state.move_down(content)
        }
        ClosedBracket {
            squared: own_squared,
        } => state.handle_closed_bracket(pos, if own_squared { "]" } else { ")" }),
        OpenCurly => {
            let scope = state.add_scope();
            state.add_val(Wrapper::new(pos).with_node(Node::Scope(scope)));
            state.move_into_new_scope(scope);
        }
        ClosedCurly => state.handle_closed_bracket(pos, "}"),
        Comma => {
            if !state.points_to_some_node() {
                // { , }
                //  ¯¯
                state.errors().push(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: ",".to_owned(),
                    },
                );
                return;
            }
            state.go_to_binding_pow(2.0); // binding power of comma
            if let Some(Node::List(..)) = state.current_node() {
                let right = state.add(Wrapper::new(pos.only_end() + 1));
                unpack!(state.current_node_mut() => Some(Node::List(list)) => list.push(right));
                state.move_down(right)
            } else {
                state.make_binary_operator(pos, |left, right| Node::List(vec![left, right]));
            }
        }
        Colon => {
            state.go_to_binding_pow(-1.0); // impossibly low binding power, making it exit everything
            if let Some(Node::ColonStruct(..)) = state.current_node() {
                let right = state.add(Wrapper::new(pos.only_end() + 1));
                unpack!(state.current_node_mut() => Some(Node::ColonStruct(list)) => list.push(right));
                state.move_down(right)
            } else if state.points_to_some_node() {
                state.make_binary_operator(pos, |left, right| Node::ColonStruct(vec![left, right]));
            }
        }
        EoF => {}
    }
}
