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
        Bracket,
        Node::{self},
        NodeId, NodeWrapper, NodeWrapping, Note, Tree,
    },
    unpack,
    utilities::Rc,
    Formatter,
};
use colored::{ColoredString, Colorize};

mod debug;
pub mod item;
mod lib;
pub mod num;
pub mod tokenizing;
mod tree_navigation;
use tree_navigation::*;

pub fn parse(text: &str, path: &'static Path) -> (String, Rc<Errors>) {
    let mut errors = Rc::new(Errors::empty());
    let mut parser =
        PrattParser::<NodeWrapper>::new(Formatter { enabled: true }, errors.clone(), path);
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
    let tree = parser.tree();
    //println!("{:?}", tree);
    (tree.to_string(), errors)
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
        kind: Bracket,
    },
    ClosedBracket {
        kind: Bracket,
    },

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

                OpenBracket { kind } => kind.display_open().bold(),
                ClosedBracket { kind } => kind.display_closed().bold(),

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
    fn new(formatter: Formatter, errors: Rc<Errors>, path: &'static Path) -> Self {
        let mut tree = Tree::<Wrapper>::new();
        let root = tree.add_root(Wrapper::new(Position::new(path)));
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
pub trait Getting<Wrapper: NodeWrapping> {
    fn formatter(&self) -> Formatter;
    fn errors(&mut self) -> &mut Errors;
}
pub trait Parser<Wrapper: NodeWrapping>: TreeNavi<Wrapper> + Getting<Wrapper> {
    fn add(&mut self, val: Wrapper) -> NodeId;
    fn set(&mut self, id: NodeId, val: Wrapper);
    fn reallocate(&mut self, id: NodeId) -> NodeId;
    fn value_to_node(&mut self, string: String, pos: Position) -> Wrapper;
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
    fn make_binary_operator(
        &mut self,
        pos: Position,
        binary_operator: impl Fn(NodeId, NodeId) -> Node,
    );
    fn handle_closed_bracket(&mut self, pos: Position, bracket: Bracket);
}
pub fn process<Wrapper: NodeWrapping, State: Parser<Wrapper> + Display>(
    state: &mut State,
    (pos, token): (Position, Token),
    token_stream: &mut Vec<(Position, Token)>,
) {
    state.formatter().input(&token);
    use Token::*;
    match token {
        UnknownOp(op) => {
            split_operator(state.errors(), op, pos)
                .rev()
                .for_each(|(pos, token)| token_stream.push((pos, token)));
            return;
        }
        Prefix(op) => {
            let op: UnaryOp = op.into();
            if matches!(op, UnaryOp::Neg | UnaryOp::Pos) && state.points_to_some_node() {
                token_stream.push((
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
                while let Some((next_pos, next)) = token_stream.pop() {
                    match next {
                        Prefix(PrefixUnaryOp::Not) => {
                            invertion = !invertion;
                            continue;
                        }
                        Infix(BinaryOp::Or(inverted)) => token_stream
                            .push((pos | next_pos, Infix(BinaryOp::Or(invertion != inverted)))),
                        Infix(BinaryOp::Xor(inverted)) => token_stream
                            .push((pos | next_pos, Infix(BinaryOp::Xor(invertion != inverted)))),
                        Infix(BinaryOp::And(inverted)) => token_stream
                            .push((pos | next_pos, Infix(BinaryOp::And(invertion != inverted)))),
                        Infix(BinaryOp::BitOr(inverted)) => token_stream.push((
                            pos | next_pos,
                            Infix(BinaryOp::BitOr(invertion != inverted)),
                        )),
                        Infix(BinaryOp::BitXor(inverted)) => token_stream.push((
                            pos | next_pos,
                            Infix(BinaryOp::BitXor(invertion != inverted)),
                        )),
                        Infix(BinaryOp::BitAnd(inverted)) => token_stream.push((
                            pos | next_pos,
                            Infix(BinaryOp::BitAnd(invertion != inverted)),
                        )),
                        Infix(BinaryOp::OrAssign(inverted)) => token_stream.push((
                            pos | next_pos,
                            Infix(BinaryOp::OrAssign(invertion != inverted)),
                        )),
                        Infix(BinaryOp::XorAssign(inverted)) => token_stream.push((
                            pos | next_pos,
                            Infix(BinaryOp::XorAssign(invertion != inverted)),
                        )),
                        Infix(BinaryOp::AndAssign(inverted)) => token_stream.push((
                            pos | next_pos,
                            Infix(BinaryOp::AndAssign(invertion != inverted)),
                        )),
                        _ => {
                            token_stream.push((next_pos, next));
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
                (0..2).for_each(|_| token_stream.push((pos, Prefix(unary_op))));
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
        Val(val) => {
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
                state.errors().push(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: ",".to_owned(),
                    },
                );
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
