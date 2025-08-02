use std::{
    fmt::{Debug, Display},
    path::Path,
    vec::IntoIter,
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
    pop_as_long_as,
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
use ::num::Integer;
use tree_navigation::*;

pub fn parse(text: &str, path: &'static Path) -> (String, Rc<Errors>) {
    let mut errors = Rc::new(Errors::empty());
    let mut parser = PrattParser::<NodeWrapper>::new(Formatter { enabled: true }, errors.clone());
    let mut push_token = |pos: Position, token: Token| process(&mut parser, (pos, token));
    let mut tokenizer = Tokenizer::new(path);
    text.chars()
        .for_each(|c| tokenizer.process_char(&mut errors, c, &mut push_token));
    tokenizer.end_of_file(&mut errors, &mut push_token);
    (parser.tree().to_string(), errors)
}
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    UnknownOp(String),
    PreUnary(PrefixUnaryOp),
    BufferedNot,
    PostUnary(PostfixUnaryOp),
    Binary(BinaryOp),
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
                PreUnary(op) => format!("{}", op).bold(),
                BufferedNot => "! [buffered]".bold(),
                PostUnary(op) => format!("{}", op).bold(),
                Binary(op) => format!("{}", op).bold(),
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
    token_buffer: Vec<(Position, Token)>,
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
            token_buffer: vec![],
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
    fn buffer(&mut self) -> &mut Vec<(Position, Token)>;
    fn empty_buffer(&mut self) -> IntoIter<(Position, Token)>;
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
    parser: &mut impl Parser<Wrapper>,
    (pos, token): (Position, Token),
) {
    parser.formatter().input(&token);
    use Token::*;
    match token {
        UnknownOp(op) => {
            split_operator(parser.errors(), op, pos)
                .for_each(|(pos, token)| process(parser, (pos, token)));
            return;
        }
        PreUnary(op) => {
            let op: UnaryOp = op.into();
            if matches!(op, UnaryOp::Neg | UnaryOp::Pos) && parser.points_to_some_node() {
                process(
                    parser,
                    (
                        pos,
                        Binary(if op == UnaryOp::Pos {
                            BinaryOp::Add
                        } else {
                            BinaryOp::Sub
                        }),
                    ),
                );
                return; // a -/+
                        //   ¯¯¯
            } else if let UnaryOp::Not = op {
                parser.buffer().push((pos, BufferedNot));
                return;
            }
            parser.empty_buffer().for_each(|item| process(parser, item));

            let operand = parser.add(Wrapper::new(pos.only_end() + 1));
            let unary_op = Wrapper::new(pos).with_node(Node::UnaryOp { op, operand });
            parser.add_val(unary_op);
            parser.move_down(operand)
        }
        BufferedNot => {
            let operand = parser.add(Wrapper::new(pos.only_end() + 1));
            let unary_op = Wrapper::new(pos).with_node(Node::UnaryOp {
                op: UnaryOp::Not,
                operand,
            });
            parser.add_val(unary_op);
            parser.move_down(operand)
        }
        PostUnary(op) => {
            let op: UnaryOp = op.into();
            if parser.points_to_some_node() {
                parser.empty_buffer().for_each(|item| process(parser, item));

                parser.go_to_binding_pow(op.binding_pow());
                let operand = parser.current_node_id().unwrap();
                let operand = parser.reallocate(operand);
                parser.set(
                    operand,
                    Wrapper::new(pos).with_node(Node::UnaryOp { op, operand }),
                );
            } else {
                let unary_op = match op {
                    UnaryOp::Increment => PrefixUnaryOp::Pos,
                    _ => PrefixUnaryOp::Neg,
                };
                (0..2).for_each(|_| parser.buffer().push((pos, PreUnary(unary_op))));
                return; // { ++/--
                        //   ¯¯¯¯¯
            }
        }
        Binary(op) => {
            if !parser.points_to_some_node() {
                parser.errors().push(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: op.to_string(),
                    },
                );
                return;
            }
            if matches!(
                op,
                BinaryOp::And | BinaryOp::Or | BinaryOp::Nand | BinaryOp::Nor
            ) {
                let mut not_pos = pos;
                let mut i = 0;
                pop_as_long_as!(parser.buffer() => (pos, BufferedNot) =>{ not_pos = pos; i += 1 });
                if i > 0 {
                    parser.buffer().push((
                        not_pos | pos,
                        Binary(match op {
                            BinaryOp::And if i.is_odd() => BinaryOp::Nand,
                            BinaryOp::Or if i.is_odd() => BinaryOp::Nor,
                            BinaryOp::Nand if i.is_odd() => BinaryOp::And,
                            BinaryOp::Nor if i.is_odd() => BinaryOp::Or,
                            _ => op,
                        }),
                    ));
                    return;
                }
            }
            if matches!(
                op,
                BinaryOp::And | BinaryOp::Or | BinaryOp::Nand | BinaryOp::Nor
            ) {
                let mut not_pos = pos;
                let mut i = 0;
                pop_as_long_as!(parser.buffer() => (pos, BufferedNot) =>{ not_pos = pos; i += 1 });
                if i > 0 {
                    parser.buffer().push((
                        not_pos | pos,
                        Binary(match op {
                            BinaryOp::And if i.is_odd() => BinaryOp::Nand,
                            BinaryOp::Or if i.is_odd() => BinaryOp::Nor,
                            BinaryOp::Nand if i.is_odd() => BinaryOp::And,
                            BinaryOp::Nor if i.is_odd() => BinaryOp::Or,
                            _ => op,
                        }),
                    ));
                    return;
                }
            }
            if parser.points_to_some_node() {
                parser.empty_buffer().for_each(|item| process(parser, item));

                parser.go_to_binding_pow(op.binding_pow());
                parser.make_binary_operator(pos, |left, right| Node::BinaryOp { op, left, right });
            }
        }
        ChainedOp(op) => {
            if !parser.points_to_some_node() {
                // { op }
                //  ¯¯¯
                parser.errors().push(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: op.to_string(),
                    },
                );
                return;
            }
            parser.empty_buffer().for_each(|item| process(parser, item));

            parser.go_to_binding_pow(op.binding_pow()); // binding power of comma
            if let Some(Node::ChainedOp { .. }) = parser.current_node() {
                let right = parser.add(Wrapper::new(pos.only_end() + 1));
                unpack!(parser.current_node_mut() => Some(Node::ChainedOp{additions, ..}) => additions.push((op, right)));
                parser.move_down(right)
            } else {
                parser.make_binary_operator(pos, |left, right| Node::ChainedOp {
                    first: left,
                    additions: NonEmptyVec::new((op, right)),
                });
            }
        }
        Quote { quote, confusions } => {
            parser.empty_buffer().for_each(|item| process(parser, item));

            let quote = Wrapper::new(pos).with_node(Node::Quote(quote)).add_notes(
                confusions
                    .into_iter()
                    .map(|confusion| Note::EscapeSequenceConfusion(confusion))
                    .collect(),
            );
            parser.add_val(quote)
        }
        Val(val) => {
            parser.empty_buffer().for_each(|item| process(parser, item));

            if let Some(keyword) = Keyword::from_str(&val) {
                todo!()
            } else {
                let val = parser.value_to_node(val, pos);
                parser.add_val(val)
            }
        }
        OpenBracket { squared } => {
            parser.empty_buffer().for_each(|item| process(parser, item));

            let content = parser.add(Wrapper::new(pos.only_end() + 1));
            let brackets = Wrapper::new(pos).with_node(Node::Brackets { squared, content });

            match parser.current() {
                Pointer::Node(node) => match parser.get(node).node() {
                    Some(..) => {
                        parser.go_to_binding_pow(10.0); // application binding power
                        let node = parser.current().unwrap();
                        let left = parser.reallocate(node);
                        parser.set(
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
                    None => parser.set(node, brackets),
                },
                Pointer::Scope(scope) => {
                    let id = parser.push(scope, brackets);
                    parser.move_down(id);
                }
            }
            parser.move_down(content)
        }
        ClosedBracket {
            squared: own_squared,
        } => {
            parser.empty_buffer().for_each(|item| process(parser, item));

            parser.handle_closed_bracket(pos, if own_squared { "]" } else { ")" })
        }
        OpenCurly => {
            parser.empty_buffer().for_each(|item| process(parser, item));

            let scope = parser.add_scope();
            parser.add_val(Wrapper::new(pos).with_node(Node::Scope(scope)));
            parser.move_into_new_scope(scope);
        }
        ClosedCurly => parser.handle_closed_bracket(pos, "}"),
        Comma => {
            if !parser.points_to_some_node() {
                // { , }
                //  ¯¯
                parser.errors().push(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: ",".to_owned(),
                    },
                );
                return;
            }
            parser.empty_buffer().for_each(|item| process(parser, item));

            parser.go_to_binding_pow(2.0); // binding power of comma
            if let Some(Node::List(..)) = parser.current_node() {
                let right = parser.add(Wrapper::new(pos.only_end() + 1));
                unpack!(parser.current_node_mut() => Some(Node::List(list)) => list.push(right));
                parser.move_down(right)
            } else {
                parser.make_binary_operator(pos, |left, right| Node::List(vec![left, right]));
            }
        }
        Colon => {
            parser.empty_buffer().for_each(|item| process(parser, item));

            parser.go_to_binding_pow(-1.0); // impossibly low binding power, making it exit everything
            if let Some(Node::ColonStruct(..)) = parser.current_node() {
                let right = parser.add(Wrapper::new(pos.only_end() + 1));
                unpack!(parser.current_node_mut() => Some(Node::ColonStruct(list)) => list.push(right));
                parser.move_down(right)
            } else if parser.points_to_some_node() {
                parser
                    .make_binary_operator(pos, |left, right| Node::ColonStruct(vec![left, right]));
            }
        }
        EoF => {
            parser.empty_buffer().for_each(|item| process(parser, item));
        }
    }
}
