use std::{
    fmt::{Debug, Display},
    path::Path,
};

use crate::{
    error::{Error, ErrorCode, Errors, Position},
    parser::tokenizing::{
        into_op::split_operator, keyword::Keyword, unary_op::UnaryOp, EscapeSequenceConfusion,
    },
    tree::{
        Node::{self},
        NodeWrapper, NodeWrapping, Note, Tree,
    },
    Formatter,
};
use colored::{ColoredString, Colorize};
use tokenizing::{
    binary_op::BinaryOp::{self},
    chained_op::ChainedOp,
    num::value_to_node,
    unary_op::{
        PostfixUnaryOp,
        PrefixUnaryOp::{self},
    },
    Tokenizer,
};
pub mod tokenizing;
mod tree_navigation;
use tree_navigation::*;
pub fn parse(text: &str, path: &'static Path) -> (String, Errors) {
    let mut errors = Errors::empty();
    let mut parser = PrattParser::<NodeWrapper>::new(Formatter { enabled: true });
    let mut push_token =
        |errors: &mut Errors, pos: Position, token: Token| parser.push(errors, pos, token);
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

pub trait Parser<T> {
    fn new(formatter: Formatter) -> Self;
    fn formatter(&self) -> Formatter;
    fn push(&mut self, errors: &mut Errors, pos: Position, token: Token);
    fn tree(self) -> T;
}
pub struct PrattParser<W: NodeWrapping> {
    pub formatter: Formatter,
    pub tree: Tree<W>,
    parse_stack: ParseStack,
}
impl<Wrapper: NodeWrapping> Parser<Tree<Wrapper>> for PrattParser<Wrapper> {
    fn new(formatter: Formatter) -> Self {
        let mut tree = Tree::<Wrapper>::new();
        let root = tree.add_scope();
        Self {
            parse_stack: ParseStack::new(Pointer::Scope(root)),
            formatter,
            tree,
        }
    }
    fn formatter(&self) -> Formatter {
        self.formatter
    }
    fn tree(self) -> Tree<Wrapper> {
        self.tree
    }
    fn push(&mut self, errors: &mut Errors, pos: Position, token: Token) {
        self.formatter.input(&token);
        use Token::*;
        match token {
            UnknownOp(mut op) => {
                split_operator(
                    errors,
                    &mut op,
                    pos,
                    &mut |errors: &mut Errors, pos: Position, token: Token| {
                        self.push(errors, pos, token)
                    },
                );
                return;
            }
            PreUnary(op) => {
                let op: UnaryOp = op.into(); // PreUnary -> UnaryOp
                if let UnaryOp::Neg | UnaryOp::Pos = op {
                    if let Some(node) = self.current_node_id() {
                        if let Some(..) = self.tree[node].node() {
                            self.push(
                                errors,
                                pos,
                                Binary(if op == UnaryOp::Pos {
                                    BinaryOp::Add
                                } else {
                                    BinaryOp::Sub
                                }),
                            );
                            return; // a -/+
                                    //   ¯¯¯
                        }
                    }
                }
                let operand = self.tree.add(Wrapper::new(pos.only_end() + 1));
                let unary_op = Wrapper::new(pos).with_node(Node::UnaryOp { op, operand });
                self.add_val(unary_op);
                self.move_down(operand)
            }
            PostUnary(op) => {
                let op: UnaryOp = op.into();
                if let Some(..) = self.current_node() {
                    while let Some(higher) = self.higher_node() {
                        match higher {
                            Node::UnaryOp { op, .. }
                                if *op != UnaryOp::Decrement && *op != UnaryOp::Increment =>
                            {
                                self.move_up()
                            }
                            _ => {
                                let operand = self.current_node_id().unwrap();
                                // This unwrap is not a problem, as,
                                // 1. As we only moved up into UnaryOp s.
                                // 2. If we actually didn't move up at all the condition that
                                //    let Some(..) = self.current_node()
                                //    would still hold up
                                self.tree[operand] = Wrapper::new(pos).with_node(Node::UnaryOp {
                                    op,
                                    operand: self.tree.add(self.tree[operand].clone()),
                                });
                                break;
                            }
                        }
                    }
                } else {
                    let unary_op = match op {
                        UnaryOp::Increment => PrefixUnaryOp::Pos,
                        _ => PrefixUnaryOp::Neg,
                    };
                    (0..2).for_each(|_| self.push(errors, pos, PreUnary(unary_op)));
                    return; // { ++/--
                            //   ¯¯¯¯¯
                }
            }
            Binary(op) => {
                self.go_to_binding_pow(op.binding_pow());
                let left = self.current().unwrap(); // todo!
                let right = self.tree.add(Wrapper::new(pos.only_end() + 1));
                self.tree[left] = Wrapper::new(pos).with_node(Node::BinaryOp {
                    op,
                    left: self.tree.add(self.tree[left].clone()),
                    right,
                });
                self.move_down(right);
            }
            ChainedOp(op) => todo!(),

            Quote { quote, confusions } => {
                let quote = Wrapper::new(pos).with_node(Node::Quote(quote)).add_notes(
                    confusions
                        .into_iter()
                        .map(|confusion| Note::EscapeSequenceConfusion(confusion))
                        .collect(),
                );
                self.add_val(quote)
            }
            Val(val) => {
                if let Some(keyword) = Keyword::from_str(&val) {
                    todo!()
                } else {
                    let val = value_to_node(val, pos, &mut self.tree);
                    self.add_val(val)
                }
            }
            OpenBracket { squared } => {
                let content = self.tree.add(Wrapper::new(pos.only_end() + 1));
                let brackets = Wrapper::new(pos).with_node(Node::Brackets { squared, content });
                match self.current() {
                    Pointer::Node(node) => match self.tree[node].node() {
                        Some(_) => {
                            self.tree[node] = Wrapper::new(pos).with_node(Node::BinaryOp {
                                op: BinaryOp::Apply,
                                left: self.tree.add(self.tree[node].clone()),
                                right: content,
                            });
                        }
                        None => self.tree[node] = brackets,
                    },
                    Pointer::Scope(scope) => {
                        let id = self.tree.add(brackets);
                        self.tree[scope].push(id);
                        self.move_down(id);
                    }
                }
                self.move_down(content)
            }
            ClosedBracket {
                squared: own_squared,
            } => self.handle_closed_bracket(errors, pos, if own_squared { "]" } else { ")" }),
            OpenCurly => {
                let scope = self.tree.add_scope();
                self.add_val(Wrapper::new(pos).with_node(Node::Scope(scope)));
                self.move_into_new_scope(scope);
            }
            ClosedCurly => self.handle_closed_bracket(errors, pos, "}"),

            EoF => {}
        }
    }
}
impl<W: NodeWrapping> PrattParser<W> {
    fn add_val(&mut self, val: W) {
        loop {
            match self.current() {
                Pointer::Node(node) => match self.tree[node].node() {
                    Some(_) => self.move_up(),
                    None => {
                        self.tree[node] = val;
                        return;
                    }
                },
                Pointer::Scope(scope) => {
                    let id = self.tree.add(val);
                    self.tree[scope].push(id);
                    self.move_down(id);
                    return;
                }
            }
        }
    }
    fn go_to_binding_pow(&mut self, binding_pow: f32) {
        while let Some(higher) = self.higher_node() {
            match higher {
                Node::BinaryOp { op: kind, .. } if kind.binding_pow() >= binding_pow => {
                    self.move_up()
                }
                Node::UnaryOp { .. } => self.move_up(),
                _ => return,
            }
        }
    }
    fn handle_closed_bracket(&mut self, errors: &mut Errors, pos: Position, bracket: &str) {
        while let Some(higher) = self.higher() {
            match higher {
                Pointer::Node(node) => {
                    match node.get(&self.tree).unwrap() // unwrapping is ok since this is the higher layer
                {
                    Node::Brackets { squared, .. }
                        if *squared && bracket == "]" || !squared && bracket == ")" =>
                    {
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::Brackets { squared, .. } => {
                        errors.push(Error::new(
                            pos,
                            ErrorCode::WrongClosedBracket {
                                expected: if *squared { "]" } else { ")" }.to_owned(),
                                found: bracket.to_owned(),
                            },
                        ));
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::Scope(..) if bracket == "}" => {
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::Scope(..) => {
                        errors.push(Error::new(
                            pos,
                            ErrorCode::WrongClosedBracket {
                                expected: "}".to_owned(),
                                found: bracket.to_owned(),
                            },
                        ));
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    _ => self.move_up(),
                }
                }
                Pointer::Scope(..) => self.move_up(),
            }
        }
        errors.push(Error::new(
            pos,
            ErrorCode::NoOpenedBracket {
                closed: bracket.to_owned(),
            },
        ));
    }
}
