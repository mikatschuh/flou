use std::{
    fmt::{Debug, Display},
    path::Path,
};

use crate::{
    error::{Error, ErrorCode, Errors, Position},
    parser::tokenizing::{
        binary_op::BindingPow, into_op::split_operator, keyword::Keyword, unary_op::UnaryOp,
        EscapeSequenceConfusion,
    },
    tree::{
        Node::{self},
        NodeId, NodeWrapper, NodeWrapping, Note, Tree,
    },
    unpack,
    utilities::NonEmptyVec,
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
                let op: UnaryOp = op.into();
                if matches!(op, UnaryOp::Neg | UnaryOp::Pos) && self.points_to_some_node() {
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
                let operand = self.tree.add(Wrapper::new(pos.only_end() + 1));
                let unary_op = Wrapper::new(pos).with_node(Node::UnaryOp { op, operand });
                self.add_val(unary_op);
                self.move_down(operand)
            }
            PostUnary(op) => {
                let op: UnaryOp = op.into();
                if self.points_to_some_node() {
                    self.go_to_binding_pow(op.binding_pow());
                    let operand = self.current_node_id().unwrap();
                    self.tree[operand] = Wrapper::new(pos).with_node(Node::UnaryOp {
                        op,
                        operand: self.tree.add(self.tree[operand].clone()),
                    });
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
                if self.points_to_some_node() {
                    self.make_binary_operator(pos, |left, right| Node::BinaryOp {
                        op,
                        left,
                        right,
                    });
                } else {
                    errors.push(Error::new(
                        pos,
                        ErrorCode::ExpectedValue {
                            found: op.to_string(),
                        },
                    ))
                }
            }
            ChainedOp(op) => {
                let binding_pow = op.binding_pow();
                self.go_to_binding_pow(binding_pow); // binding power of comma
                if let Some(Node::ChainedOp { .. }) = self.current_node() {
                    let right = self.tree.add(Wrapper::new(pos.only_end() + 1));
                    unpack!(self.current_node_mut() => Some(Node::ChainedOp{additions, ..}) => additions.push((op, right)));
                    self.move_down(right)
                } else if self.points_to_some_node() {
                    self.make_binary_operator(pos, |left, right| Node::ChainedOp {
                        first: left,
                        additions: NonEmptyVec::new((op, right)),
                    });
                } else {
                    // { , }
                    //  ¯¯
                    errors.push(Error::new(
                        pos,
                        ErrorCode::ExpectedValue {
                            found: ",".to_owned(),
                        },
                    ))
                }
            }
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
                        Some(..) => {
                            self.go_to_binding_pow(10.0); // application binding power
                            let node = self.current().unwrap();
                            self.tree[node] = Wrapper::new(pos).with_node(Node::BinaryOp {
                                op: if squared {
                                    BinaryOp::Index
                                } else {
                                    BinaryOp::App
                                },
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
            Comma => {
                self.go_to_binding_pow(2.0); // binding power of comma
                if let Some(Node::List(..)) = self.current_node() {
                    let right = self.tree.add(Wrapper::new(pos.only_end() + 1));
                    unpack!(self.current_node_mut() => Some(Node::List(list)) => list.push(right));
                    self.move_down(right)
                } else if self.points_to_some_node() {
                    self.make_binary_operator(pos, |left, right| Node::List(vec![left, right]));
                } else {
                    // { , }
                    //  ¯¯
                    errors.push(Error::new(
                        pos,
                        ErrorCode::ExpectedValue {
                            found: ",".to_owned(),
                        },
                    ))
                }
            }
            Colon => {
                self.go_to_binding_pow(-1.0); // impossibly low binding power, making it exit everything
                if let Some(Node::ColonStruct(..)) = self.current_node() {
                    let right = self.tree.add(Wrapper::new(pos.only_end() + 1));
                    unpack!(self.current_node_mut() => Some(Node::ColonStruct(list)) => list.push(right));
                    self.move_down(right)
                } else if self.points_to_some_node() {
                    self.make_binary_operator(pos, |left, right| {
                        Node::ColonStruct(vec![left, right])
                    });
                }
            }
            EoF => {}
        }
    }
}
impl<Wrapper: NodeWrapping> PrattParser<Wrapper> {
    fn add_val(&mut self, val: Wrapper) {
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
    /// Moves up until its at the right height. It will only ever move into nodes.
    fn go_to_binding_pow(&mut self, binding_pow: f32) {
        while let Some(higher) = self.higher_node() {
            match higher {
                Node::BinaryOp { op, .. } if op.binding_pow() >= binding_pow => self.move_up(),
                Node::UnaryOp { op, .. } if op.binding_pow() >= binding_pow && !op.is_postfix() => {
                    self.move_up()
                }
                Node::List(..) if 2.0 >= binding_pow => self.move_up(),
                Node::ColonStruct(..) if -1.0 >= binding_pow => self.move_up(),
                Node::ChainedOp { additions, .. }
                    if additions.last().0.binding_pow() >= binding_pow =>
                {
                    self.move_up()
                }
                _ => return,
            }
        }
    }
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
    ) {
        let left = self.current_node_id().unwrap();
        let right = self.tree.add(Wrapper::new(pos.only_end() + 1));
        self.tree[left] = Wrapper::new(pos)
            .with_node(binary_operator(self.tree.move_to_new_location(left), right));
        self.move_down(right);
    }
    fn handle_closed_bracket(&mut self, errors: &mut Errors, pos: Position, bracket: &str) {
        while let Some(higher) = self.higher() {
            match higher {
                Pointer::Node(node) => {
                    match node.get(&self.tree).unwrap() // unwrapping is ok since this is the higher layer
                {
                    Node::BinaryOp { op, .. }
                    if (*op == BinaryOp::App && bracket == ")" || *op == BinaryOp::Index && bracket == "]")=> {
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::BinaryOp { op, .. } if matches!(op, BinaryOp::App | BinaryOp::Index) => {
                        errors.push(Error::new(
                            pos,
                            ErrorCode::WrongClosedBracket {
                                expected: if *op == BinaryOp::App { ")" } else { "]" }.to_owned(),
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
