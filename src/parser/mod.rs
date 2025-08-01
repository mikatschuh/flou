use std::{
    fmt::{Debug, Display},
    path::Path,
};

use crate::{
    error::{Error, ErrorCode, Errors, Position},
    parser::{
        num::value_to_node,
        tokenizing::{
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
    },
    tree::{
        Node::{self},
        NodeId, NodeWrapper, NodeWrapping, Note, ScopeId, Tree,
    },
    unpack,
    utilities::NonEmptyVec,
    Formatter,
};
use colored::{ColoredString, Colorize};

pub mod num;

pub mod tokenizing;
mod tree_navigation;
use tree_navigation::*;

pub fn parse(text: &str, path: &'static Path) -> (String, Errors) {
    let mut errors = Errors::empty();
    let mut parser = PrattParser::<NodeWrapper>::new(Formatter { enabled: true });
    let mut push_token =
        |errors: &mut Errors, pos: Position, token: Token| process(&mut parser, errors, pos, token);
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

pub struct PrattParser<W: NodeWrapping> {
    formatter: Formatter,
    tree: Tree<W>,
    token_buffer: Vec<Token>,
    parse_stack: ParseStack,
}
impl<Wrapper: NodeWrapping> PrattParser<Wrapper> {
    fn new(formatter: Formatter) -> Self {
        let mut tree = Tree::<Wrapper>::new();
        let root = tree.add_scope();
        Self {
            parse_stack: ParseStack::new(Pointer::Scope(root)),
            formatter,
            token_buffer: vec![],
            tree,
        }
    }
    fn tree(self) -> Tree<Wrapper> {
        self.tree
    }
}
pub trait Parser<Wrapper: NodeWrapping>: TreeNavi<Wrapper> {
    fn formatter(&self) -> Formatter;
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
    fn handle_closed_bracket(&mut self, errors: &mut Errors, pos: Position, bracket: &str);
}
pub fn process<Wrapper: NodeWrapping>(
    parser: &mut impl Parser<Wrapper>,
    errors: &mut Errors,
    pos: Position,
    token: Token,
) {
    parser.formatter().input(&token);
    use Token::*;
    match token {
        UnknownOp(mut op) => {
            split_operator(
                errors,
                &mut op,
                pos,
                &mut |errors: &mut Errors, pos: Position, token: Token| {
                    process(parser, errors, pos, token)
                },
            );
            return;
        }
        PreUnary(op) => {
            let op: UnaryOp = op.into();
            if matches!(op, UnaryOp::Neg | UnaryOp::Pos) && parser.points_to_some_node() {
                process(
                    parser,
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
            let operand = parser.add(Wrapper::new(pos.only_end() + 1));
            let unary_op = Wrapper::new(pos).with_node(Node::UnaryOp { op, operand });
            parser.add_val(unary_op);
            parser.move_down(operand)
        }
        PostUnary(op) => {
            let op: UnaryOp = op.into();
            if parser.points_to_some_node() {
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
                (0..2).for_each(|_| process(parser, errors, pos, PreUnary(unary_op)));
                return; // { ++/--
                        //   ¯¯¯¯¯
            }
        }
        Binary(op) => {
            if !parser.points_to_some_node() {
                errors.push(Error::new(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: op.to_string(),
                    },
                ));
                return;
            }
            if parser.points_to_some_node() {
                parser.go_to_binding_pow(op.binding_pow());
                parser.make_binary_operator(pos, |left, right| Node::BinaryOp { op, left, right });
            } else if matches!(
                parser.higher_node(),
                Some(Node::UnaryOp {
                    op: UnaryOp::Not,
                    ..
                })
            ) && matches!(op, BinaryOp::And | BinaryOp::Or)
            {
                parser.move_up(); // moving to the layer of the unary op
            }
        }
        ChainedOp(op) => {
            if !parser.points_to_some_node() {
                // { op }
                //  ¯¯¯
                errors.push(Error::new(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: op.to_string(),
                    },
                ));
                return;
            }
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
            let quote = Wrapper::new(pos).with_node(Node::Quote(quote)).add_notes(
                confusions
                    .into_iter()
                    .map(|confusion| Note::EscapeSequenceConfusion(confusion))
                    .collect(),
            );
            parser.add_val(quote)
        }
        Val(val) => {
            if let Some(keyword) = Keyword::from_str(&val) {
                todo!()
            } else {
                let val = parser.value_to_node(val, pos);
                parser.add_val(val)
            }
        }
        OpenBracket { squared } => {
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
        } => parser.handle_closed_bracket(errors, pos, if own_squared { "]" } else { ")" }),
        OpenCurly => {
            let scope = parser.add_scope();
            parser.add_val(Wrapper::new(pos).with_node(Node::Scope(scope)));
            parser.move_into_new_scope(scope);
        }
        ClosedCurly => parser.handle_closed_bracket(errors, pos, "}"),
        Comma => {
            if !parser.points_to_some_node() {
                // { , }
                //  ¯¯
                errors.push(Error::new(
                    pos,
                    ErrorCode::ExpectedValue {
                        found: ",".to_owned(),
                    },
                ));
                return;
            }
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
        EoF => {}
    }
}
impl<Wrapper: NodeWrapping> Parser<Wrapper> for PrattParser<Wrapper> {
    fn formatter(&self) -> Formatter {
        self.formatter
    }
    #[inline]
    fn add(&mut self, val: Wrapper) -> NodeId {
        self.tree.add(val)
    }
    #[inline]
    fn set(&mut self, id: NodeId, val: Wrapper) {
        self.tree[id] = val
    }
    #[inline]
    fn get(&self, id: NodeId) -> &Wrapper {
        &self.tree[id]
    }
    #[inline]
    fn push(&mut self, scope: ScopeId, val: Wrapper) -> NodeId {
        let id = self.add(val);
        self.tree[scope].push(id);
        id
    }
    #[inline]
    fn reallocate(&mut self, id: NodeId) -> NodeId {
        self.tree.move_to_new_location(id)
    }
    #[inline]
    fn add_scope(&mut self) -> ScopeId {
        self.tree.add_scope()
    }
    #[inline]
    fn value_to_node(&mut self, string: String, pos: Position) -> Wrapper {
        value_to_node(string, pos, &mut self.tree)
    }
    fn add_val(&mut self, val: Wrapper) {
        loop {
            match self.current() {
                Pointer::Node(node) => match self.tree[node].node() {
                    Some(_) => self.move_up(),
                    None => {
                        if let Some(higher) = self.higher_node_id() {
                            higher
                                .get_wrapper_mut(&mut self.tree)
                                .pos_mut()
                                .set_end(val.pos().end_line, val.pos().end_char);
                        }
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
    fn make_binary_operator(
        &mut self,
        pos: Position,
        binary_operator: impl Fn(NodeId, NodeId) -> Node,
    ) {
        let left = self.current_node_id().unwrap();
        let right = self.tree.add(Wrapper::new(pos.only_end() + 1));
        self.tree[left] = Wrapper::new(left.get_wrapper(&self.tree).pos() | pos + 1)
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
