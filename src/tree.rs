use num::BigUint;

use crate::{
    comp,
    error::Span,
    parser::{
        binary_op::BinaryOp,
        chained_op::ChainedOp,
        intern::{Internalizer, Symbol},
        tokenizing::{with_written_out_escape_sequences, EscapeSequenceConfusion},
        unary_op::UnaryOp,
    },
    typing::Type,
};
// use colored::Colorize;
use std::{
    fmt::{Debug, Display},
    marker::PhantomData,
    ops::{Index, IndexMut},
    vec,
};

pub trait NodeWrapping<'src>: Clone + Debug {
    fn new(span: Span, node: Node<'src>) -> Self;
    fn add_note(self, comment: Note) -> Self;
    fn add_notes(self, comments: Vec<Note>) -> Self;
    fn with_type(self, typed: Type<'src>) -> Self;
    fn node_mut(&mut self) -> &mut Node<'src>;
    fn node(&self) -> &Node<'src>;
    fn typed(&self) -> Option<Type>;
    fn span(&self) -> Span;
    fn span_mut(&mut self) -> &mut Span;

    fn display(
        &self,
        tree: &Tree<'src, Self>,
        internalizer: &Internalizer<'src>,
        indentation: String,
    ) -> String;
}

const DISPLAY_INDENTATION: &str = "|   ";
const DISPLAY_INDENTATION_NEG_1: &str = "   ";
#[derive(Debug, Clone, PartialEq)]
pub struct NodeWrapper<'src> {
    span: Span,
    node: Node<'src>,
    typed: Option<Type<'src>>,
    notes: Vec<Note>,
}

impl<'src> NodeWrapping<'src> for NodeWrapper<'src> {
    fn new(span: Span, node: Node<'src>) -> Self {
        Self {
            span: span,
            node,
            notes: vec![],
            typed: None,
        }
    }
    #[inline]
    fn add_note(mut self, comment: Note) -> Self {
        self.notes.push(comment);
        self
    }
    fn add_notes(mut self, mut comments: Vec<Note>) -> Self {
        self.notes.append(&mut comments);
        self
    }
    #[inline]
    fn with_type(mut self, typed: Type<'src>) -> Self {
        self.typed = Some(typed);
        self
    }
    #[inline]
    fn node_mut(&mut self) -> &mut Node<'src> {
        &mut self.node
    }
    #[inline]
    fn node(&self) -> &Node<'src> {
        &self.node
    }
    #[inline]
    fn typed(&self) -> Option<Type> {
        self.typed
    }
    #[inline]
    fn span(&self) -> Span {
        self.span
    }
    #[inline]
    fn span_mut(&mut self) -> &mut Span {
        &mut self.span
    }
    fn display(
        &self,
        tree: &Tree<'src, Self>,
        internalizer: &Internalizer,
        indentation: String,
    ) -> String {
        let next_indentation = indentation.clone() + DISPLAY_INDENTATION;
        use Node::*;
        format!(
            "{}",
            match &self.node {
                Literal {
                    val,
                    imaginary_coefficient,
                } => format!("{val} {}", if *imaginary_coefficient { "i" } else { "()" }),
                Ident(id) => format!("Id  {}", internalizer.resolve(*id)),
                Quote(quote) => format!("Quote  \"{}\"", with_written_out_escape_sequences(&quote)),
                BinaryOp { op, left, right } => format!(
                    "{op} {{\n{}{}\n{}{} \n{}}}",
                    next_indentation.clone(),
                    left.get_wrapper(tree)
                        .display(tree, internalizer, next_indentation.clone()),
                    next_indentation.clone(),
                    right
                        .get_wrapper(tree)
                        .display(tree, internalizer, next_indentation),
                    indentation
                ),
                UnaryOp { op, operand } => format!(
                    "{op} {}",
                    operand.get_wrapper(tree).display(
                        tree,
                        internalizer,
                        indentation
                            + &op
                                .to_string()
                                .chars()
                                .fold(String::new(), |acc, _| acc + " ")
                            + " "
                    )
                ),
                Brackets {
                    kind: Bracket::Round,
                    content,
                } => format!(
                    "({}{}{})",
                    DISPLAY_INDENTATION_NEG_1,
                    content
                        .get_wrapper(tree)
                        .display(tree, internalizer, next_indentation),
                    DISPLAY_INDENTATION_NEG_1,
                ),
                Brackets {
                    kind: Bracket::Squared,
                    content,
                } => format!(
                    "[{}{}{}]",
                    DISPLAY_INDENTATION_NEG_1,
                    content
                        .get_wrapper(tree)
                        .display(tree, internalizer, next_indentation),
                    DISPLAY_INDENTATION_NEG_1,
                ),
                Brackets {
                    kind: Bracket::Curly,
                    content,
                } => format!(
                    "{{{}{}{}}}",
                    DISPLAY_INDENTATION_NEG_1,
                    content
                        .get_wrapper(tree)
                        .display(tree, internalizer, next_indentation),
                    DISPLAY_INDENTATION_NEG_1
                ),
                List(list) =>
                    if list.len() < 2 {
                        format!(
                            "[{}]",
                            list.iter()
                                .map(|item| {
                                    format!(
                                        "{}",
                                        item.get_wrapper(tree).display(
                                            tree,
                                            internalizer,
                                            next_indentation.clone()
                                        )
                                    )
                                })
                                .collect::<String>()
                        )
                    } else {
                        format!(
                            "[\n{}{}]",
                            list.iter()
                                .map(|item| {
                                    format!(
                                        "{}{},\n",
                                        next_indentation.clone(),
                                        item.get_wrapper(tree).display(
                                            tree,
                                            internalizer,
                                            next_indentation.clone()
                                        )
                                    )
                                })
                                .collect::<String>(),
                            indentation
                        )
                    },
                ColonStruct(content) =>
                    if content.len() == 1 {
                        format!(
                            "{}",
                            content[0].get_wrapper(tree).display(
                                tree,
                                internalizer,
                                next_indentation.clone()
                            )
                        )
                    } else {
                        let mut list = content.iter();
                        format!(
                            "{}{}",
                            list.next().unwrap().get_wrapper(tree).display(
                                tree,
                                internalizer,
                                indentation.clone() + "  "
                            ),
                            list.map(|item| {
                                format!(
                                    "\n{}: {}",
                                    indentation.clone(),
                                    item.get_wrapper(tree).display(
                                        tree,
                                        internalizer,
                                        indentation.clone() + "  "
                                    )
                                )
                            })
                            .collect::<String>(),
                        )
                    },
                Statements(content) =>
                    if content.len() == 1 {
                        format!(
                            "{}",
                            content[0].get_wrapper(tree).display(
                                tree,
                                internalizer,
                                next_indentation.clone()
                            )
                        )
                    } else {
                        let mut string = String::new();
                        if content.len() == 0 {
                            string += &format!("\n{}", next_indentation,)
                        } else {
                            content
                                .into_iter()
                                .map(|node| &tree[*node])
                                .for_each(|node| {
                                    string += &format!(
                                        "\n{}{}",
                                        next_indentation,
                                        &node.display(
                                            &tree,
                                            internalizer,
                                            next_indentation.clone()
                                        )
                                    )
                                });
                        }
                        format!("_{string}\n{indentation}¯")
                    },
                ChainedOp { first, additions } => format!(
                    "Chained  {}{}",
                    first.get_wrapper(tree).display(
                        tree,
                        internalizer,
                        indentation.clone() + "   "
                    ),
                    additions
                        .iter()
                        .map(|item| {
                            format!(
                                "\n{}{}{}{}",
                                indentation.clone(),
                                item.0,
                                (0..3 - item.0.to_string().chars().count())
                                    .map(|_| " ")
                                    .collect::<String>(),
                                item.1.get_wrapper(tree).display(
                                    tree,
                                    internalizer,
                                    indentation.clone() + "   "
                                )
                            )
                        })
                        .collect::<String>(),
                ),
                _ => todo!(),
            }
        )
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Note {
    EscapeSequenceConfusion(EscapeSequenceConfusion),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path<'src> {
    pub content: NodeId<'src>,
}
impl<'src> Path<'src> {
    pub fn new(content: NodeId<'src>) -> Self {
        Self { content }
    }
    pub fn display<'tree>(
        &self,
        tree: &'src Tree<'src, impl NodeWrapping<'src>>,
        internalizer: &Internalizer<'src>,
        indentation: String,
    ) -> String {
        format!(
            "{}",
            self.content
                .get_wrapper(tree)
                .display(tree, internalizer, indentation)
        )
    }
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Bracket {
    Round,
    Squared,
    Curly,
}
impl Bracket {
    pub fn display_open(self) -> &'static str {
        match self {
            Bracket::Round => "(",
            Bracket::Squared => "[",
            Bracket::Curly => "{",
        }
    }
    pub fn display_closed(self) -> &'static str {
        match self {
            Bracket::Round => ")",
            Bracket::Squared => "]",
            Bracket::Curly => "}",
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Node<'src> {
    // control flow structures
    Conditional {
        condition: NodeId<'src>,
        looping: bool,
        then_body: Path<'src>,
        else_body: Option<Path<'src>>,
    }, // if/loop condition then_body (else else_body)
    // single values
    Literal {
        val: BigUint,
        imaginary_coefficient: bool,
    }, // (0(b/s/o/d/x))N(.N)((u/i/f/c)(0(b/s/o/d/x))N)(i)
    Quote(String), // "..."

    // identifiers
    Ident(Symbol<'src>),       // x
    PrimitiveType(Type<'src>), // u32, i32, c32, f32, ...

    // multiple values
    List(comp::Vec<NodeId<'src>, 2>),        // a, b, c, d, ...
    ColonStruct(comp::Vec<NodeId<'src>, 2>), // a : b : c : d
    Statements(comp::Vec<NodeId<'src>, 2>),

    // operations
    BinaryOp {
        op: BinaryOp,
        left: NodeId<'src>,
        right: NodeId<'src>,
    }, // left op right
    ChainedOp {
        first: NodeId<'src>,
        additions: comp::Vec<(ChainedOp, NodeId<'src>), 1>,
    }, // first op additions[0].0 additions[0].1
    UnaryOp {
        op: UnaryOp,
        operand: NodeId<'src>,
    }, // op operand
    Brackets {
        kind: Bracket,
        content: NodeId<'src>,
    }, // squared content squared
}
/*
impl Node {
    fn as_code<W: Clone + std::fmt::Debug + Unwrapable>(
        &self,
        node_buffer: &NodeBuffer<W>,
        indentation: String,
    ) -> String {
        let next_indentation = indentation.clone() + "  ";
        use Node::*;
        match self {
            Num(num) => format!("Num({:?})", num),
            Id(name) => format!("Id(String::from(\"{}\"))", name),
            Quote(quote) => format!("Quote(String::from(\"{}\"))", quote),

            Sum(content) => format!("Sum({})", to_rust_code(content, indentation)),
            Com(content) => format!("Com({})", to_rust_code(content, indentation)),
            List(content) => {
                format!("List({})", to_rust_code(content, indentation))
            }
            UnaryOp { kind, operand } => {
                format!(
                    "UnaryOp{{kind: {:?}, operand: Box::new({})}}",
                    kind,
                    operand.unwrap().as_code(indentation)
                )
            }
            ChainedOp { first, additions } => {
                format!(
                    "{}{{\n{}{}: {}::new({}),\n{}{}: vec![{}]\n{}}}",
                    "ChainedOp".truecolor(239, 229, 182),
                    next_indentation.clone(),
                    "first".truecolor(164, 189, 255),
                    "Box".truecolor(239, 229, 182),
                    first.unwrap().as_code(next_indentation.clone()),
                    next_indentation.clone(),
                    "additions".truecolor(164, 189, 255),
                    additions
                        .iter()
                        .map(|n| format!(
                            "({:?}, {})",
                            n.0,
                            n.1.unwrap().as_code(next_indentation.clone())
                        ))
                        .collect::<Vec<String>>()
                        .join(", "),
                    indentation
                )
            }
            Scope(..) => todo!(),
            Conditional { .. } => todo!(),

            BinaryOp { kind, left, right } => format!(
                "{} {{\n{}{}: {}, \n{}{}: {}::new({}), \n{}{}: {}::new({})\n{}}}",
                "BinaryOp".truecolor(239, 229, 182),
                next_indentation.clone(),
                "kind".truecolor(164, 189, 255),
                format!("{:?}", kind).truecolor(239, 229, 182),
                next_indentation.clone(),
                "left".truecolor(164, 189, 255),
                "Box".truecolor(239, 229, 182),
                left.unwrap().as_code(next_indentation.clone()),
                next_indentation.clone(),
                "right".truecolor(164, 189, 255),
                "Box".truecolor(239, 229, 182),
                right.unwrap().as_code(next_indentation.clone()),
                indentation
            ),
        }
    }
}
pub fn to_rust_code<W: Clone + Debug + Unwrapable>(v: &Vec<W>, indentation: String) -> String {
    format!(
        "vec![{}]",
        v.iter()
            .map(|n| n.unwrap().as_code(indentation.clone()))
            .collect::<Vec<String>>()
            .join(", ")
    )
}*/
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeId<'tree> {
    _marker: PhantomData<&'tree ()>,
    idx: usize,
}

impl<'tree> NodeId<'tree> {
    #[inline]
    fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData::default(),
            idx,
        }
    }
    #[inline]
    pub fn get(self, tree: &'tree Tree<'tree, impl NodeWrapping<'tree>>) -> &'tree Node<'tree> {
        tree[self].node()
    }
    #[inline]
    pub fn get_mut(
        self,
        tree: &'tree mut Tree<'tree, impl NodeWrapping<'tree>>,
    ) -> &'tree mut Node<'tree> {
        tree[self].node_mut()
    }
    #[inline]
    pub fn get_wrapper<'src, Wrapper: NodeWrapping<'src>>(
        self,
        tree: &'tree Tree<'src, Wrapper>,
    ) -> &'tree Wrapper
    where
        'tree: 'src,
    {
        &tree[self]
    }
    #[inline]
    pub fn get_wrapper_mut<Wrapper: NodeWrapping<'tree>>(
        self,
        tree: &'tree mut Tree<'tree, Wrapper>,
    ) -> &'tree mut Wrapper {
        &mut tree[self]
    }
}

#[derive(Clone, Debug)]
pub struct Tree<'src, W: NodeWrapping<'src>> {
    _marker: PhantomData<&'src ()>,
    pub root: NodeId<'src>,
    nodes: Vec<W>,
}
impl<'src, Wrapper: NodeWrapping<'src>> Index<NodeId<'src>> for Tree<'src, Wrapper> {
    type Output = Wrapper;
    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index.idx]
    }
}
impl<'src, W: NodeWrapping<'src>> IndexMut<NodeId<'src>> for Tree<'src, W> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index.idx]
    }
}
impl<'src, W: NodeWrapping<'src>> Tree<'src, W> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData::default(),
            root: NodeId::new(0),
            nodes: vec![],
        }
    }
    pub fn add(&mut self, node: W) -> NodeId {
        self.nodes.push(node);
        NodeId::new(self.nodes.len() - 1)
    }
    pub fn add_root(&mut self, node: W) -> NodeId {
        self.nodes.push(node);
        NodeId::new(self.nodes.len() - 1)
    }
    pub fn move_to_new_location(&mut self, node: NodeId<'src>) -> NodeId {
        self.add(self[node].clone())
    }
}

impl<'src, W: NodeWrapping<'src>> Tree<'src, W> {
    pub fn to_string(&'src self, internalizer: &Internalizer<'src>) -> String {
        format!(
            "{}",
            self.root
                .get_wrapper(self)
                .display(self, internalizer, String::new())
        )
    }
}
