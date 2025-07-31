use num::BigUint;

use crate::{
    error::Position,
    parser::tokenizing::{
        binary_op::BinaryOp, chained_op::ChainedOp, num::NumberParsingNote, unary_op::UnaryOp,
        with_written_out_escape_sequences, EscapeSequenceConfusion,
    },
    typing::Type,
};
// use colored::Colorize;
use std::{
    fmt::{Debug, Display},
    ops::{Index, IndexMut},
    vec,
};

pub trait NodeWrapping: Clone + Debug {
    fn new(pos: Position) -> Self;
    fn add_note(self, comment: Note) -> Self;
    fn add_notes(self, comments: Vec<Note>) -> Self;
    fn with_node(self, node: Node) -> Self;
    fn with_type(self, typed: Type) -> Self;
    fn node_mut(&mut self) -> &mut Option<Node>;
    fn node(&self) -> Option<&Node>;
    fn typed(&self) -> Option<Type>;
    fn pos(&self) -> Position;
    fn pos_mut(&mut self) -> &mut Position;

    fn display(&self, tree: &Tree<Self>, indentation: String) -> String;
}

#[derive(Debug, Clone)]
pub struct NonNullNodeWrapper {
    node: Node,
    pos: Position,
}
trait Unwrapable: Clone + Debug {
    fn unwrap(&self) -> &Node;
}
impl Unwrapable for NonNullNodeWrapper {
    fn unwrap(&self) -> &Node {
        &self.node
    }
}
const DISPLAY_INDENTATION: &str = "|   ";
#[derive(Debug, Clone, PartialEq)]
pub struct NodeWrapper {
    pos: Position,
    node: Option<Node>,
    typed: Option<Type>,
    notes: Vec<Note>,
}

impl NodeWrapping for NodeWrapper {
    fn new(pos: Position) -> Self {
        Self {
            pos,
            node: None,
            notes: vec![],
            typed: None,
        }
    }
    #[inline]
    fn add_note(mut self, comment: Note) -> Self {
        self.notes.push(comment);
        self
    }
    #[inline]
    fn add_notes(mut self, mut comments: Vec<Note>) -> Self {
        self.notes.append(&mut comments);
        self
    }
    #[inline]
    fn with_node(mut self, node: Node) -> Self {
        self.node = Some(node);
        self
    }
    #[inline]
    fn with_type(mut self, typed: Type) -> Self {
        self.typed = Some(typed);
        self
    }
    #[inline]
    fn node_mut(&mut self) -> &mut Option<Node> {
        &mut self.node
    }
    #[inline]
    fn node(&self) -> Option<&Node> {
        self.node.as_ref()
    }
    #[inline]
    fn typed(&self) -> Option<Type> {
        self.typed
    }
    #[inline]
    fn pos(&self) -> Position {
        self.pos
    }
    #[inline]
    fn pos_mut(&mut self) -> &mut Position {
        &mut self.pos
    }
    fn display(&self, tree: &Tree<Self>, indentation: String) -> String {
        let Some(node) = self.node() else {
            return format!("None");
        };
        let next_indentation = indentation.clone() + DISPLAY_INDENTATION;
        use Node::*;
        format!(
            "{}",
            match node {
                Scope(scope) => {
                    tree[*scope].display(tree, indentation)
                }
                Literal {
                    val,
                    imaginary_coefficient,
                } => format!("{val} {}", if *imaginary_coefficient { "i" } else { "()" }),
                Id(id) => format!("Id  {}", id),
                Quote(quote) => format!("Quote  \"{}\"", with_written_out_escape_sequences(&quote)),
                BinaryOp { op, left, right } => format!(
                    "{op} {{\n{}{}\n{}{} \n{}}}",
                    next_indentation.clone(),
                    left.get_wrapper(tree)
                        .display(tree, next_indentation.clone()),
                    next_indentation.clone(),
                    right.get_wrapper(tree).display(tree, next_indentation),
                    indentation
                ),
                UnaryOp { op, operand } => format!(
                    "{op} {}",
                    operand.get_wrapper(tree).display(
                        tree,
                        indentation
                            + &op
                                .to_string()
                                .chars()
                                .fold(String::new(), |acc, _| acc + " ")
                            + " "
                    )
                ),
                Brackets { squared, content } if !squared => format!(
                    "( {} )",
                    content.get_wrapper(tree).display(tree, next_indentation)
                ),
                Brackets { squared, content } if *squared => format!(
                    "[ {} ]",
                    content.get_wrapper(tree).display(tree, next_indentation)
                ),
                List(list) =>
                    if list.len() < 2 {
                        format!(
                            "[{}]",
                            list.iter()
                                .map(|item| {
                                    format!(
                                        "{}",
                                        item.get_wrapper(tree)
                                            .display(tree, next_indentation.clone())
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
                                        item.get_wrapper(tree)
                                            .display(tree, next_indentation.clone())
                                    )
                                })
                                .collect::<String>(),
                            indentation
                        )
                    },
                _ => todo!(),
            }
        )
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Note {
    NumberParsingNote(NumberParsingNote),
    EscapeSequenceConfusion(EscapeSequenceConfusion),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub content: Vec<NodeId>,
}
impl Scope {
    pub fn new() -> Self {
        Self { content: vec![] }
    }
    pub fn push(&mut self, node: NodeId) {
        self.content.push(node)
    }
    pub fn display(&self, tree: &Tree<impl NodeWrapping>, indentation: String) -> String {
        let next_indentation = indentation.clone() + DISPLAY_INDENTATION;
        let mut string = String::new();
        if self.content.len() == 0 {
            string += &format!("\n{}", next_indentation,)
        } else {
            for node in self.into_iter().map(|node| &tree[*node]) {
                string += &format!(
                    "\n{}{}",
                    next_indentation,
                    &node.display(&tree, next_indentation.clone())
                )
            }
        }
        format!("{{{string}\n{indentation}}}")
    }
}
impl<'a> IntoIterator for &'a Scope {
    type Item = &'a NodeId;
    type IntoIter = std::slice::Iter<'a, NodeId>;

    fn into_iter(self) -> Self::IntoIter {
        self.content.iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Scope(ScopeId), // { ... }
    // control flow structures
    Conditional {
        condition: NodeId,
        looping: bool,
        then_body: NodeId,
        else_body: Option<NodeId>,
    }, // if/loop condition then_body (else else_body)
    // single values
    Literal {
        val: BigUint,
        imaginary_coefficient: bool,
    }, // (0(b/s/o/d/x))N(.N)((u/i/f/c)(0(b/s/o/d/x))N)(i)
    Quote(String), // "..."

    // identifiers
    Id(String),          // x
    PrimitiveType(Type), // u32, i32, c32, f32, ...

    // multiple values
    List(Vec<NodeId>),        // a, b, c, d, ...
    ColonStruct(Vec<NodeId>), // a : b : c : d

    // operations
    BinaryOp {
        op: BinaryOp,
        left: NodeId,
        right: NodeId,
    }, // left op right
    ChainedOp {
        first: NodeId,
        additions: Vec<(ChainedOp, NodeId)>,
    }, // first op additions[0].0 additions[0].1
    UnaryOp {
        op: UnaryOp,
        operand: NodeId,
    }, // op operand
    Brackets {
        squared: bool,
        content: NodeId,
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
pub struct NodeId(usize);

impl NodeId {
    #[inline]
    pub fn get(self, tree: &Tree<impl NodeWrapping>) -> Option<&Node> {
        tree[self].node()
    }
    #[inline]
    pub fn get_mut(self, tree: &mut Tree<impl NodeWrapping>) -> &mut Option<Node> {
        tree[self].node_mut()
    }
    #[inline]
    pub fn get_wrapper<Wrapper: NodeWrapping>(self, tree: &Tree<Wrapper>) -> &Wrapper {
        &tree[self]
    }
    #[inline]
    pub fn get_wrapper_mut<Wrapper: NodeWrapping>(self, tree: &mut Tree<Wrapper>) -> &mut Wrapper {
        &mut tree[self]
    }
    #[inline]
    pub fn as_non_null(self, tree: &Tree<impl NodeWrapping>) -> NonNullNodeId {
        assert!(if let Some(..) = self.get(tree) {
            true
        } else {
            false
        });
        NonNullNodeId(self)
    }
}
impl From<NonNullNodeId> for NodeId {
    #[inline]
    fn from(value: NonNullNodeId) -> Self {
        value.0
    }
}
/// A node id with the certainty of having a actual node in place
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NonNullNodeId(NodeId);

impl NonNullNodeId {
    #[inline]
    pub fn get(self, tree: &Tree<impl NodeWrapping>) -> &Node {
        tree[self.0].node().unwrap()
    }
    #[inline]
    pub fn get_mut(self, tree: &mut Tree<impl NodeWrapping>) -> &mut Node {
        tree[self.0].node_mut().as_mut().unwrap()
    }
    #[inline]
    pub fn get_wrapper<Wrapper: NodeWrapping>(self, tree: &Tree<Wrapper>) -> &Wrapper {
        &tree[self.0]
    }
    #[inline]
    pub fn get_wrapper_mut<Wrapper: NodeWrapping>(self, tree: &mut Tree<Wrapper>) -> &mut Wrapper {
        &mut tree[self.0]
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ScopeId(usize);
impl ScopeId {
    pub fn get<W: NodeWrapping>(self, tree: &Tree<W>) -> &Scope {
        &tree.scopes[self.0]
    }
    pub fn get_mut<W: NodeWrapping>(self, tree: &mut Tree<W>) -> &mut Scope {
        &mut tree.scopes[self.0]
    }
}
#[derive(Clone, Debug)]
pub struct Tree<W: NodeWrapping> {
    nodes: Vec<W>,
    scopes: Vec<Scope>,
}
impl<Wrapper: NodeWrapping> Index<NodeId> for Tree<Wrapper> {
    type Output = Wrapper;
    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index.0]
    }
}
impl<W: NodeWrapping> IndexMut<NodeId> for Tree<W> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index.0]
    }
}
impl<Wrapper: NodeWrapping> Index<NonNullNodeId> for Tree<Wrapper> {
    type Output = Wrapper;
    fn index(&self, index: NonNullNodeId) -> &Self::Output {
        &self.nodes[index.0 .0]
    }
}
impl<W: NodeWrapping> IndexMut<NonNullNodeId> for Tree<W> {
    fn index_mut(&mut self, index: NonNullNodeId) -> &mut Self::Output {
        &mut self.nodes[index.0 .0]
    }
}
impl<W: NodeWrapping> Index<ScopeId> for Tree<W> {
    type Output = Scope;
    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index.0]
    }
}
impl<W: NodeWrapping> IndexMut<ScopeId> for Tree<W> {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scopes[index.0]
    }
}
impl<W: NodeWrapping> Tree<W> {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            scopes: vec![],
        }
    }
    pub fn add(&mut self, node: W) -> NodeId {
        self.nodes.push(node);
        NodeId(self.nodes.len() - 1)
    }
    pub fn add_scope(&mut self) -> ScopeId {
        self.scopes.push(Scope::new());
        ScopeId(self.scopes.len() - 1)
    }
    pub fn root(&self) -> &Scope {
        &self.scopes[0]
    }
}

impl<W: NodeWrapping> Display for Tree<W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.root().display(self, String::new()))
    }
}
