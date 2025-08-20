use num::BigUint;

use crate::{
    comp,
    error::Span,
    parser::{
        binary_op::BinaryOp,
        intern::{Internalizer, Symbol},
        tokenizing::{with_written_out_escape_sequences, EscapeSequenceConfusion},
        unary_op::UnaryOp,
    },
    typing::Type,
};
// use colored::Colorize;
use std::{
    fmt::Debug,
    marker::PhantomData,
    ops::{Index, IndexMut},
    vec,
};

pub trait NodeWrapping<'src>: Clone + Debug {
    fn new(span: Span) -> Self;
    fn with_note(self, comment: Note<'src>) -> Self;
    fn with_notes(self, comments: Vec<Note<'src>>) -> Self;
    fn with_type(self, typed: Type<'src>) -> Self;
    fn with_node(self, node: Node<'src>) -> Self;
    fn node(&self) -> Option<&Node<'src>>;
    fn node_mut(&mut self) -> &mut Option<Node<'src>>;
    fn typed(&self) -> Option<&Type>;
    fn type_mut(&mut self) -> &mut Option<Type<'src>>;
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
    node: Option<Node<'src>>,
    typed: Option<Type<'src>>,
    notes: Vec<Note<'src>>,
}

impl<'src> NodeWrapping<'src> for NodeWrapper<'src> {
    fn new(span: Span) -> Self {
        Self {
            span,
            node: None,
            notes: vec![],
            typed: None,
        }
    }
    #[inline]
    fn with_note(mut self, comment: Note<'src>) -> Self {
        self.notes.push(comment);
        self
    }
    fn with_notes(mut self, mut comments: Vec<Note<'src>>) -> Self {
        self.notes.append(&mut comments);
        self
    }
    #[inline]
    fn with_type(mut self, typed: Type<'src>) -> Self {
        self.typed = Some(typed);
        self
    }
    #[inline]
    fn with_node(mut self, node: Node<'src>) -> Self {
        self.node = Some(node);
        self
    }
    #[inline]
    fn node_mut(&mut self) -> &mut Option<Node<'src>> {
        &mut self.node
    }
    #[inline]
    fn node(&self) -> Option<&Node<'src>> {
        self.node.as_ref()
    }
    #[inline]
    fn typed(&self) -> Option<&Type> {
        self.typed.as_ref()
    }
    #[inline]
    fn type_mut(&mut self) -> &mut Option<Type<'src>> {
        &mut self.typed
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
        let Some(ref node) = self.node else {
            return "None".to_owned();
        };
        let next_indentation = indentation.clone() + DISPLAY_INDENTATION;
        use Node::*;

        match node {
            Binding { left, right } => {
                format!(
                    "= {{\n{}{}\n{}{} \n{}}}",
                    next_indentation.clone(),
                    tree[*left].display(tree, internalizer, next_indentation.clone()),
                    next_indentation.clone(),
                    tree[*right].display(tree, internalizer, next_indentation),
                    indentation
                )
            }
            Literal { val } => format!(
                "{}{}",
                val,
                self.typed.as_ref().map_or("".to_owned(), |ty| format!(
                    " - {}",
                    ty.display(tree, internalizer, indentation)
                ))
            ),
            Ident(id) => format!("Id  {}", internalizer.resolve(*id)),
            Lifetime(sym) => format!("Lifetime  {}", internalizer.resolve(*sym)),
            Field(sym) => format!("Field  {}", internalizer.resolve(*sym)),
            Placeholder => "..".to_owned(),
            Quote(quote) => format!("Quote  \"{}\"", with_written_out_escape_sequences(quote)),
            PrimitiveType(..) => todo!(),
            Unit => "()".to_owned(),
            Binary { op, left, right } => format!(
                "{op} {{\n{}{}\n{}{} \n{}}}",
                next_indentation.clone(),
                tree[*left].display(tree, internalizer, next_indentation.clone()),
                next_indentation.clone(),
                tree[*right].display(tree, internalizer, next_indentation),
                indentation
            ),
            Unary { op, val } => format!(
                "{op} {}",
                tree[*val].display(
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
                tree[*content].display(tree, internalizer, next_indentation),
                DISPLAY_INDENTATION_NEG_1,
            ),
            Brackets {
                kind: Bracket::Squared,
                content,
            } => format!(
                "[{}{}{}]",
                DISPLAY_INDENTATION_NEG_1,
                tree[*content].display(tree, internalizer, next_indentation),
                DISPLAY_INDENTATION_NEG_1,
            ),
            Brackets {
                kind: Bracket::Curly,
                content,
            } => format!(
                "{{{}{}{}}}",
                DISPLAY_INDENTATION_NEG_1,
                tree[*content].display(tree, internalizer, next_indentation),
                DISPLAY_INDENTATION_NEG_1
            ),
            List(list) => {
                if list.len() < 2 {
                    format!(
                        "[{}]",
                        list.iter()
                            .map(|item| {
                                tree[*item].display(tree, internalizer, next_indentation.clone())
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
                                    tree[*item].display(
                                        tree,
                                        internalizer,
                                        next_indentation.clone()
                                    )
                                )
                            })
                            .collect::<String>(),
                        indentation
                    )
                }
            }
            ColonStruct(content) => {
                if content.len() == 1 {
                    tree[content[0]].display(tree, internalizer, next_indentation.clone())
                } else {
                    let mut list = content.iter();
                    tree[*list.next().unwrap()].display(
                        tree,
                        internalizer,
                        indentation.clone() + "  ",
                    ) + &list
                        .map(|item| {
                            format!(
                                "\n{}: {}",
                                indentation.clone(),
                                tree[*item].display(tree, internalizer, indentation.clone() + "  ")
                            )
                        })
                        .collect::<String>()
                }
            }
            Statements(content) => {
                if content.len() == 1 {
                    tree[content[0]].display(tree, internalizer, next_indentation.clone())
                } else {
                    let mut string = String::new();
                    if content.len() == 0 {
                        string += &format!("\n{next_indentation}")
                    } else {
                        content
                            .into_iter()
                            .map(|node| &tree[*node])
                            .for_each(|node| {
                                string += &format!(
                                    "\n{}{}",
                                    next_indentation,
                                    &node.display(tree, internalizer, next_indentation.clone())
                                )
                            });
                    }
                    format!("_{string}\n{indentation}¯")
                }
            }
            Chain { first, additions } => format!(
                "Chained [\n{}{}{}\n{}]",
                next_indentation.clone(),
                tree[*first].display(tree, internalizer, next_indentation.clone() + "   "),
                additions
                    .iter()
                    .map(|item| {
                        let op = item.0.as_str();
                        format!(
                            "\n{}{}{}{}",
                            next_indentation.clone(),
                            op,
                            (0..3 - op.chars().count()).map(|_| " ").collect::<String>(),
                            tree[item.1].display(
                                tree,
                                internalizer,
                                next_indentation.clone() + "   "
                            )
                        )
                    })
                    .collect::<String>(),
                indentation
            ),
            Conditional {
                condition,
                looping,
                then_body,
                else_body,
            } => format!(
                "{} {}\n{}=> {}{}",
                if *looping { "loop" } else { "if" },
                tree[*condition].display(tree, internalizer, indentation.clone() + "   "),
                indentation.clone(),
                then_body.display(tree, internalizer, indentation.clone() + "   "),
                else_body
                    .as_ref()
                    .map_or("".to_owned(), |else_body| format!(
                        "\n{}else {}",
                        indentation.clone(),
                        &else_body.display(tree, internalizer, indentation + "     "),
                    ))
            ),
            Fields { val, fields } => format!(
                "{}{}",
                tree[*val].display(tree, internalizer, indentation.clone()),
                fields
                    .iter()
                    .map(|sym| format!(" . {}", internalizer.resolve(*sym)))
                    .collect::<String>()
            ),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Note<'src> {
    NumberParsingNote { invalid_suffix: &'src str },
    EscapeSequenceConfusion(EscapeSequenceConfusion),
}

impl<'src> From<&'src str> for Note<'src> {
    fn from(value: &'src str) -> Self {
        Self::NumberParsingNote {
            invalid_suffix: value,
        }
    }
}
impl<'src> From<EscapeSequenceConfusion> for Note<'src> {
    fn from(value: EscapeSequenceConfusion) -> Self {
        Self::EscapeSequenceConfusion(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Jump<P: Clone + Debug + PartialEq + Eq> {
    Continue { layers: usize },
    Exit { layers: usize, val: Path<P> },
    Return { val: Path<P> },
}

impl<'src> Jump<NodeId<'src>> {
    fn as_heap<W: NodeWrapping<'src>>(
        &'src self,
        tree: &'src Tree<'src, W>,
    ) -> Jump<Box<HeapNode<'src>>> {
        use Jump::*;
        match self {
            Continue { layers } => Continue { layers: *layers },
            Exit { layers, val } => Exit {
                layers: *layers,
                val: val.as_heap(tree),
            },
            Return { val } => Return {
                val: val.as_heap(tree),
            },
        }
    }
    fn display<W: NodeWrapping<'src>>(
        &self,
        tree: &'src Tree<'src, W>,
        internalizer: &Internalizer<'src>,
        indentation: String,
    ) -> String {
        use Jump::*;
        match self {
            Continue { layers } => format!("continue * {layers}"),
            Exit { layers, val } => format!(
                "exit * {} {}",
                layers,
                val.display(tree, internalizer, indentation)
            ),
            Return { val } => format!("return {}", val.display(tree, internalizer, indentation)),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path<P: Clone + Debug + PartialEq + Eq> {
    pub content: P,
    pub jump: Option<Box<Jump<P>>>,
}
impl<P: Clone + Debug + PartialEq + Eq> Path<P> {
    #[inline]
    pub fn new(content: P) -> Self {
        Self {
            content,
            jump: None,
        }
    }
}
impl<'src> Path<NodeId<'src>> {
    fn as_heap<W: NodeWrapping<'src>>(
        &'src self,
        tree: &'src Tree<'src, W>,
    ) -> Path<Box<HeapNode<'src>>> {
        Path {
            content: Box::new(tree[self.content].node().unwrap().as_heap(tree)),
            jump: self.jump.as_ref().map(|jump| Box::new(jump.as_heap(tree))),
        }
    }
    fn display<W: NodeWrapping<'src>>(
        &self,
        tree: &'src Tree<'src, W>,
        internalizer: &Internalizer<'src>,
        indentation: String,
    ) -> String {
        tree[self.content].display(tree, internalizer, indentation.clone())
            + &self.jump.as_ref().map_or("".to_owned(), |jump| {
                format!(
                    "\n{}{}",
                    indentation.clone(),
                    jump.display(tree, internalizer, indentation)
                )
            })
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node<'src> {
    // control flow structures
    Conditional {
        condition: NodeId<'src>,
        looping: bool,
        then_body: Path<NodeId<'src>>,
        else_body: Option<Path<NodeId<'src>>>,
    }, // if/loop condition then_body (else else_body)

    Binding {
        left: NodeId<'src>,
        right: NodeId<'src>,
    },
    // single values
    Literal {
        val: BigUint,
    }, // (0(b/s/o/d/x))N(.N)((u/i/f/c)(0(b/s/o/d/x))N)(i)
    Quote(String), // "..."
    Placeholder,   // ..

    // identifiers
    Ident(Symbol<'src>),       // x
    Lifetime(Symbol<'src>),    // 'x
    Field(Symbol<'src>),       // .x
    PrimitiveType(Type<'src>), // u32, i32, c32, f32, ...
    Unit,

    // multiple values
    List(comp::Vec<NodeId<'src>, 2>),        // a, b, c, d, ...
    ColonStruct(comp::Vec<NodeId<'src>, 2>), // a : b : c : d
    Statements(comp::Vec<NodeId<'src>, 2>),  // a b c d ...

    // operations
    Binary {
        op: BinaryOp,
        left: NodeId<'src>,
        right: NodeId<'src>,
    }, // left op right
    Chain {
        first: NodeId<'src>,
        additions: comp::Vec<(BinaryOp, NodeId<'src>), 1>,
    }, // first op additions[0].0 additions[0].1
    Unary {
        op: UnaryOp,
        val: NodeId<'src>,
    }, // op operand
    Fields {
        val: NodeId<'src>,
        fields: comp::Vec<Symbol<'src>, 1>,
    },
    Brackets {
        kind: Bracket,
        content: NodeId<'src>,
    }, // squared content squared
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HeapNode<'src> {
    // control flow structures
    Conditional {
        condition: Box<HeapNode<'src>>,
        looping: bool,
        then_body: Path<Box<HeapNode<'src>>>,
        else_body: Option<Path<Box<HeapNode<'src>>>>,
    }, // if/loop condition then_body (else else_body)

    Binding {
        left: Box<HeapNode<'src>>,
        right: Box<HeapNode<'src>>,
    },
    // single values
    Literal {
        val: BigUint,
    }, // (0(b/s/o/d/x))N(.N)((u/i/f/c)(0(b/s/o/d/x))N)(i)
    Quote(String), // "..."
    Placeholder,   // ..

    // identifiers
    Ident(Symbol<'src>),       // x
    Lifetime(Symbol<'src>),    // 'x
    Field(Symbol<'src>),       // .x
    PrimitiveType(Type<'src>), // u32, i32, c32, f32, ...
    Unit,

    // multiple values
    List(comp::Vec<Box<HeapNode<'src>>, 2>), // a, b, c, d, ...
    ColonStruct(comp::Vec<Box<HeapNode<'src>>, 2>), // a : b : c : d
    Statements(comp::Vec<Box<HeapNode<'src>>, 2>),

    // operations
    Binary {
        op: BinaryOp,
        left: Box<HeapNode<'src>>,
        right: Box<HeapNode<'src>>,
    }, // left op right
    Chain {
        first: Box<HeapNode<'src>>,
        additions: comp::Vec<(BinaryOp, Box<HeapNode<'src>>), 1>,
    }, // first op additions[0].0 additions[0].1
    Unary {
        op: UnaryOp,
        operand: Box<HeapNode<'src>>,
    }, // op operand
    Fields {
        val: Box<HeapNode<'src>>,
        fields: comp::Vec<Symbol<'src>, 1>,
    }, // val.fields
    Brackets {
        kind: Bracket,
        content: Box<HeapNode<'src>>,
    }, // squared content squared
}
impl<'src> Node<'src> {
    fn as_heap<W: NodeWrapping<'src>>(&'src self, tree: &'src Tree<'src, W>) -> HeapNode<'src> {
        fn boxed<'src, W: NodeWrapping<'src>>(
            node: &NodeId<'src>,
            tree: &'src Tree<'src, W>,
        ) -> Box<HeapNode<'src>> {
            Box::new(tree[*node].node().unwrap().as_heap(tree))
        }
        use HeapNode::*;
        match self {
            Self::Conditional {
                condition,
                looping,
                then_body,
                else_body,
            } => Conditional {
                condition: boxed(condition, tree),
                looping: *looping,
                then_body: then_body.as_heap(tree),
                else_body: else_body.as_ref().map(|else_body| else_body.as_heap(tree)),
            },
            Self::Binding { left, right } => Binding {
                left: boxed(left, tree),
                right: boxed(right, tree),
            },
            Self::Literal { val } => Literal { val: val.clone() },
            Self::Quote(string) => Quote(string.clone()),
            Self::Placeholder => Placeholder,
            Self::Ident(symbol) => Ident(*symbol),
            Self::Lifetime(symbol) => Lifetime(*symbol),
            Self::Field(symbol) => Field(*symbol),
            Self::PrimitiveType(kind) => PrimitiveType(kind.clone()),
            Self::Unit => Unit,
            Self::List(content) => List(
                content
                    .into_iter()
                    .map(|val| boxed(val, tree))
                    .collect::<comp::Vec<_, 2>>(),
            ),
            Self::ColonStruct(content) => ColonStruct(
                content
                    .into_iter()
                    .map(|val| boxed(val, tree))
                    .collect::<comp::Vec<_, 2>>(),
            ),
            Self::Statements(content) => Statements(
                content
                    .into_iter()
                    .map(|val| boxed(val, tree))
                    .collect::<comp::Vec<_, 2>>(),
            ),
            Self::Binary { op, left, right } => Binary {
                op: *op,
                left: boxed(left, tree),
                right: boxed(right, tree),
            },
            Self::Chain { first, additions } => Chain {
                first: boxed(first, tree),
                additions: additions
                    .into_iter()
                    .map(|item| (item.0, boxed(&item.1, tree)))
                    .collect(),
            },
            Self::Unary { op, val: operand } => Unary {
                op: *op,
                operand: boxed(operand, tree),
            },
            Self::Fields { val, fields } => Fields {
                val: boxed(val, tree),
                fields: fields.clone(),
            },
            Self::Brackets { kind, content } => Brackets {
                kind: *kind,
                content: boxed(content, tree),
            },
        }
    }
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
            .join(", "),
    )
}*/
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeId<'tree> {
    _marker: PhantomData<&'tree ()>,
    idx: usize,
}

impl<'tree> NodeId<'tree> {
    #[inline]
    fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData,
            idx,
        }
    }
}
#[derive(Clone, Debug)]
pub struct Tree<'src, W: NodeWrapping<'src>> {
    _marker: PhantomData<&'src ()>,
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
impl<'src, W: NodeWrapping<'src> + 'src> Tree<'src, W> {
    pub fn new() -> Self {
        Self {
            _marker: PhantomData,
            nodes: vec![],
        }
    }
    pub fn add(&mut self, node: W) -> NodeId<'src> {
        self.nodes.push(node);
        NodeId::new(self.nodes.len() - 1)
    }
    pub fn move_to_new_location(&mut self, node: NodeId<'src>) -> NodeId<'src> {
        self.add(self[node].clone())
    }

    pub fn build_graph(&'src self, root: NodeId<'src>) -> Box<HeapNode<'src>> {
        Box::new(self[root].node().unwrap().as_heap(self))
    }
}

impl<'src, W: NodeWrapping<'src>> Tree<'src, W> {
    pub fn to_string(&'src self, root: NodeId<'src>, internalizer: &Internalizer<'src>) -> String {
        self[root]
            .display(self, internalizer, String::new())
            .to_string()
    }
}
