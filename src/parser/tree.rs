use bumpalo::boxed::Box as BumpBox;
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
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
    vec,
};

pub trait TreeDisplay<'src> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: &String) -> String;
}

#[derive(Debug, PartialEq, Eq)]
pub struct NodeWrapper<'src> {
    pub span: Span,
    pub node: Option<Node<'src>>,
    pub typed: Option<Type<'src>>,
    pub notes: Vec<Note<'src>>,
}

impl<'src> NodeWrapper<'src> {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            node: None,
            notes: vec![],
            typed: None,
        }
    }
    #[inline]
    pub fn with_note(mut self, comment: Note<'src>) -> Self {
        self.notes.push(comment);
        self
    }
    #[inline]
    pub fn with_notes(mut self, mut comments: Vec<Note<'src>>) -> Self {
        self.notes.append(&mut comments);
        self
    }
    #[inline]
    pub fn with_type(mut self, typed: Type<'src>) -> Self {
        self.typed = Some(typed);
        self
    }
    #[inline]
    pub fn with_node(mut self, node: Node<'src>) -> Self {
        self.node = Some(node);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Note<'src> {
    NumberParsingNote { invalid_suffix: &'src str },
    EscapeSequenceConfusion(EscapeSequenceConfusion),
}

impl<'src> From<EscapeSequenceConfusion> for Note<'src> {
    fn from(value: EscapeSequenceConfusion) -> Self {
        Self::EscapeSequenceConfusion(value)
    }
}

impl<'src> TreeDisplay<'src> for NodeWrapper<'src> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: &String) -> String {
        let Some(ref node) = self.node else {
            return "•".to_owned();
        };

        /// Unicode Characters:
        /// ─ │ ┌ ┐ └ ┘ ├ ┤ ┬ ┴ ┼ ╭ ╮ ╰ ╯
        ///
        /// ┌─ Node
        /// │─ (=)
        /// │─ (<)

        const FORK_START: &str = "┌─";
        const FORK_END: &str = "╰─";
        const BRANCH: &str = "│─";
        const VERTICAL_PLUS_2: &str = "│  ";

        macro_rules! tree {
            (vec $nodes:expr, $others:expr) => {{
                let mut nodes = $nodes.iter().rev();
                let last = nodes.next().unwrap();
                let mut nodes = nodes.rev();
                format!(
                    "{FORK_START} {}{}{}",
                    $others(nodes.next().unwrap(), indentation.clone() + VERTICAL_PLUS_2),
                    nodes
                        .map(|node| format!(
                            "\n{indentation}{BRANCH} {}",
                            $others(node, indentation.clone() + VERTICAL_PLUS_2)
                        ))
                        .collect::<String>(),
                    format!(
                        "\n{indentation}{FORK_END} {}",
                        $others(last, indentation.clone() + "   ")
                    )
                )
            }};
            (vec $nodes:expr, $first:expr, $others:expr) => {{
                let mut nodes = $nodes.iter().rev();
                let last = nodes.next().unwrap();
                let mut nodes = nodes.rev();
                format!(
                    "{FORK_START} {}{}{}",
                    $first(nodes.next().unwrap(), indentation.clone() + VERTICAL_PLUS_2),
                    nodes
                        .map(|node| format!(
                            "\n{indentation}{BRANCH} {}",
                            $others(node, indentation.clone() + VERTICAL_PLUS_2)
                        ))
                        .collect::<String>(),
                    format!(
                        "\n{indentation}{FORK_END} {}",
                        $others(last, indentation.clone() + "   ")
                    )
                )
            }};
            ($root:expr, vec $nodes:expr, $others:expr) => {{
                let mut nodes = $nodes.iter().rev();
                if let Some(last) = nodes.next() {
                    let nodes = nodes.rev();
                    format!(
                        "{}{}\n{indentation}{FORK_END} {}",
                        $root,
                        nodes
                            .map(|node| format!(
                                "\n{indentation}{BRANCH} {}",
                                $others(node, indentation.clone() + VERTICAL_PLUS_2)
                            ))
                            .collect::<String>(),
                        $others(last, indentation.clone() + "   ")
                    )
                } else {
                    $root.to_string()
                }
            }};
            ($root:expr, [$($prev_nodes:expr),*], $prev_others:expr, vec $nodes:expr, $others:expr) => {{
                let mut nodes = $nodes.iter().rev();
                let last = nodes.next().unwrap();
                let nodes = nodes.rev();
                format!(
                    "{}{}{}\n{indentation}{FORK_END} {}",
                    $root,
                    vec![$(format!(
                        "\n{indentation}{BRANCH} {}",
                        $prev_others($prev_nodes, indentation.clone() + VERTICAL_PLUS_2)
                    )), *]
                        .into_iter()
                        .collect::<String>(),
                    nodes
                        .map(|node| format!(
                            "\n{indentation}{BRANCH} {}",
                            $others(node, indentation.clone() + VERTICAL_PLUS_2)
                        ))
                        .collect::<String>(),
                    $others(last, indentation.clone() + "   ")
                )
            }};
            ($root:expr, $last:expr, $others:expr) => {
                format!(
                    "{}\n{indentation}{FORK_END} {}",
                    $root,
                    $others($last, indentation.clone() + "   ")
                )
            };
            ($root:expr, [$($nodes:expr),*], $last:expr, $others:expr) => {
                format!(
                    "{}{}\n{indentation}{FORK_END} {}",
                    $root,
                    vec![$(format!(
                        "\n{indentation}{BRANCH} {}",
                        $others($nodes, indentation.clone() + VERTICAL_PLUS_2)
                    )), *]
                        .into_iter()
                        .collect::<String>(),
                    $others($last, indentation.clone() + "   ")
                )
            };
        }

        use Node::*;
        match node {
            Binding { exprs } => tree!(
                vec exprs,
                |node: &NodeBox<'src>, indent| node.display(internalizer, &indent),
                |node: &NodeBox<'src>, indent| format!("(=) {}", node.display(internalizer, &(indent + "    ")))
            ),
            Literal { val } => format!(
                "{}{}",
                val,
                self.typed.as_ref().map_or("".to_owned(), |ty| format!(
                    " - {}",
                    ty.display(internalizer, indentation)
                ))
            ),
            Ident(id) => format!("Id  {}", internalizer.resolve(*id)),
            Lifetime(sym) => format!("Lifetime  {}", internalizer.resolve(*sym)),
            Placeholder => "..".to_owned(),
            Quote(quote) => format!("Quote  \"{}\"", with_written_out_escape_sequences(quote)),
            Label { label, content } => tree!(
                format!("Label  {}", internalizer.resolve(*label)),
                content,
                |node: &NodeBox<'src>, indent| node.display(internalizer, &indent)
            ),
            PrimitiveType(ty) => format!("Type  {}", ty.display(internalizer, indentation)),
            Unit => "()".to_owned(),
            Binary { op, lhs, rhs } => tree!(op, [lhs], rhs, |node: &NodeBox<'src>, indent| node
                .display(internalizer, &indent)),
            Unary { op, val } => tree!(op, val, |node: &NodeBox<'src>, indent| node
                .display(internalizer, &indent)),
            List(list) => {
                tree!("[]", vec list, |node: &NodeBox<'src>, indent| node.display(internalizer, &indent))
            }
            Or(list) => {
                tree!("|", vec list, |node: &NodeBox<'src>, indent| node.display(internalizer, &indent))
            }
            Contract { lhs, rhs } => {
                tree!(":", [lhs], rhs, |node: &NodeBox<'src>, indent| {
                    node.display(internalizer, &indent)
                })
            }
            Lifetimed { sym, val } => tree!(
                format!("'{}", internalizer.resolve(*sym)),
                val,
                |val: &NodeBox<'src>, indent| val.display(internalizer, &indent)
            ),
            Statements(content) => {
                tree!(vec content, |node: &NodeBox<'src>, indent| node.display(internalizer, &indent))
            }
            Chain { first, additions } => tree!(
                first.display(internalizer, indentation),
                vec additions,
                |node: &(BinaryOp, NodeBox<'src>), indent| format!(
                    "{} {}",
                    node.0,
                    node.1.display(
                        internalizer,
                        &(indent + format!("{} ", node.0.to_string().chars().map(|_| " ").collect::<String>()).as_ref())))
            ),
            Loop {
                condition,
                then_body,
                else_body,
            } => {
                let vector = else_body
                    .as_ref()
                    .map_or_else(|| vec![then_body], |else_body| vec![then_body, &else_body]);
                tree!(
                    "loop",
                    [condition],
                    |node: &NodeBox<'src>, indent| node.display(internalizer, &indent),
                    vec vector,
                    |node: &Path<'src>, indent| node.display(internalizer, &indent)
                )
            }
            If {
                condition,
                then_body,
                else_body,
            } => {
                let vector = else_body
                    .as_ref()
                    .map_or_else(|| vec![then_body], |else_body| vec![then_body, &else_body]);
                tree!(
                    "if",
                    [condition],
                    |node: &NodeBox<'src>, indent| node.display(internalizer, &indent),
                    vec vector,
                    |node: &Path<'src>, indent| node.display(internalizer, &indent)
                )
            }
            Proc { convention, body } => {
                let body = vec![body];
                tree!(
                    "proc",
                    [convention],
                    |conv: &Option<NodeBox<'src>>, indent| conv.as_ref().map_or_else(
                        || "\"default\"".to_owned(),
                        |conv| conv.display(internalizer, &indent)
                    ),
                    vec body,
                    |body: &Path<'src>, indent| body.display(internalizer, &indent)
                )
            }
            Continue { layers } => format!("continue * {}", layers + 1),
            Break { layers, val } => tree!(
                format!("break * {layers}"),
                val,
                |val: &NodeBox<'src>, indent| val.display(internalizer, &indent)
            ),
            Return { val } => tree!(format!("return"), val, |val: &NodeBox<'src>, indent| val
                .display(internalizer, &indent)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Path<'src> {
    pub node: NodeBox<'src>,
}

impl<'src> TreeDisplay<'src> for Path<'src> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: &String) -> String {
        self.node.display(internalizer, &indentation)
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

#[derive(Debug, PartialEq, Eq)]
pub enum Node<'src> {
    // control flow structures
    Loop {
        condition: NodeBox<'src>,
        then_body: Path<'src>,
        else_body: Option<Path<'src>>,
    }, // loop condition then_body (else else_body)

    If {
        condition: NodeBox<'src>,
        then_body: Path<'src>,
        else_body: Option<Path<'src>>,
    }, // loop condition then_body (else else_body)

    Proc {
        convention: Option<NodeBox<'src>>,
        body: Path<'src>,
    },

    Continue {
        layers: usize,
    },
    Break {
        layers: usize,
        val: NodeBox<'src>,
    },
    Return {
        val: NodeBox<'src>,
    },

    Binding {
        exprs: comp::Vec<NodeBox<'src>, 2>,
    },
    Contract {
        lhs: NodeBox<'src>,
        rhs: NodeBox<'src>,
    }, // a: type
    Lifetimed {
        sym: Symbol<'src>,
        val: NodeBox<'src>,
    },

    // single values
    Literal {
        val: BigUint,
    }, // (0(b/s/o/d/x))N(.N)((u/i/f/c)(0(b/s/o/d/x))N)(i)
    Quote(String), // "..."
    Placeholder,   // ..
    Label {
        label: Symbol<'src>,
        content: NodeBox<'src>,
    },

    // identifiers
    Ident(Symbol<'src>),    // x
    Lifetime(Symbol<'src>), // 'x

    PrimitiveType(Type<'src>), // u32, i32, c32, f32, ...
    Unit,
    Or(Vec<NodeBox<'src>>),

    // multiple values
    List(comp::Vec<NodeBox<'src>, 2>),       // a, b, c, d, ...
    Statements(comp::Vec<NodeBox<'src>, 2>), // a b c d ...

    // operations
    Binary {
        op: BinaryOp,
        lhs: NodeBox<'src>,
        rhs: NodeBox<'src>,
    }, // left op right
    Chain {
        first: NodeBox<'src>,
        additions: comp::Vec<(BinaryOp, NodeBox<'src>), 1>,
    }, // first op additions[0].0 additions[0].1
    Unary {
        op: UnaryOp,
        val: NodeBox<'src>,
    }, // op operand
}

#[derive(Debug, PartialEq, Eq)]
pub struct NodeBox<'a> {
    ptr: bumpalo::boxed::Box<'a, NodeWrapper<'a>>,
}

impl<'a> NodeBox<'a> {
    #[inline]
    pub fn new(ptr: BumpBox<'a, NodeWrapper<'a>>) -> Self {
        Self { ptr }
    }
}

impl<'src> Deref for NodeBox<'src> {
    type Target = NodeWrapper<'src>;
    fn deref(&self) -> &Self::Target {
        self.ptr.as_ref()
    }
}

impl<'src> DerefMut for NodeBox<'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ptr.as_mut()
    }
}
