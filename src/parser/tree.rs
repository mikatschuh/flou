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
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String;
}

const DISPLAY_INDENTATION: &str = "|   ";
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
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String {
        let Some(ref node) = self.node else {
            return "None".to_owned();
        };
        let next_indentation = indentation.clone() + DISPLAY_INDENTATION;
        use Node::*;

        match node {
            Binding { exprs } => {
                format!(
                    "{}{}",
                    exprs.first().display(internalizer, indentation.clone()),
                    exprs
                        .iter()
                        .skip(1)
                        .map(|expr| format!(
                            "\n{}= {}",
                            indentation.clone(),
                            expr.display(internalizer, format!("{}  ", indentation))
                        ))
                        .collect::<String>()
                )
            }
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
            Label { label, content } => format!(
                "Label  {} - {}",
                internalizer.resolve(*label),
                content.display(internalizer, indentation)
            ),
            PrimitiveType(ty) => format!("Type  {}", ty.display(internalizer, indentation)),
            Unit => "()".to_owned(),
            Binary { op, lhs, rhs } => format!(
                "{op} {{\n{}{}\n{}{} \n{}}}",
                next_indentation.clone(),
                lhs.display(internalizer, next_indentation.clone()),
                next_indentation.clone(),
                rhs.display(internalizer, next_indentation),
                indentation
            ),
            Unary { op, val } => format!(
                "{op} {}",
                val.display(
                    internalizer,
                    indentation
                        + &op
                            .to_string()
                            .chars()
                            .fold(String::new(), |acc, _| acc + " ")
                        + " "
                )
            ),
            Ref { lifetime, val } => {
                format!(
                    "'{} -> {}",
                    lifetime
                        .as_ref()
                        .map_or("_", |lifetime| internalizer.resolve(*lifetime)),
                    val.display(internalizer, indentation)
                )
            }
            List(list) => {
                if list.len() < 2 {
                    format!(
                        "[{}]",
                        list.iter()
                            .map(|item| { item.display(internalizer, next_indentation.clone()) })
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
                                    item.display(internalizer, next_indentation.clone())
                                )
                            })
                            .collect::<String>(),
                        indentation
                    )
                }
            }
            Contract { lhs, rhs } => format!(
                ": {{\n{}{}\n{}{} \n{}}}",
                next_indentation.clone(),
                lhs.display(internalizer, next_indentation.clone()),
                next_indentation.clone(),
                rhs.display(internalizer, next_indentation),
                indentation
            ),
            Statements(content) => {
                if content.len() == 1 {
                    content[0].display(internalizer, next_indentation.clone())
                } else {
                    let mut string = String::new();
                    if content.len() == 0 {
                        string += &format!("\n{next_indentation}")
                    } else {
                        content.into_iter().map(|node| node).for_each(|node| {
                            string += &format!(
                                "\n{}{}",
                                next_indentation,
                                &node.display(internalizer, next_indentation.clone())
                            )
                        });
                    }
                    format!("_{string}\n{indentation}¯")
                }
            }
            Chain { first, additions } => format!(
                "Chained [\n{}{}{}\n{}]",
                next_indentation.clone(),
                first.display(internalizer, next_indentation.clone() + "   "),
                additions
                    .iter()
                    .map(|item| {
                        let op = item.0.to_string();
                        format!(
                            "\n{}{}{}{}",
                            next_indentation.clone(),
                            op,
                            (0..3 - op.chars().count()).map(|_| " ").collect::<String>(),
                            item.1
                                .display(internalizer, next_indentation.clone() + "   ")
                        )
                    })
                    .collect::<String>(),
                indentation
            ),
            Loop {
                condition,
                then_body,
                else_body,
            } => format!(
                "{} {}\n{}=> {}{}",
                "loop",
                condition.display(internalizer, indentation.clone() + "     "),
                indentation.clone(),
                then_body.display(internalizer, indentation.clone() + "   "),
                else_body
                    .as_ref()
                    .map_or("".to_owned(), |else_body| format!(
                        "\n{}else {}",
                        indentation.clone(),
                        &else_body.display(internalizer, indentation + "     "),
                    ))
            ),
            If {
                condition,
                then_body,
                else_body,
            } => format!(
                "{} {}\n{}=> {}{}",
                "if",
                condition.display(internalizer, indentation.clone() + "   "),
                indentation.clone(),
                then_body.display(internalizer, indentation.clone() + "   "),
                else_body
                    .as_ref()
                    .map_or("".to_owned(), |else_body| format!(
                        "\n{}else {}",
                        indentation.clone(),
                        &else_body.display(internalizer, indentation + "     "),
                    ))
            ),
            Proc { convention, body } => {
                format!(
                    "proc {}{}",
                    convention.as_ref().map_or_else(
                        || "".to_owned(),
                        |conv| format!("{} ", conv.display(internalizer, indentation.clone()))
                    ),
                    body.display(internalizer, indentation)
                )
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Jump<'src> {
    Continue,
    Break {
        layers: usize,
        val: Option<Box<NodeBox<'src>>>,
    },
    Return {
        layers: usize,
        val: Option<Box<NodeBox<'src>>>,
    },
}

impl<'src> TreeDisplay<'src> for Jump<'src> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String {
        use Jump::*;
        match self {
            Continue => format!("continue"),
            Break { layers, val } => format!(
                "break * {}{}",
                layers + 1,
                val.as_ref().map_or_else(
                    || "".to_owned(),
                    |val| format!(" {}", val.display(internalizer, indentation))
                )
            ),
            Return { layers, val } => format!(
                "return * {}{}",
                layers + 1,
                val.as_ref().map_or_else(
                    || "".to_owned(),
                    |val| format!(" {}", val.display(internalizer, indentation))
                )
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Path<'src> {
    pub node: Option<NodeBox<'src>>,
    pub jump: Option<Jump<'src>>,
}

impl<'src> Path<'src> {
    #[inline]
    pub const fn is_none(&self) -> bool {
        self.node.is_none() && self.jump.is_none()
    }
}

impl<'src> TreeDisplay<'src> for Path<'src> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String {
        match self.node.as_ref() {
            Some(node) => {
                node.display(internalizer, indentation.clone())
                    + &self.jump.as_ref().map_or_else(
                        || "".to_owned(),
                        |jump| {
                            format!(
                                "\n{}{}",
                                indentation.clone(),
                                jump.display(internalizer, indentation)
                            )
                        },
                    )
            }
            None => format!(
                "{}",
                self.jump.as_ref().map_or_else(
                    || "".to_owned(),
                    |jump| jump.display(internalizer, indentation)
                )
            ),
        }
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

    Binding {
        exprs: comp::Vec<NodeBox<'src>, 2>,
    },
    Contract {
        lhs: NodeBox<'src>,
        rhs: NodeBox<'src>,
    }, // a: type

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

    Ref {
        lifetime: Option<Symbol<'src>>,
        val: NodeBox<'src>,
    },
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
