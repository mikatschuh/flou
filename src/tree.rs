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
const DISPLAY_INDENTATION_NEG_1: &str = "   ";
#[derive(Debug, PartialEq, Eq)]
pub struct NodeWrapper<'src, J: Jump<'src>> {
    pub span: Span,
    pub node: Option<Node<'src, J>>,
    pub typed: Option<Type<'src, J>>,
    pub notes: Vec<Note<'src>>,
}

impl<'src, J: Jump<'src>> NodeWrapper<'src, J> {
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
    pub fn with_type(mut self, typed: Type<'src, J>) -> Self {
        self.typed = Some(typed);
        self
    }
    #[inline]
    pub fn with_node(mut self, node: Node<'src, J>) -> Self {
        self.node = Some(node);
        self
    }
}

impl<'src, J: Jump<'src>> TreeDisplay<'src> for NodeWrapper<'src, J> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String {
        let Some(ref node) = self.node else {
            return "None".to_owned();
        };
        let next_indentation = indentation.clone() + DISPLAY_INDENTATION;
        use Node::*;

        match node {
            Binding { lhs, rhs } => {
                format!(
                    "= {{\n{}{}\n{}{} \n{}}}",
                    next_indentation.clone(),
                    lhs.display(internalizer, next_indentation.clone()),
                    next_indentation.clone(),
                    rhs.display(internalizer, next_indentation),
                    indentation
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
            Field(sym) => format!("Field  {}", internalizer.resolve(*sym)),
            Placeholder => "..".to_owned(),
            Quote(quote) => format!("Quote  \"{}\"", with_written_out_escape_sequences(quote)),
            PrimitiveType(..) => todo!(),
            Unit => "()".to_owned(),
            Binary {
                op,
                lhs: left,
                rhs: right,
            } => format!(
                "{op} {{\n{}{}\n{}{} \n{}}}",
                next_indentation.clone(),
                left.display(internalizer, next_indentation.clone()),
                next_indentation.clone(),
                right.display(internalizer, next_indentation),
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
            ColonStruct(content) => {
                if content.len() == 1 {
                    content[0].display(internalizer, next_indentation.clone())
                } else {
                    let mut list = content.iter();
                    list.next()
                        .unwrap()
                        .display(internalizer, indentation.clone() + "  ")
                        + &list
                            .map(|item| {
                                format!(
                                    "\n{}: {}",
                                    indentation.clone(),
                                    item.display(internalizer, indentation.clone() + "  ")
                                )
                            })
                            .collect::<String>()
                }
            }
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
                        let op = item.0.as_str();
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
            Fields { val, fields } => format!(
                "{}{}",
                val.display(internalizer, indentation.clone()),
                fields
                    .iter()
                    .map(|sym| format!(" . {}", internalizer.resolve(*sym)))
                    .collect::<String>()
            ),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
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

pub trait Jump<'src>: Debug + PartialEq + Eq + TreeDisplay<'src> {
    const NONE: Self;
}

#[derive(Debug, PartialEq, Eq)]
pub struct NoJump {}

impl<'src> Jump<'src> for NoJump {
    const NONE: Self = NoJump {};
}

impl<'src> TreeDisplay<'src> for NoJump {
    fn display(&self, _: &Internalizer<'src>, _: String) -> String {
        "".to_owned()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ReturnJump<'src> {
    Return { val: Box<Path<'src, Self>> },
    None,
}

impl<'src> Jump<'src> for ReturnJump<'src> {
    const NONE: Self = Self::None;
}

impl<'src> TreeDisplay<'src> for ReturnJump<'src> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String {
        use ReturnJump::*;
        match self {
            Return { val } => format!("return {}", val.content.display(internalizer, indentation)),
            None => format!(""),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LoopJump<'src> {
    Continue {
        layers: usize,
    },
    Break {
        layers: usize,
        val: Box<Path<'src, Self>>,
    },
    None,
}

impl<'src> Jump<'src> for LoopJump<'src> {
    const NONE: Self = Self::None;
}

impl<'src> TreeDisplay<'src> for LoopJump<'src> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String {
        use LoopJump::*;
        match self {
            Continue { layers } => format!("continue * {layers}"),
            Break { layers, val } => format!(
                "exit * {} {}",
                layers,
                val.content.display(internalizer, indentation)
            ),
            None => format!(""),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum EveryJump<'src> {
    Continue {
        layers: usize,
    },
    Break {
        layers: usize,
        val: Box<Path<'src, Self>>,
    },
    Return {
        val: Box<Path<'src, Self>>,
    },
    None,
}

impl<'src> Jump<'src> for EveryJump<'src> {
    const NONE: Self = Self::None;
}

impl<'src> TreeDisplay<'src> for EveryJump<'src> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String {
        use EveryJump::*;
        match self {
            Continue { layers } => format!("continue * {layers}"),
            Break { layers, val } => format!(
                "exit * {} {}",
                layers,
                val.content.display(internalizer, indentation)
            ),
            Return { val } => format!("return {}", val.content.display(internalizer, indentation)),
            None => format!(""),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Path<'src, J: Jump<'src>> {
    pub content: NodeBox<'src, J>,
    pub jump: J,
}

impl<'src, J: Jump<'src>> Path<'src, J> {
    #[inline]
    pub fn new(content: NodeBox<'src, J>) -> Self {
        Self {
            content,
            jump: J::NONE,
        }
    }
}

impl<'src, J: Jump<'src> + TreeDisplay<'src>> TreeDisplay<'src> for Path<'src, J> {
    fn display(&self, internalizer: &Internalizer<'src>, indentation: String) -> String {
        self.content.display(internalizer, indentation.clone())
            + &format!(
                "\n{}{}",
                indentation.clone(),
                self.jump.display(internalizer, indentation)
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

#[derive(Debug, PartialEq, Eq)]
pub enum Node<'src, J: Jump<'src>> {
    // control flow structures
    Loop {
        condition: NodeBox<'src, J>,
        then_body: Path<'src, EveryJump<'src>>,
        else_body: Option<Path<'src, EveryJump<'src>>>,
    }, // loop condition then_body (else else_body)

    If {
        condition: NodeBox<'src, J>,
        then_body: Path<'src, J>,
        else_body: Option<Path<'src, J>>,
    }, // loop condition then_body (else else_body)

    Binding {
        lhs: NodeBox<'src, J>,
        rhs: NodeBox<'src, J>,
    },
    // single values
    Literal {
        val: BigUint,
    }, // (0(b/s/o/d/x))N(.N)((u/i/f/c)(0(b/s/o/d/x))N)(i)
    Quote(String), // "..."
    Placeholder,   // ..

    // identifiers
    Ident(Symbol<'src>),          // x
    Lifetime(Symbol<'src>),       // 'x
    Field(Symbol<'src>),          // .x
    PrimitiveType(Type<'src, J>), // u32, i32, c32, f32, ...
    Unit,

    // multiple values
    List(comp::Vec<NodeBox<'src, J>, 2>), // a, b, c, d, ...
    ColonStruct(comp::Vec<NodeBox<'src, J>, 2>), // a : b : c : d
    Statements(comp::Vec<NodeBox<'src, J>, 2>), // a b c d ...

    // operations
    Binary {
        op: BinaryOp,
        lhs: NodeBox<'src, J>,
        rhs: NodeBox<'src, J>,
    }, // left op right
    Chain {
        first: NodeBox<'src, J>,
        additions: comp::Vec<(BinaryOp, NodeBox<'src, J>), 1>,
    }, // first op additions[0].0 additions[0].1
    Unary {
        op: UnaryOp,
        val: NodeBox<'src, J>,
    }, // op operand

    Ref {
        lifetime: Option<Symbol<'src>>,
        val: NodeBox<'src, J>,
    },
    Fields {
        val: NodeBox<'src, J>,
        fields: comp::Vec<Symbol<'src>, 1>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct NodeBox<'a, J: Jump<'a>> {
    ptr: bumpalo::boxed::Box<'a, NodeWrapper<'a, J>>,
}

impl<'a, J: Jump<'a>> NodeBox<'a, J> {
    #[inline]
    pub fn new(ptr: BumpBox<'a, NodeWrapper<'a, J>>) -> Self {
        Self { ptr }
    }
    #[inline]
    pub fn clone(&mut self) -> Self {
        unsafe {
            NodeBox::new(BumpBox::from_raw(
                self.ptr.as_mut() as *mut NodeWrapper<'a, J>
            ))
        }
    }
}

impl<'src, J: Jump<'src>> Deref for NodeBox<'src, J> {
    type Target = NodeWrapper<'src, J>;
    fn deref(&self) -> &Self::Target {
        self.ptr.as_ref()
    }
}

impl<'src, J: Jump<'src>> DerefMut for NodeBox<'src, J> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ptr.as_mut()
    }
}
