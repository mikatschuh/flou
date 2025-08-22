use std::fmt::Display;

use crate::{
    parser::{intern::Internalizer, num},
    tree::{NodeId, NodeWrapping, Tree},
};

pub type OptSize = Option<usize>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumberKind {
    Unsigned,
    Signed,
    Float,
    Arbitrary,
}
use NumberKind::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumberType {
    pub kind: NumberKind,
    pub size: OptSize,
}

impl Display for NumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            match self.kind {
                Unsigned => "u",
                Signed => "i",
                Float => "f",
                Arbitrary => "()",
            },
            match &self.size {
                Some(size) => size.to_string(),
                None if self.kind != Arbitrary => "_".to_owned(),
                None => "".to_owned(),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'tree> {
    Number(NumberType),
    Expr(NodeId<'tree>),
}
use Type::*;

impl<'tree> From<NumberType> for Type<'tree> {
    fn from(value: NumberType) -> Self {
        Number(value)
    }
}

impl<'tree> From<NodeId<'tree>> for Type<'tree> {
    fn from(value: NodeId<'tree>) -> Self {
        Expr(value)
    }
}

impl<'tree> Type<'tree> {
    pub fn display<W: NodeWrapping<'tree>>(
        &self,
        tree: &Tree<'tree, W>,
        internalizer: &Internalizer<'tree>,
        indentation: String,
    ) -> String {
        match self {
            Number(num) => format!("{num}"),
            Expr(node) => tree[*node].display(tree, internalizer, indentation),
        }
    }
}

#[cfg(target_pointer_width = "64")]
const SYSTEM_SIZE: usize = 64;

#[cfg(target_pointer_width = "32")]
const SYSTEM_SIZE: usize = 32;

#[cfg(target_pointer_width = "16")]
const SYSTEM_SIZE: usize = 16;

pub struct TypeParser {
    target_ptr_size: usize,
}

impl TypeParser {
    pub const fn new() -> Self {
        Self {
            target_ptr_size: SYSTEM_SIZE,
        }
    }
    pub fn parse_number_type(&self, mut input: &[u8]) -> Option<NumberType> {
        let kind = match input.get(0)? {
            b'u' => Unsigned,
            b'i' => Signed,
            b'f' => Float,
            _ => return None,
        };
        input = &input[1..];
        if input.is_empty() {
            return Some(NumberType { kind, size: None });
        };
        Some(NumberType {
            kind,
            size: match input {
                b"x" => Some(self.target_ptr_size),
                _ => Some({
                    let parsed_size = num::parse_number(&mut input).1?.to_u64_digits();
                    let size = parsed_size.first()?;
                    if parsed_size.first().is_some() {
                        return None;
                    }
                    *size as usize
                }),
            },
        })
    }
}
