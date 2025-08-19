use std::fmt::Display;

use num::BigUint;

use crate::{
    parser::intern::Internalizer,
    tree::{NodeId, NodeWrapping, Tree},
};

pub type OptSize = Option<BigUint>;

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
