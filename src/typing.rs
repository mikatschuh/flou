use num::BigUint;

use crate::tree::NodeId;

pub type OptSize = Option<BigUint>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NumberType {
    Arbitrary,
    Unsigned(OptSize),
    Signed(OptSize),
    Float(OptSize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'tree> {
    Number(NumberType),
    Expr(NodeId<'tree>),
}

impl<'tree> From<NumberType> for Type<'tree> {
    fn from(value: NumberType) -> Self {
        Type::Number(value)
    }
}

impl<'tree> From<NodeId<'tree>> for Type<'tree> {
    fn from(value: NodeId<'tree>) -> Self {
        Type::Expr(value)
    }
}
