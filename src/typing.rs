use std::fmt::Display;

use crate::tree::NodeId;

pub type OptSize = Option<usize>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativNumber {
    Arbitrary,
    Unsigned(OptSize),
    Signed(OptSize),
    Float(OptSize),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type<'tree> {
    Number(NativNumber),
    ComplexNumber(NativNumber),
    Expr(NodeId<'tree>),
}
