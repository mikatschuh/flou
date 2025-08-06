#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ChainedOp {
    Equal,          // a == b
    NonEqual,       // a != b
    Smaller,        // a < b
    GreaterOrEqual, // a >= b
    Greater,        // a > b
    SmallerOrEqual, // a <= b
}
use std::fmt;
use ChainedOp::*;

use crate::parser::tokenizing::binary_op::BindingPow;
impl fmt::Display for ChainedOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Equal => "==",
            NonEqual => "!=",
            Smaller => "<",
            GreaterOrEqual => ">=",
            Greater => ">",
            SmallerOrEqual => "<=",
        };
        write!(f, "{}", string)
    }
}
impl BindingPow for ChainedOp {
    fn binding_pow(self) -> i8 {
        match self {
            Equal => 7,          // a == b
            NonEqual => 7,       // a != b
            Smaller => 7,        // a < b
            GreaterOrEqual => 7, // a >= b
            Greater => 7,        // a > b
            SmallerOrEqual => 7, // a <= b
        }
    }
}
impl ChainedOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Equal => "==",
            NonEqual => "!=",
            Smaller => "<",
            GreaterOrEqual => ">=",
            Greater => ">",
            SmallerOrEqual => "<=",
        }
    }
}
