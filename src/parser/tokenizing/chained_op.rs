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
    fn binding_pow(self) -> f32 {
        match self {
            Equal => 4.0,          // a == b
            NonEqual => 4.0,       // a != b
            Smaller => 4.0,        // a < b
            GreaterOrEqual => 4.0, // a >= b
            Greater => 4.0,        // a > b
            SmallerOrEqual => 4.0, // a <= b
        }
    }
}
