use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    // infront of one argument // unary - prefix - ops
    Ref, // -> a
    Not, // !a

    Neg, // -a
    Pos, // +a

    // after one argument // unary - postfix - ops
    Increment, // a++
    Decrement, // b--
}

use UnaryOp::*;
impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Ref => "->",
            Not => "!",

            Neg => "-",
            Pos => "+",

            Increment => "++",
            Decrement => "--",
        };
        write!(f, "{string}")
    }
}

impl UnaryOp {
    pub(in crate::parser) fn is_postfix(self) -> bool {
        matches!(self, Decrement | Increment)
    }
}
