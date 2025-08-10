use std::fmt;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum UnaryOp {
    // infront of one argument // unary - prefix - ops
    Ref,   // -> a
    Deref, // *a
    Not,   // !a

    Neg, // -a

    LfT, // 'a

    // after one argument // unary - postfix - ops
    Increment, // a++
    Decrement, // b--
}
use UnaryOp::*;
impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Ref => "->",
            Deref => "*",
            Not => "!",

            Neg => "-",

            LfT => "'",

            Increment => "++",
            Decrement => "--",
        };
        write!(f, "{}", string)
    }
}

impl UnaryOp {
    pub(in crate::parser) fn is_postfix(self) -> bool {
        matches!(self, Decrement | Increment)
    }
}
