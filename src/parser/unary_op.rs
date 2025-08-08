use std::fmt;

use crate::parser::binary_op::BindingPow;

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

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnaryOp::*;
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
        matches!(self, Self::Decrement | Self::Increment)
    }
}
impl BindingPow for UnaryOp {
    fn binding_pow(self) -> i8 {
        use UnaryOp::*;
        match self {
            Neg => 13,

            Ref => 18,
            Deref => 18,
            Not => 18,

            Increment => 17,
            Decrement => 17,

            LfT => 20,
        }
    }
}
