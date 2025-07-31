use std::fmt;

use crate::parser::tokenizing::binary_op::BindingPow;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum UnaryOp {
    // infront of one argument // unary - prefix - ops
    Ref, // -> a
    Not, // !a

    Neg, // -a
    Pos, // +a

    LfT, // 'a

    // after one argument // unary - postfix - ops
    Increment, // a++
    Decrement, // b--
}
impl From<PrefixUnaryOp> for UnaryOp {
    fn from(value: PrefixUnaryOp) -> Self {
        use PrefixUnaryOp::*;
        match value {
            Ref => UnaryOp::Ref,
            Not => UnaryOp::Not,
            Neg => UnaryOp::Neg,
            Pos => UnaryOp::Pos,
            LfT => UnaryOp::LfT,
        }
    }
}
impl From<PostfixUnaryOp> for UnaryOp {
    fn from(value: PostfixUnaryOp) -> Self {
        use PostfixUnaryOp::*;
        match value {
            Increment => UnaryOp::Increment,
            Decrement => UnaryOp::Decrement,
        }
    }
}
impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnaryOp::*;
        let string = match self {
            Ref => "->",
            Not => "!",

            Neg => "-",
            Pos => "+",

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
    fn binding_pow(self) -> f32 {
        use UnaryOp::*;
        match self {
            Neg => 5.4,
            Pos => 5.4,

            Ref => 9.0,
            Not => 9.0,

            Increment => 8.0,
            Decrement => 8.0,

            LfT => 11.0,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum PrefixUnaryOp {
    // infront of one argument // unary - prefix - ops
    Ref, // -> a
    Not, // !a
    Neg, // -a
    Pos, // +a
    LfT, // 'a
}

impl fmt::Display for PrefixUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PrefixUnaryOp::*;
        let string = match self {
            Ref => "->",
            Not => "!",
            Neg => "- {",
            Pos => "+ {",
            LfT => "'",
        };
        write!(f, "{}", string)
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum PostfixUnaryOp {
    // after one argument // unary - postfix - ops
    Increment, // a++
    Decrement, // b--
}

impl fmt::Display for PostfixUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PostfixUnaryOp::*;
        let string = match self {
            Increment => "++",
            Decrement => "--",
        };
        write!(f, "{}", string)
    }
}
