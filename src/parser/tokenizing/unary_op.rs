use std::fmt;

#[derive(Clone, Copy, PartialEq, Debug)]
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
impl From<PrefixUnaryOp> for UnaryOp {
    fn from(value: PrefixUnaryOp) -> Self {
        use PrefixUnaryOp::*;
        match value {
            Ref => UnaryOp::Ref,
            Not => UnaryOp::Not,
            Neg => UnaryOp::Neg,
            Pos => UnaryOp::Pos,
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

            Increment => "++",
            Decrement => "--",
        };
        write!(f, "{}", string)
    }
}
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum PrefixUnaryOp {
    // infront of one argument // unary - prefix - ops
    Ref, // @a
    Not, // !a
    Neg, // -a
    Pos, // +a
}

impl fmt::Display for PrefixUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PrefixUnaryOp::*;
        let string = match self {
            Ref => "->",
            Not => "!",
            Neg => "- {",
            Pos => "+ {",
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
