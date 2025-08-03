pub trait BindingPow {
    fn binding_pow(self) -> i8;
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BinaryOp {
    Equation, // a = b

    Write, // a := b

    OrAssign(bool),  // a |= b
    XorAssign(bool), // a >|= b
    AndAssign(bool), // a &= b

    AddAssign, // a += b
    SubAssign, // a -= b

    MulAssign, // a *= b
    DivAssign, // a /= b
    ModAssign, // a %= b

    Swap, // a =|= b

    Or(bool),  // a || b
    Xor(bool), // a >|| b
    And(bool), // a && b

    BitOr(bool),  // a | b
    BitXor(bool), // a >| b
    BitAnd(bool), // a & b

    Add, // a + b
    Sub, // a - b

    Mul, // a * b
    Div, // a / b
    Mod, // a % b

    Dot,   // a · b
    Cross, // a >< b
    Power, // a ^ b

    Index, // a[b]
    App,   // a(b)
}
use std::fmt;
impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinaryOp::*;
        write!(
            f,
            "{}",
            match self {
                Equation => "=",

                Write => ":=",

                AndAssign(false) => "&=",
                AndAssign(true) => "!&=",
                OrAssign(false) => "|=",
                OrAssign(true) => "!|=",
                XorAssign(false) => ">|=",
                XorAssign(true) => "!>|=",

                AddAssign => "+=",
                SubAssign => "-=",

                MulAssign => "*=",
                DivAssign => "/=",
                ModAssign => "%=",

                Swap => "=|=",

                Or(false) => "||",
                Or(true) => "!||",
                And(false) => "&&",
                And(true) => "!&&",
                Xor(false) => ">||",
                Xor(true) => "!>||",

                BitOr(false) => "|",
                BitOr(true) => "!|",
                BitAnd(false) => "&",
                BitAnd(true) => "!&",
                BitXor(false) => ">|",
                BitXor(true) => "!>|",

                Add => "+",
                Sub => "-",

                Mul => "*",
                Div => "/",
                Mod => "%",

                Dot => "·",
                Cross => "><",
                Power => "^",

                Index => "index",
                App => "app",
            }
        )
    }
}
use BinaryOp::*;
impl BindingPow for BinaryOp {
    fn binding_pow(self) -> i8 {
        match self {
            Equation => 1, // a = b

            Write | OrAssign(..) | XorAssign(..) | AndAssign(..) | AddAssign | SubAssign
            | MulAssign | DivAssign | ModAssign | Swap => 2,

            // Comma => 3
            Or(..) => 4,
            Xor(..) => 5,
            And(..) => 6,

            // Smaller / Greater / SmallerOrEqual / GreaterOrEqual => 4.1
            // Equal / NotEqual => 4.2
            BitOr(..) => 8,
            BitXor(..) => 9,
            BitAnd(..) => 10,

            Add | Sub => 11,

            Mul | Div | Mod => 12,
            // Neg => 13
            // Pos => 13
            Dot | Cross | Power => 14,

            Index | App => 19,
        }
    }
}
