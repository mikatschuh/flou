pub trait BindingPow {
    fn binding_pow(self) -> i8;
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BinaryOp {
    Equation, // a = b

    Write, // a := b

    OrAssign,   // a |= b
    NorAssign,  // a !|= b
    XorAssign,  // a >|= b
    XnorAssign, // a !>|= b
    AndAssign,  // a &= b
    NandAssign, // a !&= b

    AddAssign, // a += b
    SubAssign, // a -= b

    MulAssign, // a *= b
    DivAssign, // a /= b
    ModAssign, // a %= b

    Swap, // a =|= b

    Or,   // a || b
    Nor,  // a !|| b
    Xor,  // a >|| b
    Xnor, // a !>|| b
    And,  // a && b
    Nand, // a !&& b

    BitOr,   // a | b
    BitNor,  // a !| b
    BitXor,  // a >| b
    BitXnor, // a !>| b
    BitAnd,  // a & b
    BitNand, // a !& b

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
        write!(f, "{}", self.as_str())
    }
}
use BinaryOp::*;
impl BindingPow for BinaryOp {
    fn binding_pow(self) -> i8 {
        match self {
            Equation => 1, // a = b

            Write | OrAssign | NorAssign | XorAssign | XnorAssign | AndAssign | NandAssign
            | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | Swap => 2,

            // Comma => 3
            Or | Nor => 4,
            Xor | Xnor => 5,
            And | Nand => 6,

            // Smaller / Greater / SmallerOrEqual / GreaterOrEqual => 4.1
            // Equal / NotEqual => 4.2
            BitOr | BitNor => 8,
            BitXor | BitXnor => 9,
            BitAnd | BitNand => 10,

            Add | Sub => 11,

            Mul | Div | Mod => 12,
            // Neg => 13
            // Pos => 13
            Dot | Cross | Power => 14,

            Index | App => 19,
        }
    }
}
impl BinaryOp {
    pub fn as_str(self) -> &'static str {
        match self {
            Equation => "=",

            Write => ":=",

            OrAssign => "|=",
            NorAssign => "!|=",
            XorAssign => ">|=",
            XnorAssign => "!>|=",
            AndAssign => "&=",
            NandAssign => "!&=",

            AddAssign => "+=",
            SubAssign => "-=",

            MulAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",

            Swap => "=|=",

            Or => "||",
            Nor => "!||",
            Xor => ">||",
            Xnor => "!>||",
            And => "&&",
            Nand => "!&&",

            BitOr => "|",
            BitNor => "!|",
            BitXor => ">|",
            BitXnor => "!>|",
            BitAnd => "&",
            BitNand => "!&",

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
    }
}
