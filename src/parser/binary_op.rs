#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub enum BinaryOp {
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

    DotAssign,   // a ·= b
    CrossAssign, // a ><= b
    PowAssign,   // a ^= b

    Swap, // a =|= b

    Or,   // a || b
    Nor,  // a !|| b
    Xor,  // a >|| b
    Xnor, // a !>|| b
    And,  // a && b
    Nand, // a !&& b

    Equal,          // a == b
    NonEqual,       // a != b
    Smaller,        // a < b
    GreaterOrEqual, // a >= b
    Greater,        // a > b
    SmallerOrEqual, // a <= b

    BitShiftLeft,  // a << b
    BitShiftRight, // a >> b

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
    Pow,   // a ^ b

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
impl BinaryOp {
    pub fn as_str(self) -> &'static str {
        match self {
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

            DotAssign => "·=",
            CrossAssign => "><=",
            PowAssign => "^=",

            Swap => "=|=",

            Or => "||",
            Nor => "!||",
            Xor => ">||",
            Xnor => "!>||",
            And => "&&",
            Nand => "!&&",

            Equal => "==",
            NonEqual => "!=",
            Smaller => "<",
            GreaterOrEqual => ">=",
            Greater => ">",
            SmallerOrEqual => "<=",

            BitShiftLeft => "<<",
            BitShiftRight => ">>",

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
            Pow => "^",

            Index => "-[-",
            App => "-(-",
        }
    }

    pub fn is_chained(self) -> bool {
        matches!(
            self,
            Equal | NonEqual | Smaller | SmallerOrEqual | Greater | GreaterOrEqual,
        )
    }
}
