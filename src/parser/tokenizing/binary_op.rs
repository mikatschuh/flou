#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BinaryOp {
    Apply, // a(b) / a[b]

    Mul, // a * b
    Div, // a / b
    Mod, // a % b

    Add, // a + b
    Sub, // a - b

    BitwiseAnd, // a & b
    BitwiseOr,  // a | b
    BitwiseXor, // a >| b

    Or,   // a || b
    Nor,  // a !|| b
    And,  // a && b
    Nand, // a !&& b

    Write, // a := b

    MulAssign, // a *= b
    DivAssign, // a /= b
    ModAssign, // a %= b

    AddAssign, // a += b
    SubAssign, // a -= b

    BitwiseAndAssign, // a &= b
    BitwiseOrAssign,  // a |= b
    BitwiseXorAssign, // a >|= b

    Swap, // a =|= b

    Equation, // a = b
}
use std::fmt;
impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinaryOp::*;
        write!(
            f,
            "{}",
            match self {
                Apply => "app",

                Mul => "*",
                Div => "/",
                Mod => "%",

                Add => "+",
                Sub => "-",

                BitwiseOr => "|",
                BitwiseAnd => "&",
                BitwiseXor => ">|",

                Or => "||",
                Nor => "!||",
                And => "&&",
                Nand => "!&&",

                Write => ":=",

                MulAssign => "*=",
                DivAssign => "/=",
                ModAssign => "%=",

                AddAssign => "+=",
                SubAssign => "-=",

                BitwiseAndAssign => "&=",
                BitwiseOrAssign => "|=",
                BitwiseXorAssign => ">|=",

                Swap => "=|=",

                Equation => "=",
            }
        )
    }
}
use colored::Colorize;
use BinaryOp::*;
impl BinaryOp {
    pub(in crate::parser) fn binding_pow(self) -> f32 {
        match self {
            Apply => 7.0,

            Mul => 6.0,
            Div => 6.0,
            Mod => 6.0,

            Add => 5.0,
            Sub => 5.0,

            BitwiseOr => 4.0,
            BitwiseAnd => 4.0,
            BitwiseXor => 4.0,

            Or => 3.0,   // a || b
            Nor => 3.0,  // a !|| b
            And => 3.0,  // a && b
            Nand => 3.0, // a !&& b

            // Comma => 2.5
            Write => 2.0,

            MulAssign => 2.0,
            DivAssign => 2.0,
            ModAssign => 2.0,

            AddAssign => 2.0,
            SubAssign => 2.0,

            BitwiseAndAssign => 2.0,
            BitwiseOrAssign => 2.0,
            BitwiseXorAssign => 2.0,

            Swap => 2.0,

            Equation => 1.0, // a = b
        }
    }
}
