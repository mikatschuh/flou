pub trait BindingPow {
    fn binding_pow(self) -> f32;
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BinaryOp {
    Equation, // a = b

    Write, // a := b

    MulAssign, // a *= b
    DivAssign, // a /= b
    ModAssign, // a %= b

    AddAssign, // a += b
    SubAssign, // a -= b

    BitAndAssign, // a &= b
    BitOrAssign,  // a |= b
    BitXorAssign, // a >|= b

    Swap, // a =|= b

    Or,   // a || b
    Nor,  // a !|| b
    And,  // a && b
    Nand, // a !&& b

    BitAnd, // a & b
    BitOr,  // a | b
    BitXor, // a >| b

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

                MulAssign => "*=",
                DivAssign => "/=",
                ModAssign => "%=",

                AddAssign => "+=",
                SubAssign => "-=",

                BitAndAssign => "&=",
                BitOrAssign => "|=",
                BitXorAssign => ">|=",

                Swap => "=|=",

                Or => "||",
                Nor => "!||",
                And => "&&",
                Nand => "!&&",

                BitOr => "|",
                BitAnd => "&",
                BitXor => ">|",

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
    fn binding_pow(self) -> f32 {
        match self {
            Equation => 0.0, // a = b

            Write => 1.0,

            MulAssign => 1.0,
            DivAssign => 1.0,
            ModAssign => 1.0,

            AddAssign => 1.0,
            SubAssign => 1.0,

            BitAndAssign => 1.0,
            BitOrAssign => 1.0,
            BitXorAssign => 1.0,

            Swap => 1.0,

            // Comma => 2.0
            Or => 3.0,   // a || b
            Nor => 3.0,  // a !|| b
            And => 3.0,  // a && b
            Nand => 3.0, // a !&& b

            // Smaller / Greater / SmallerOrEqual / GreaterOrEqual => 4.1
            // Equal / NotEqual => 4.2
            BitOr => 5.1,
            BitAnd => 5.1,
            BitXor => 5.1,

            Add => 5.2,
            Sub => 5.2,

            Mul => 5.3,
            Div => 5.3,
            Mod => 5.3,
            // Neg => 5.4
            // Pos => 5.4
            Dot => 5.5,
            Cross => 5.5,
            Power => 5.5,

            // CustomOperators => 5.5
            Index => 10.0,
            App => 10.0,
        }
    }
}
