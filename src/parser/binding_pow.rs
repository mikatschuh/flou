use crate::parser::{binary_op::BinaryOp, unary_op::UnaryOp};

impl BinaryOp {
    pub fn binding_pow(self) -> (u8, u8) {
        use BinaryOp::*;
        match self {
            Equation => (2, 3),

            Write | OrAssign | NorAssign | XorAssign | XnorAssign | AndAssign | NandAssign
            | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | DotAssign
            | CrossAssign | PowerAssign | Swap => (4, 5),

            // Comma => 6
            Or | Nor => (7, 8),
            Xor | Xnor => (9, 10),
            And | Nand => (11, 12),

            Equal | NonEqual | Smaller | SmallerOrEqual | Greater | GreaterOrEqual => (13, 13),

            BitShiftLeft => (14, 15),
            BitShiftRight => (16, 17),

            BitOr | BitNor => (18, 19),
            BitXor | BitXnor => (20, 21),
            BitAnd | BitNand => (22, 23),

            Add | Sub => (24, 25),

            Mul | Div | Mod => (26, 27),
            // Neg => 15
            Dot | Cross | Power => (28, 29),

            Index | App => (33, 35),
        }
    }
}
impl UnaryOp {
    pub fn binding_pow(self) -> u8 {
        use UnaryOp::*;
        match self {
            Neg => 27,

            Ref | Deref | Not => 32,

            Increment | Decrement => 4,

            LfT => 34,
        }
    }
}
