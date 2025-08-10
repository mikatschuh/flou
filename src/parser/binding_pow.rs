use crate::parser::{binary_op::BinaryOp, unary_op::UnaryOp};

impl BinaryOp {
    pub fn binding_pow(self) -> (u8, u8) {
        use BinaryOp::*;
        match self {
            Equation => (2, 3),

            Write | OrAssign | NorAssign | XorAssign | XnorAssign | AndAssign | NandAssign
            | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | DotAssign
            | CrossAssign | PowerAssign | Swap => (4, 5),

            // Comma => (6, 7)
            Or | Nor => (8, 9),
            Xor | Xnor => (10, 11),
            And | Nand => (12, 13),

            Equal | NonEqual | Smaller | SmallerOrEqual | Greater | GreaterOrEqual => (14, 15),

            BitShiftLeft | BitShiftRight => (16, 17),

            BitOr | BitNor => (19, 20),
            BitXor | BitXnor => (21, 22),
            BitAnd | BitNand => (23, 24),

            Add | Sub => (25, 26),

            Mul | Div | Mod | Dot | Cross => (26, 28),
            // Neg => 15
            Power => (29, 30),

            Index | App => (33, 34),
        }
    }
}
impl UnaryOp {
    pub fn binding_pow(self) -> u8 {
        use UnaryOp::*;
        match self {
            Neg => 28,

            Ref | Deref | Not => 32,

            Increment | Decrement => 4,

            LfT => 34,
        }
    }
}
