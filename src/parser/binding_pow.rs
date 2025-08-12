use crate::parser::{binary_op::BinaryOp, unary_op::UnaryOp};

impl BinaryOp {
    pub fn binding_pow(self) -> (u8, u8) {
        use BinaryOp::*;
        match self {
            // Inside of Brackets Comma => (0, 1)
            // Statements => (2, 3)
            // Colon Statements => (4, 5)
            Equation => (6, 7),

            Write | OrAssign | NorAssign | XorAssign | XnorAssign | AndAssign | NandAssign
            | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | DotAssign
            | CrossAssign | PowerAssign | Swap => (10, 11),

            // Comma => (8, 7)
            Or | Nor => (20, 21),
            Xor | Xnor => (30, 31),
            And | Nand => (40, 41),

            Equal | NonEqual | Smaller | SmallerOrEqual | Greater | GreaterOrEqual => (50, 51),

            BitShiftLeft | BitShiftRight => (60, 61),

            BitOr | BitNor => (70, 71),
            BitXor | BitXnor => (80, 81),
            BitAnd | BitNand => (90, 91),

            Add | Sub => (100, 101),

            Mul | Div | Mod | Dot | Cross => (110, 111),
            // Neg => 15
            Power => (121, 120),

            Index | App => (130, 131),
        }
    }
}
impl UnaryOp {
    pub fn binding_pow(self) -> u8 {
        use UnaryOp::*;
        match self {
            Neg => 115,

            Ref | Deref | Not => 125,

            Increment | Decrement => 10,

            LfT => u8::MAX,
        }
    }
}
