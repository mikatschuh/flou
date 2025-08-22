use crate::{
    parser::{
        binary_op::BinaryOp,
        tokenizing::token::{Token, TokenKind},
        unary_op::UnaryOp,
    },
    tree::Bracket,
};

use Bracket::*;
use TokenKind::*;

pub(super) const STATEMENT: u8 = 3;
pub(super) const COLON: u8 = 5;
pub(super) const BINDING: u8 = 7;
pub(super) const SINGLE_VALUE: u8 = 124;

pub(super) const fn comma(in_brackets: bool) -> u8 {
    match in_brackets {
        true => 1,
        false => 16,
    }
}

impl<'src> Token<'src> {
    pub const fn binding_pow(self, comma_override: bool) -> u8 {
        match self.kind {
            Closed(..) | EqualPipe => 0,

            Not | NotNot | Tick | RightArrow | Ident | Quote | Keyword(..) | Open(Curly) => 2,

            Colon => 4,

            Equal => 6,

            ColonEqual | PipeEqual | NotPipeEqual | RightPipeEqual | NotRightPipeEqual
            | AndEqual | NotAndEqual | PlusEqual | DashEqual | StarEqual | SlashEqual
            | PercentEqual | DotEqual | CrossEqual | UpEqual | SwapSign | PlusPlus => 10,

            Comma if comma_override => 0,
            Comma => 15,

            PipePipe | NotPipePipe => 20,
            RightPipePipe | NotRightPipePipe => 30,
            AndAnd | NotAndAnd => 40,

            EqualEqual | NotEqual | Left | NotLeft | LeftEqual | NotLeftEqual | Right
            | NotRight | RightEqual | NotRightEqual => 50,

            LeftLeft | RightRight => 60,

            Pipe | NotPipe => 70,
            RightPipe | NotRightPipe => 80,
            And | NotAnd => 90,

            Plus | Dash(..) => 100,

            Star | Slash | Percent | Dot | Cross => 110,

            Up => 121,

            Open(Squared | Round) => 130,
        }
    }
}

impl UnaryOp {
    pub const fn binding_pow(self) -> u8 {
        use UnaryOp::*;
        match self {
            Inc | Dec => 10,
            Neg => 115,
            Not => 125,
        }
    }
}

impl BinaryOp {
    pub const fn binding_pow(self) -> u8 {
        use BinaryOp::*;
        match self {
            Index | App => 0,

            Write | OrAssign | NorAssign | XorAssign | XnorAssign | AndAssign | NandAssign
            | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign | DotAssign
            | CrossAssign | PowAssign | Swap => 11,

            Or | Nor => 21,
            Xor | Xnor => 31,
            And | Nand => 41,

            Eq | Ne | Smaller | SmallerEq | Greater | GreaterEq => 51,

            BitShiftLeft | BitShiftRight => 61,

            BitOr | BitNor => 71,
            BitXor | BitXnor => 81,
            BitAnd | BitNand => 91,

            Add | Sub => 101,

            Mul | Div | Mod | Dot | Cross => 111,

            Pow => 120,
        }
    }
}
