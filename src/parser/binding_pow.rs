use crate::parser::{
    binary_op::BinaryOp,
    keyword::Keyword,
    tokenizing::token::{Token, TokenKind},
    tree::Bracket,
    unary_op::UnaryOp,
};

use Bracket::*;
use Keyword::*;
use TokenKind::*;

pub(super) const STATEMENT: u8 = 1;
pub(super) const COLON: u8 = 4;
pub(super) const BINDING: u8 = 3;
pub(super) const SINGLE_VALUE: u8 = 124;

impl<'src> Token<'src> {
    pub const fn binding_pow(self) -> Option<u8> {
        Some(match self.kind {
            Closed(..) | Comma | Keyword(Else | Continue | Break | Return) => return None,

            Tick | RightArrow | Ident | Quote | Keyword(If | Loop) | Open(Curly) => 0,

            Equal | EqualPipe => 2,

            Colon => 5,

            ColonEqual | PipeEqual | NotPipeEqual | RightPipeEqual | NotRightPipeEqual
            | AndEqual | NotAndEqual | PlusEqual | DashEqual | StarEqual | SlashEqual
            | PercentEqual | DotEqual | CrossEqual | UpEqual | SwapSign | PlusPlus | DashDash => 10,

            PipePipe | NotPipePipe => 20,
            RightPipePipe | NotRightPipePipe => 30,
            AndAnd | NotAndAnd => 40,

            EqualEqual | NotEqual | Left | NotLeft | LeftEqual | NotLeftEqual | Right
            | NotRight | RightEqual | NotRightEqual => 50,

            LeftLeft | RightRight => 60,

            Pipe | NotPipe => 70,
            RightPipe | NotRightPipe => 80,
            And | NotAnd => 90,

            Plus | Dash => 100,

            Star | Slash | Percent | Dot | Cross => 110,

            Not => 115,

            Up => 121,

            Open(Squared | Round) => 130,
        })
    }
}

impl UnaryOp {
    pub const fn binding_pow(self) -> u8 {
        use UnaryOp::*;
        match self {
            Inc | Dec => 0,
            Fac => 0,

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

            Lsh | Rsh => 61,

            BitOr | BitNor => 71,
            BitXor | BitXnor => 81,
            BitAnd | BitNand => 91,

            Add | Sub => 101,

            Mul | Div | Mod | Dot | Cross => 111,

            Pow => 120,
        }
    }
}
