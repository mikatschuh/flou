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

pub(super) type BindingPow = usize;

pub(super) const STATEMENT: BindingPow = 1;
pub(super) const LABEL: BindingPow = 2;
pub(super) const PATH: BindingPow = 2;
pub(super) const COLON: BindingPow = 4;
pub(super) const BINDING: BindingPow = 3;
pub(super) const SINGLE_VALUE: BindingPow = 124;

impl<'src> Token<'src> {
    pub const fn binding_pow(self) -> Option<BindingPow> {
        Some(match self.kind {
            Closed(..) | Comma | Keyword(Else | Continue | Break | Return) => return None,

            Tick | RightArrow | Ident | Quote | Keyword(If | Loop | Proc) | Open(Curly) => 0,

            Equal | EqualPipe => 2,

            Colon => 5,

            ColonEqual | LeftLeftEqual | RightRightEqual | PipeEqual | NotPipeEqual
            | RightPipeEqual | NotRightPipeEqual | AndEqual | NotAndEqual | PlusEqual
            | DashEqual | StarEqual | SlashEqual | PercentEqual | DotEqual | CrossEqual
            | UpEqual | SwapSign | PlusPlus | DashDash => 10,

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

            Up => 121 + ((self.src.len() - 1) << 1),

            Open(Squared | Round) => usize::MAX,
        })
    }
}

impl UnaryOp {
    pub const fn binding_pow(self) -> BindingPow {
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
    pub const fn binding_pow(self) -> BindingPow {
        use BinaryOp::*;
        match self {
            Index | App => 0,

            Write
            | LshAssign
            | RshAssign
            | OrAssign
            | NorAssign
            | XorAssign
            | XnorAssign
            | AndAssign
            | NandAssign
            | AddAssign
            | SubAssign
            | MulAssign
            | DivAssign
            | ModAssign
            | DotAssign
            | CrossAssign
            | PowAssign { .. }
            | Swap => 11,

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

            Pow { grade } => 120 + (grade << 1),
        }
    }
}
