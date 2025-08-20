use crate::{
    parser::{
        tokenizing::token::{Token, TokenKind},
        unary_op::UnaryOp,
    },
    tree::Bracket,
};

use Bracket::*;
use TokenKind::*;

pub(super) const SINGLE_VALUE: u8 = 124;
pub(super) const EXPONENT: u8 = 121;

impl<'src> Token<'src> {
    pub fn binding_pow(self) -> u8 {
        match self.kind {
            Closed(..) | EqualPipe => 0,

            Not | NotNot | Tick | RightArrow | Ident | Quote | Keyword(..) | Open(Curly) => 2,

            Colon => 4,

            Equal => 6,

            ColonEqual | PipeEqual | NotPipeEqual | RightPipeEqual | NotRightPipeEqual
            | AndEqual | NotAndEqual | PlusEqual | DashEqual | StarEqual | SlashEqual
            | PercentEqual | DotEqual | CrossEqual | UpEqual | SwapSign | PlusPlus => 10,

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

            LeftArrow => 124,

            Open(Squared | Round) => 130,
        }
    }
    pub fn right_bp(self) -> u8 {
        match self.kind {
            Closed(..) | Open(Squared | Round) | EqualPipe | LeftArrow => 0,

            Not | NotNot | Tick | RightArrow | Ident | Quote | Keyword(..) | Open(Curly) => 3,

            Colon => 5,

            Equal => 7,

            ColonEqual | PipeEqual | NotPipeEqual | RightPipeEqual | NotRightPipeEqual
            | AndEqual | NotAndEqual | PlusEqual | DashEqual | StarEqual | SlashEqual
            | PercentEqual | DotEqual | CrossEqual | UpEqual | SwapSign | PlusPlus => 11,

            Comma => 16,

            PipePipe | NotPipePipe => 21,
            RightPipePipe | NotRightPipePipe => 31,
            AndAnd | NotAndAnd => 41,

            EqualEqual | NotEqual | Left | NotLeft | LeftEqual | NotLeftEqual | Right
            | NotRight | RightEqual | NotRightEqual => 51,

            LeftLeft | RightRight => 61,

            Pipe | NotPipe => 71,
            RightPipe | NotRightPipe => 81,
            And | NotAnd => 91,

            Plus | Dash(..) => 101,

            Star | Slash | Percent | Dot | Cross => 111,

            Up => 120,
        }
    }
}
impl UnaryOp {
    pub fn bp(self) -> u8 {
        use UnaryOp::*;
        match self {
            Neg | Pos => 115,
            Ref | Not => 125,

            Increment | Decrement => 10,
        }
    }
}
