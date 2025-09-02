use crate::{
    error::Span,
    parser::{binary_op::BinaryOp, keyword::Keyword, tree::Bracket, unary_op::UnaryOp},
};
use colored::{ColoredString, Colorize};
use num::Integer;
use std::{fmt::Display, num::NonZeroU32};

#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub struct Token<'src> {
    pub span: Span,
    pub src: &'src str,
    pub kind: TokenKind,
}

#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub enum TokenKind {
    Not,    // !
    NotNot, // !!
    Tick,   // '
    Equal,  // =

    EqualEqual, // ==
    NotEqual,   // !=

    Left,         // <
    LeftLeft,     // <<
    NotLeft,      // !<
    LeftEqual,    // <=
    NotLeftEqual, // !<=

    Right,         // >
    RightRight,    // >>
    NotRight,      // !>
    RightEqual,    // >=
    NotRightEqual, // !>=
    RightArrow,    // ->

    Plus,             // +
    PlusPlus,         // ++
    PlusEqual,        // +=
    Dash(NonZeroU32), // (-)+
    DashEqual,        // -=

    Star,         // *
    StarEqual,    // *=
    Slash,        // /
    SlashEqual,   // /=
    Percent,      // %
    PercentEqual, // %=

    Dot,        // a · b
    DotEqual,   // a ·= b
    Cross,      // a >< b
    CrossEqual, // a ><= b
    Up,         // a ^ b
    UpEqual,    // a ^= b

    Pipe,         // |
    PipePipe,     // ||
    NotPipe,      // !|
    NotPipePipe,  // !||
    PipeEqual,    // |=
    NotPipeEqual, // !|=

    RightPipe,         // >|
    RightPipePipe,     // >||
    NotRightPipe,      // !>|
    NotRightPipePipe,  // !>||
    RightPipeEqual,    // >|=
    NotRightPipeEqual, // !>|=

    And,         // &
    AndAnd,      // &&
    NotAnd,      // !&
    NotAndAnd,   // !&&
    AndEqual,    // &=
    NotAndEqual, // !&=

    Colon,      // :
    ColonEqual, // :=
    EqualPipe,  // =|
    SwapSign,   // =|=

    Comma, // ,

    Ident,            // _
    Quote,            // "_"
    Keyword(Keyword), // if / loop / ..

    Open(Bracket),   // ( / [ / {
    Closed(Bracket), // ) / ] / }
}
use Bracket::*;
use TokenKind::*;

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.kind {
                Not => "!",
                NotNot => "!!",
                Tick => "'",
                Equal => "=",

                EqualEqual => "==",
                NotEqual => "!=",

                Left => "<",
                LeftLeft => "<<",
                NotLeft => "!<",
                LeftEqual => "<=",
                NotLeftEqual => "!<=",

                Right => ">",
                RightRight => ">>",
                NotRight => "!>",
                RightEqual => ">=",
                NotRightEqual => "!>=",
                RightArrow => "->",

                Plus => "+",
                PlusPlus => "++",
                PlusEqual => "+=",
                Dash(count) =>
                    return write!(f, "{}", (0..count.get()).map(|_| " ").collect::<String>()),
                DashEqual => "-=",

                Star => "*",
                StarEqual => "*=",
                Slash => "/",
                SlashEqual => "/=",
                Percent => "%",
                PercentEqual => "%=",

                Dot => "·",
                DotEqual => "·=",
                Cross => "><",
                CrossEqual => "><=",
                Up => "^",
                UpEqual => "^=",

                Pipe => "|",
                PipePipe => "||",
                NotPipe => "!|",
                NotPipePipe => "!||",
                PipeEqual => "|=",
                NotPipeEqual => "!|=",

                RightPipe => ">|",
                RightPipePipe => ">||",
                NotRightPipe => "!>|",
                NotRightPipePipe => "!>||",
                RightPipeEqual => ">|=",
                NotRightPipeEqual => "!>|=",

                And => "&",
                AndAnd => "&&",
                NotAnd => "!&",
                NotAndAnd => "!&&",
                AndEqual => "&=",
                NotAndEqual => "!&=",

                Colon => ":",
                ColonEqual => ":=",
                EqualPipe => "=|",
                SwapSign => "=|=",

                Comma => ",",

                Ident => self.src,
                Quote => self.src,
                Keyword(keyword) => keyword.display(),

                Open(bracket) => bracket.display_open(),

                Closed(bracket) => bracket.display_closed(),
            }
        )
    }
}

impl<'a> Token<'a> {
    #[inline]
    pub fn new(span: Span, src: &'a str, kind: TokenKind) -> Self {
        Self { span, src, kind }
    }
    #[inline]
    pub fn bold(&self) -> ColoredString {
        self.to_string().bold()
    }
}

impl TokenKind {
    pub const DASH_DASH: Self = Self::Dash(unsafe { NonZeroU32::new_unchecked(2) });
    pub const DASH: Self = Self::Dash(unsafe { NonZeroU32::new_unchecked(1) });

    pub fn new(c: char) -> Option<TokenKind> {
        match c {
            '!' => Some(Not),
            '\'' => Some(Tick),
            '=' => Some(Equal),
            '+' => Some(Plus),
            '-' => Some(Self::DASH),
            '*' => Some(Star),
            '/' => Some(Slash),
            '%' => Some(Percent),
            '·' => Some(Dot),
            '^' => Some(Up),
            '|' => Some(Pipe),
            '&' => Some(And),
            '<' => Some(Left),
            '>' => Some(Right),
            ':' => Some(Colon),
            ',' => Some(Comma),
            '(' => Some(Open(Round)),
            '[' => Some(Open(Squared)),
            '{' => Some(Open(Curly)),
            ')' => Some(Closed(Round)),
            ']' => Some(Closed(Squared)),
            '}' => Some(Closed(Curly)),
            _ => None,
        }
    }
    pub fn add(self, c: char) -> Option<TokenKind> {
        // transformation table to make tokens out of their char components
        match self {
            Not => match c {
                '!' => Some(NotNot),
                '=' => Some(NotEqual),
                '|' => Some(NotPipe),
                '&' => Some(NotAnd),
                '<' => Some(NotLeft),
                '>' => Some(NotRight),
                _ => None,
            },
            NotNot => match c {
                '!' => Some(Not),
                '=' => Some(EqualEqual),
                '|' => Some(Pipe),
                '&' => Some(And),
                '<' => Some(Left),
                '>' => Some(Right),
                _ => None,
            },
            Equal => match c {
                '|' => Some(EqualPipe),
                '=' => Some(EqualEqual),
                _ => None,
            },
            EqualEqual => match c {
                '=' => Some(EqualEqual),
                _ => None,
            },
            Left => match c {
                '<' => Some(LeftLeft),
                '=' => Some(LeftEqual),
                _ => None,
            },
            NotLeft => match c {
                '=' => Some(NotLeftEqual),
                _ => None,
            },
            LeftEqual => match c {
                '=' => Some(LeftEqual),
                _ => None,
            },
            NotLeftEqual => match c {
                '=' => Some(NotLeftEqual),
                _ => None,
            },
            Right => match c {
                '=' => Some(RightEqual),
                '>' => Some(RightRight),
                '|' => Some(RightPipe),
                '<' => Some(Cross),
                _ => None,
            },
            NotRight => match c {
                '|' => Some(NotRightPipe),
                '=' => Some(NotRightEqual),
                _ => None,
            },
            RightEqual => match c {
                '=' => Some(RightEqual),
                _ => None,
            },
            NotRightEqual => match c {
                '=' => Some(NotRightEqual),
                _ => None,
            },
            Plus => match c {
                '+' => Some(PlusPlus),
                '=' => Some(PlusEqual),
                _ => None,
            },
            Dash(count) => match c {
                '-' => Some(Dash(count.checked_add(1)?)),
                '=' if count.get().is_odd() => Some(DashEqual),
                '=' => Some(PlusEqual),
                '>' => Some(RightArrow),
                _ => None,
            },
            Star => match c {
                '=' => Some(StarEqual),
                _ => None,
            },
            Slash => match c {
                '=' => Some(SlashEqual),
                _ => None,
            },
            Percent => match c {
                '=' => Some(PercentEqual),
                _ => None,
            },
            Dot => match c {
                '=' => Some(DotEqual),
                _ => None,
            },
            Cross => match c {
                '=' => Some(CrossEqual),
                _ => None,
            },
            Up => match c {
                '=' => Some(UpEqual),
                _ => None,
            },
            Pipe => match c {
                '|' => Some(PipePipe),
                '=' => Some(PipeEqual),
                _ => None,
            },
            NotPipe => match c {
                '|' => Some(NotPipePipe),
                '=' => Some(NotPipeEqual),
                _ => None,
            },
            RightPipe => match c {
                '|' => Some(RightPipePipe),
                '=' => Some(RightPipeEqual),
                _ => None,
            },
            NotRightPipe => match c {
                '|' => Some(NotRightPipePipe),
                '=' => Some(NotRightPipeEqual),
                _ => None,
            },
            And => match c {
                '&' => Some(AndAnd),
                '=' => Some(AndEqual),
                _ => None,
            },
            NotAnd => match c {
                '&' => Some(NotAndAnd),
                '=' => Some(NotAndEqual),
                _ => None,
            },
            Colon => match c {
                '=' => Some(ColonEqual),
                _ => None,
            },
            EqualPipe => match c {
                '=' => Some(SwapSign),
                _ => None,
            },
            _ => None,
        }
    }
    pub fn ends_with(self, c: char) -> bool {
        match c {
            '!' => matches!(self, Not | NotNot),
            '\'' => matches!(self, Tick),
            '=' => matches!(
                self,
                Equal
                    | PlusEqual
                    | DashEqual
                    | StarEqual
                    | SlashEqual
                    | PercentEqual
                    | DotEqual
                    | CrossEqual
                    | UpEqual
                    | PipeEqual
                    | NotPipeEqual
                    | RightPipeEqual
                    | NotRightPipeEqual
                    | AndAnd
                    | NotAndEqual
                    | EqualEqual
                    | NotEqual
                    | LeftEqual
                    | NotLeftEqual
                    | RightEqual
                    | NotRightEqual
                    | ColonEqual
                    | SwapSign
            ),
            '>' => matches!(self, RightArrow | Right | RightRight | NotRight),
            '+' => matches!(self, Plus | PlusPlus),
            '-' => matches!(self, Dash(..)),
            '*' => self == Star,
            '/' => self == Slash,
            '%' => self == Percent,
            '·' => self == Dot,
            '<' => matches!(self, Cross | Left | LeftLeft | NotLeft),
            '^' => self == Up,
            '|' => matches!(
                self,
                Pipe | NotPipe
                    | PipePipe
                    | NotPipePipe
                    | RightPipe
                    | NotRightPipe
                    | RightPipePipe
                    | NotRightPipePipe
                    | EqualPipe
            ),
            '&' => matches!(self, And | NotAnd | AndAnd | NotAndAnd),
            ':' => self == Colon,
            ',' => self == Comma,
            '(' => self == Open(Round),
            '[' => self == Open(Squared),
            '{' => self == Open(Curly),
            ')' => self == Closed(Round),
            ']' => self == Closed(Squared),
            '}' => self == Closed(Curly),
            _ => false,
        }
    }

    pub fn as_prefix(self) -> Option<UnaryOp> {
        use UnaryOp::*;
        match self {
            Self::Not => Some(Not),
            _ => None,
        }
    }

    pub fn as_infix(self) -> Option<BinaryOp> {
        use BinaryOp::*;
        match self {
            EqualEqual => Some(Eq),
            NotEqual => Some(Ne),

            Left => Some(Smaller),
            LeftLeft => Some(Lsh),
            NotLeft => Some(GreaterEq),
            LeftEqual => Some(SmallerEq),
            NotLeftEqual => Some(Greater),

            Right => Some(Greater),
            RightRight => Some(Rsh),
            NotRight => Some(SmallerEq),
            RightEqual => Some(GreaterEq),
            NotRightEqual => Some(Smaller),

            Plus => Some(Add),
            PlusEqual => Some(AddAssign),
            DashEqual => Some(SubAssign),

            Star => Some(Mul),
            StarEqual => Some(MulAssign),
            Slash => Some(Div),
            SlashEqual => Some(DivAssign),
            Percent => Some(Mod),
            PercentEqual => Some(ModAssign),

            Self::Dot => Some(Dot),
            DotEqual => Some(DotAssign),
            Self::Cross => Some(Cross),
            CrossEqual => Some(CrossAssign),
            Up => Some(Pow),
            UpEqual => Some(PowAssign),

            Pipe => Some(BitOr),
            PipePipe => Some(Or),
            NotPipe => Some(BitNor),
            NotPipePipe => Some(Nor),
            PipeEqual => Some(OrAssign),
            NotPipeEqual => Some(NorAssign),

            RightPipe => Some(BitXor),
            RightPipePipe => Some(Xor),
            NotRightPipe => Some(BitXnor),
            NotRightPipePipe => Some(Xnor),
            RightPipeEqual => Some(XorAssign),
            NotRightPipeEqual => Some(XnorAssign),

            Self::And => Some(BitAnd),
            AndAnd => Some(And),
            NotAnd => Some(BitNand),
            NotAndAnd => Some(Nand),
            AndEqual => Some(AndAssign),
            NotAndEqual => Some(NandAssign),

            ColonEqual => Some(Write),
            SwapSign => Some(Swap),

            _ => None,
        }
    }

    pub fn as_postfix(self) -> Option<UnaryOp> {
        use UnaryOp::*;
        match self {
            PlusPlus => Some(Inc),
            _ => None,
        }
    }
}
