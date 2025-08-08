use std::fmt::Display;

use colored::{ColoredString, Colorize};

use crate::error::Span;

#[derive(PartialEq, Debug, Clone, Eq)]
pub struct Token<'src> {
    pub span: Span,
    pub src: &'src str,
    pub kind: TokenKind,
}

#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub enum TokenKind {
    Not,   // !
    Tick,  // '
    Equal, // =

    EqualEqual, // ==
    NotEqual,   // !=

    Left,         // <
    LeftLeft,     // <<
    NotLeft,      // !<
    LeftEqual,    // <=
    NotLeftEqual, // !<=
    LeftArrow,    // <-

    Right,         // >
    RightRight,    // >>
    NotRight,      // !>
    RightEqual,    // >=
    NotRightEqual, // !>=
    RightArrow,    // ->

    Plus,       // +
    PlusPlus,   // ++
    PlusEqual,  // +=
    Minus,      // -
    MinusMinus, // --
    MinusEqual, // -=

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

    Ident, // _
    Quote, // "_"

    OpenParen,   // (
    OpenBracket, // [
    OpenBrace,   // {

    ClosedParen,   // )
    ClosedBracket, // ]
    ClosedBrace,   // }
}
use TokenKind::*;

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.kind {
                Not => "!",
                Tick => "'",
                Equal => "=",

                EqualEqual => "==",
                NotEqual => "!=",

                Left => "<",
                LeftLeft => "<<",
                NotLeft => "!<",
                LeftEqual => "<=",
                NotLeftEqual => "!<=",
                LeftArrow => "<-",

                Right => ">",
                RightRight => ">>",
                NotRight => "!>",
                RightEqual => ">=",
                NotRightEqual => "!>=",
                RightArrow => "->",

                Plus => "+",
                PlusPlus => "++",
                PlusEqual => "+=",
                Minus => "-",
                MinusMinus => "--",
                MinusEqual => "-=",

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

                OpenParen => "(",
                OpenBracket => "[",
                OpenBrace => "{",

                ClosedParen => ")",
                ClosedBracket => "]",
                ClosedBrace => "}",
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
        format!("{}", self).bold()
    }
}
impl TokenKind {
    pub fn new(c: char) -> Option<TokenKind> {
        match c {
            '!' => Some(Not),
            '\'' => Some(Tick),
            '=' => Some(Equal),
            '+' => Some(Plus),
            '-' => Some(Minus),
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
            '(' => Some(OpenParen),
            '[' => Some(OpenBracket),
            '{' => Some(OpenBrace),
            ')' => Some(ClosedParen),
            ']' => Some(ClosedBracket),
            '}' => Some(ClosedBrace),
            _ => None,
        }
    }
    pub fn add(self, c: char) -> Option<TokenKind> {
        // transformation table to make tokens out of their char components
        match self {
            Not => match c {
                '=' => Some(NotEqual),
                '|' => Some(NotPipe),
                '&' => Some(NotAnd),
                '<' => Some(NotLeft),
                '>' => Some(NotRight),
                _ => None,
            },
            Equal => match c {
                '=' => Some(EqualEqual),
                '|' => Some(EqualPipe),
                _ => None,
            },
            Plus => match c {
                '+' => Some(PlusPlus),
                '=' => Some(PlusEqual),
                _ => None,
            },
            Minus => match c {
                '-' => Some(MinusMinus),
                '=' => Some(MinusEqual),
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
            Left => match c {
                '=' => Some(LeftEqual),
                '<' => Some(LeftLeft),
                '-' => Some(LeftArrow),
                _ => None,
            },
            NotLeft => match c {
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
                '=' => Some(NotRightEqual),
                '|' => Some(NotRightPipe),
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
            '!' => matches!(self, Not),
            '\'' => matches!(self, Tick),
            '=' => matches!(
                self,
                Equal
                    | PlusEqual
                    | MinusEqual
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
            '>' => matches!(self, RightArrow | Right | NotRight),
            '+' => matches!(self, Plus | PlusPlus),
            '-' => matches!(self, Minus | MinusMinus),
            '*' => self == Star,
            '/' => self == Slash,
            '%' => self == Percent,
            '·' => self == Dot,
            '<' => matches!(self, Cross | Left | NotLeft),
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
            '(' => self == OpenParen,
            '[' => self == OpenBracket,
            '{' => self == OpenBrace,
            ')' => self == ClosedParen,
            ']' => self == ClosedBracket,
            '}' => self == ClosedBrace,
            _ => false,
        }
    }
}
