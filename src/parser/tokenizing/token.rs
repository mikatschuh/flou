use crate::{
    error::Span,
    parser::{binary_op::BinaryOp, keyword::Keyword, tree::Bracket, unary_op::UnaryOp},
};
use colored::{ColoredString, Colorize};
use std::fmt::Display;

#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub struct Token<'src> {
    pub span: Span,
    pub src: &'src str,
    pub kind: TokenKind,
}

#[derive(PartialEq, Debug, Clone, Copy, Eq)]
pub enum TokenKind {
    Not, // !

    Tick,  // '
    Equal, // =

    EqualEqual, // ==
    NotEqual,   // !=

    Left,          // <
    LeftLeft,      // <<
    LeftLeftEqual, // <<=
    NotLeft,       // !<
    LeftEqual,     // <=
    NotLeftEqual,  // !<=

    Right,           // >
    RightRight,      // >>
    RightRightEqual, // >>=
    NotRight,        // !>
    RightEqual,      // >=
    NotRightEqual,   // !>=
    RightArrow,      // ->

    Plus,      // +
    PlusPlus,  // ++
    PlusEqual, // +=
    Dash,      // -
    DashDash,  // --
    DashEqual, // -=

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
    Up,         // a (^)+ b
    UpEqual,    // a (^)+= b

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
        write!(f, "{}", self.src)
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
    pub fn new(c: char) -> Option<TokenKind> {
        Some(match c {
            '!' => Not,
            '\'' => Tick,
            '=' => Equal,
            '+' => Plus,
            '-' => Dash,
            '*' => Star,
            '/' => Slash,
            '%' => Percent,
            '·' => Dot,
            '^' => Up,
            '|' => Pipe,
            '&' => And,
            '<' => Left,
            '>' => Right,
            ':' => Colon,
            ',' => Comma,
            '(' => Open(Round),
            '[' => Open(Squared),
            '{' => Open(Curly),
            ')' => Closed(Round),
            ']' => Closed(Squared),
            '}' => Closed(Curly),
            _ => return None,
        })
    }
    pub fn add(self, c: char) -> Option<TokenKind> {
        // transformation table to make tokens out of their char components
        Some(match self {
            Not if c == '=' => NotEqual,
            NotEqual if c == '=' => NotEqual,

            Not if c == '|' => NotPipe,
            NotPipe if c == '|' => NotPipePipe,
            NotPipe if c == '=' => NotPipeEqual,

            Not if c == '&' => NotAnd,
            NotAnd if c == '&' => NotAndAnd,
            NotAnd if c == '=' => NotAndEqual,

            Not if c == '<' => NotLeft,
            NotLeft if c == '=' => NotLeftEqual,
            NotLeftEqual if c == '=' => NotLeftEqual,

            Not if c == '>' => NotRight,
            NotRight if c == '|' => NotRightPipe,
            NotRightPipe if c == '|' => NotRightPipePipe,
            NotRightPipe if c == '=' => NotRightPipeEqual,
            NotRight if c == '=' => NotRightEqual,
            NotRightEqual if c == '=' => NotRightEqual,

            Equal if c == '|' => EqualPipe,
            EqualPipe if c == '=' => SwapSign,
            Equal if c == '=' => EqualEqual,
            EqualEqual if c == '=' => EqualEqual,

            Left if c == '<' => LeftLeft,
            LeftLeft if c == '=' => LeftLeftEqual,
            Left if c == '=' => LeftEqual,
            LeftEqual if c == '=' => LeftEqual,

            Right if c == '>' => RightRight,
            RightRight if c == '=' => RightRightEqual,
            Right if c == '=' => RightEqual,
            Right if c == '|' => RightPipe,
            RightPipe if c == '|' => RightPipePipe,
            RightPipe if c == '=' => RightPipeEqual,
            Right if c == '<' => Cross,
            RightEqual if c == '=' => RightEqual,

            Plus if c == '+' => PlusPlus,
            Plus if c == '=' => PlusEqual,

            Dash if c == '-' => DashDash,
            Dash if c == '=' => DashEqual,
            Dash if c == '>' => RightArrow,

            Star if c == '=' => StarEqual,

            Slash if c == '=' => SlashEqual,

            Percent if c == '=' => PercentEqual,

            Dot if c == '=' => DotEqual,

            Cross if c == '=' => CrossEqual,

            Up if c == '^' => Up,
            Up if c == '=' => UpEqual,

            Pipe if c == '|' => PipePipe,
            Pipe if c == '=' => PipeEqual,

            And if c == '&' => AndAnd,
            And if c == '=' => AndEqual,

            Colon if c == '=' => ColonEqual,

            _ => return None,
        })
    }
    pub fn ends_with(self, c: char) -> bool {
        match c {
            '!' => matches!(self, Not),
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
            '-' => matches!(self, Dash | DashDash),
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
}
impl<'src> Token<'src> {
    pub fn as_prefix(self) -> Option<UnaryOp> {
        use UnaryOp::*;
        Some(match self.kind {
            Dash => Neg,
            TokenKind::Not => Not,
            RightArrow => Ptr,
            And => Ref,
            Ident if self.src == "mut" => Mut,
            _ => return None,
        })
    }

    pub fn as_infix(self) -> Option<BinaryOp> {
        use BinaryOp::*;
        match self.kind {
            EqualEqual => Some(Eq),
            NotEqual => Some(Ne),

            Left => Some(Smaller),
            LeftLeft => Some(Lsh),
            LeftLeftEqual => Some(LshAssign),
            NotLeft => Some(GreaterEq),
            LeftEqual => Some(SmallerEq),
            NotLeftEqual => Some(Greater),

            Right => Some(Greater),
            RightRight => Some(Rsh),
            RightRightEqual => Some(RshAssign),
            NotRight => Some(SmallerEq),
            RightEqual => Some(GreaterEq),
            NotRightEqual => Some(Smaller),

            Plus => Some(Add),
            PlusEqual => Some(AddAssign),
            Dash => Some(Sub),
            DashEqual => Some(SubAssign),

            Star => Some(Mul),
            StarEqual => Some(MulAssign),
            Slash => Some(Div),
            SlashEqual => Some(DivAssign),
            Percent => Some(Mod),
            PercentEqual => Some(ModAssign),

            TokenKind::Dot => Some(Dot),
            DotEqual => Some(DotAssign),
            TokenKind::Cross => Some(Cross),
            CrossEqual => Some(CrossAssign),
            Up => Some(Pow {
                grade: self.src.len() - 1,
            }),
            UpEqual => Some(PowAssign {
                grade: self.src.len() - 2,
            }), // -2 to account for the equal sign

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

            TokenKind::And => Some(BitAnd),
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
        match self.kind {
            PlusPlus => Some(Inc),
            DashDash => Some(Dec),
            TokenKind::Not => Some(Fac),
            _ => None,
        }
    }
}
