use colored::*;
use std::any::Any;
use std::error;
use std::fmt;
use std::ops::Add;
use std::ops::AddAssign;
use std::ops::Sub;
use std::ops::SubAssign;

#[derive(Clone, Debug, PartialEq)]
pub struct Errors<'src> {
    file: &'src Path,
    errors: Vec<Error<'src>>,
}

impl<'src> Errors<'src> {
    pub fn new(path: &'src Path, pos: Span, error: ErrorCode<'src>) -> Self {
        Self {
            file: path,
            errors: vec![Error::new(pos, error)],
        }
    }
    pub fn empty(path: &'src Path) -> Self {
        Self {
            file: path,
            errors: Vec::new(),
        }
    }
    pub fn push(&mut self, pos: Span, error: ErrorCode<'src>) {
        self.errors.push(Error::new(pos, error))
    }
    pub fn concat(&mut self, other: Errors<'src>) {
        self.errors.extend(other.errors.iter().cloned());
    }
}
impl fmt::Display for Errors<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut string = String::new();
        for error in self.errors.iter() {
            string += &format!("{}\n", error.to_string(self.file));
        }
        write!(f, "{string}")
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct Error<'src> {
    section: Span,
    error: ErrorCode<'src>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode<'src> {
    ExpectedValue,
    ExpectedIdent,
    IdentWithJustDot,

    NoClosingQuotes { quote: &'src str },
    // control structure mistakes
    LonelyElse,

    JumpInsideFuncArg { keyword: &'src str },
    CodeAfterJump,

    // bracket errors
    NoOpenedBracket { closed: Bracket },
    NoClosedBracket { opened: Bracket },
    WrongClosedBracket { expected: Bracket, found: Bracket },
}
impl<'a> Error<'a> {
    pub fn new(pos: Span, error: ErrorCode<'a>) -> Self {
        Self {
            section: pos,
            error,
        }
    }
}

static ERROR: std::sync::LazyLock<String> =
    std::sync::LazyLock::new(|| format!("{}{}", "ERROR".bold().red(), &":".bold()));
macro_rules! format_error {
    // version without tip
    ($pos:expr, $msg:expr, [$($arg:expr),*]) => {
        format!(
            "\n{}\t{} {}\n",
            *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
            $pos
        )
    };
    // version without tip and without arguments
    ($pos:expr, $msg:expr) => {
        format!(
            "\n{}\t{} {}\n",
            *ERROR,
            $msg,
            $pos
        )
    };

    // version with tip
    ($pos:expr, $msg:expr, [$($arg:expr),*], $tip:expr, [$($tip_arg:expr),*]) => {
        format!(
            "\n{}\t{} {}\n\
             {}\t{}\n",
             *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
            $pos,
            "Tip".bold(),
            format!($tip, $(format!(" {} ", $tip_arg.bold())),*)
        )
    };
    // version with tip but without arguments
    ($pos:expr, $msg:expr, $tip:expr) => {
        format!(
            "\n{}\t{} {}\n\
             {}\t{}\n",
             *ERROR,
            $msg,
            $pos,
            "Tip".bold(),
            $tip
        )
    };
    // version with tip but without arguments for tip
    ($pos:expr, $msg:expr, [$($arg:expr),*], $tip:expr) => {
        format!(
            "\n{}\t{} {}\n\
             {}\t{}\n",
             *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
            $pos,
            "Tip".bold(),
            $tip
        )
    };
    // version without tip and without position
    ($msg:expr, [$($arg:expr),*]) => {
        format!(
            "\n{}\t{}\n",
            *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
        )
    };
    // version without tip and without position and without arguments
    ($msg:expr) => {
        format!(
            "\n{}\t{}\n",
            *ERROR,
            $msg,
        )
    };

    // version with tip and without position
    ($msg:expr, [$($arg:expr),*], $tip:expr, [$($tip_arg:expr),*]) => {
        format!(
            "\n{}\t{}\n\
             {}\t{}\n",
             *ERROR,
            format!($msg, $(format!(" {} ", $arg.bold())),*),
            "Tip".bold(),
            format!($tip, $(format!(" {} ", $tip_arg.bold())),*)
        )
    };
}
macro_rules! concat_display {
    ( $( $arg:expr ),* ) => {{
        let mut s = String::new();
        $(
            write!(&mut s, "{}", $arg).unwrap();
        )*
        s
    }};
}
#[macro_export]
macro_rules! format_error_quote_arg {
    () => {
        format!(" \"\" ").bold()
    };
    ($($es:expr),+) => {{
        use std::fmt::Write;
        format!(" \"{}\" ", concat_display!{ $($es),+ }).bold()
    }};
}
#[macro_export]
macro_rules! format_error_arg {
    () => {
        format!("  ").bold()
    };
    ($($es:expr),+) => {{
        use std::fmt::Write;
        format!(" {} ", concat_display!{ $($es),+ })
    }};
}
impl Error<'_> {
    fn to_string(&self, path: &Path) -> String {
        use ErrorCode::*;
        (match &self.error {
            ExpectedValue => format_error!(self.section.to_string(path), "expected a value"),
            ExpectedIdent {} => format_error!(
                self.section.to_string(path),
                "expected an identifier",
                "you have to always put a value behind a tick"
            ),
            IdentWithJustDot => format_error!(
                self.section.to_string(path),
                "an identifier was just made of a single dot",
                "identifiers have to contain more than just a leading dot"
            ),
            NoClosingQuotes { quote } => format_error!(
                self.section.to_string(path),
                "the ending quotes of the quote {} were missing",
                [format!("{}{}{}", "\"", quote, "\"".red().underline())]
            ),
            LonelyElse => {
                format_error!(
                    self.section.to_string(path),
                    "the else - keyword has been used without an if / loop - block infront of it",
                    "you've to add the if / loop block"
                )
            }
            JumpInsideFuncArg { keyword } => format_error!(
                self.section.to_string(path),
                "there was a {} - jump inside of a function arguments type",
                [keyword],
                "you have to remove the jump, because there is no location to jump to"
            ),
            CodeAfterJump => {
                format_error!(
                    self.section.to_string(path),
                    "there was code after a jump",
                    "a jump cannot be followed by code"
                )
            }
            NoOpenedBracket { closed } => {
                format_error!(
                    self.section.to_string(path),
                    "there was a closed bracket {} but no opened one",
                    [closed.display_closed()]
                )
            }
            NoClosedBracket { opened } => {
                format_error!(
                    self.section.to_string(path),
                    "there was a opened bracket {} but no closed one",
                    [opened.display_open()]
                )
            }
            WrongClosedBracket { expected, found } => {
                format_error!(
                    self.section.to_string(path),
                    "found the closed bracket {} but actually expected {}",
                    [found.display_closed(), expected.display_closed()]
                )
            }
        })
        .to_string()
    }
}
#[derive(Debug)]
pub enum CliError {
    Io(std::io::Error),
    CommandLine(&'static str),
    ThreadPanic(String),
    NotValidUTF8(&'static Path),
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::ThreadPanic(e) => format_error!(e),
                Self::Io(e) => format_error!(e),
                Self::NotValidUTF8(path) => {
                    format_error!(
                        "the file  {}  did not contain valid UTF-8",
                        format!("{:?}", path)
                    )
                }
                Self::CommandLine(mes) => format_error!(mes),
            }
        )
    }
}

use crate::parser::tokenizing::token::Token;
use crate::tree::Bracket;
use std::path::Path;

fn remove_quotes(path: &Path) -> String {
    String::from(
        format!("{:?}", path.as_os_str())
            .strip_prefix("\"")
            .unwrap()
            .strip_suffix("\"")
            .unwrap(),
    )
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub collum: usize,
    pub line: usize,
}
impl Position {
    #[inline]
    pub fn beginning() -> Self {
        Position { collum: 1, line: 1 }
    }
    #[inline]
    pub fn at(collum: usize, line: usize) -> Self {
        Position { collum, line }
    }
    #[inline]
    pub fn next_line(&mut self) {
        self.line += 1;
        self.collum = 1;
    }
    #[inline]
    pub fn at_next_line(mut self) -> Position {
        self.line += 1;
        self.collum = 1;
        self
    }
}

impl Add<usize> for Position {
    type Output = Self;
    #[inline]
    fn add(mut self, rhs: usize) -> Self::Output {
        self.collum += rhs;
        self
    }
}
impl Sub<usize> for Position {
    type Output = Self;
    #[inline]
    fn sub(mut self, rhs: usize) -> Self::Output {
        self.collum -= rhs;
        self
    }
}
impl AddAssign<usize> for Position {
    fn add_assign(&mut self, rhs: usize) {
        self.collum += rhs;
    }
}
impl SubAssign<usize> for Position {
    fn sub_assign(&mut self, rhs: usize) {
        self.collum -= rhs;
    }
}
impl Sub<Position> for Position {
    type Output = Span;
    /// subtracts the two positions
    #[inline]
    fn sub(self, rhs: Position) -> Self::Output {
        Span {
            start: self,
            end: rhs,
        }
    }
}
impl Sub<Span> for Position {
    type Output = Span;
    fn sub(self, mut rhs: Span) -> Self::Output {
        rhs.start = self;
        rhs
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
impl Span {
    #[inline]
    pub fn beginning() -> Self {
        Span {
            start: Position::beginning(),
            end: Position::beginning(),
        }
    }
    #[inline]
    pub fn at(start_collum: usize, start_line: usize, end_collum: usize, end_line: usize) -> Self {
        Self {
            start: Position::at(start_collum, start_line),
            end: Position::at(end_collum, end_line),
        }
    }
    #[inline]
    pub fn end(mut self) -> Self {
        self.start.line = self.end.line;
        self.start.collum = self.end.collum;
        self
    }
    #[inline]
    pub fn end_mut(&mut self) -> &mut Position {
        &mut self.end
    }
    #[inline]
    pub fn start(mut self) -> Self {
        self.end.line = self.start.line;
        self.end.collum = self.start.collum;
        self
    }
    #[inline]
    pub fn start_mut(&mut self) -> &mut Position {
        &mut self.start
    }
}
impl Add<usize> for Span {
    type Output = Self;
    #[inline]
    fn add(mut self, rhs: usize) -> Self::Output {
        self.end.collum += rhs;
        self
    }
}
impl Sub<usize> for Span {
    type Output = Self;
    #[inline]
    fn sub(mut self, rhs: usize) -> Self::Output {
        self.end.collum -= rhs;
        self
    }
}
impl From<Position> for Span {
    fn from(pos: Position) -> Self {
        Span {
            start: pos,
            end: pos,
        }
    }
}

impl Sub<Span> for Span {
    type Output = Span;
    /// combines the two spans
    #[inline]
    fn sub(mut self, rhs: Span) -> Self::Output {
        self.end.line = rhs.end.line;
        self.end.collum = rhs.end.collum;
        self
    }
}
impl SubAssign<Span> for Span {
    /// combines the two spans and stores the result in the first
    #[inline]
    fn sub_assign(&mut self, rhs: Span) {
        self.end = rhs.end
    }
}

impl Sub<Position> for Span {
    type Output = Self;
    fn sub(mut self, rhs: Position) -> Self::Output {
        self.end = rhs;
        self
    }
}

impl Span {
    fn to_string(self, path: &Path) -> String {
        match self.start.line == self.end.line {
            true => match self.start.collum == self.end.collum {
                true => format!(
                    "at {}:{}:{}",
                    remove_quotes(path),
                    self.start.line,
                    self.start.collum
                ),
                false => format!(
                    "at {}:{}:{} - {}",
                    remove_quotes(path),
                    self.start.line,
                    self.start.collum,
                    self.end.collum
                ),
            },
            false => format!(
                "at {}:{}:{}-{}:{}",
                remove_quotes(path),
                self.start.line,
                self.start.collum,
                self.end.line,
                self.end.collum
            ),
        }
    }
}

// implentation of error
impl error::Error for CliError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            CliError::Io(e) => Some(e),
            _ => None,
        }
    }
}

// convertion of other error types
impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> CliError {
        CliError::Io(err)
    }
}
impl From<Box<dyn Any + Send + 'static>> for CliError {
    fn from(err: Box<dyn Any + Send + 'static>) -> CliError {
        if let Some(panic_msg) = err.downcast_ref::<String>() {
            CliError::ThreadPanic(panic_msg.clone())
        } else {
            CliError::ThreadPanic("unknown-panic-error".to_string())
        }
    }
}
