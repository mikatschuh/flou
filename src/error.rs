use colored::*;
use std::any::Any;
use std::error;
use std::fmt;
use std::ops::Add;
use std::ops::BitOr;
use std::ops::Sub;

pub struct Errors(Vec<Error>);

impl Errors {
    pub fn new(pos: Position, error: ErrorCode) -> Self {
        Self(vec![Error::new(pos, error)])
    }
    pub fn empty() -> Self {
        Self(Vec::new())
    }
    pub fn push(&mut self, error: Error) {
        self.0.push(error)
    }
    pub fn concat(&mut self, other: Errors) {
        self.0.extend(other.0.iter().cloned());
    }
}
impl Add for Errors {
    type Output = Self;
    fn add(self, mut rhs: Self) -> Self::Output {
        let mut res = self.0;
        res.append(&mut rhs.0);
        Errors(res)
    }
}
impl fmt::Display for Errors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut string = String::new();
        for error in self.0.iter() {
            string += &format!("{}\n", error);
        }
        write!(f, "{}", string)
    }
}
#[derive(Clone)]
pub struct Error {
    pos: Position,
    error: ErrorCode,
}
#[derive(Clone)]
pub enum ErrorCode {
    ExpectedValue {
        found: String,
    },
    UnknownOperator {
        operator: String,
    },
    MissingClosingQuotes {
        quote: String,
    },
    NumberContainedOnlyPrefix {
        number: String,
    },
    InvalidCombination {
        left: Option<Token>,
        right: Option<Token>,
    },
    // control structure mistakes
    ElseWithNoIf,
    SecondElse,
    // bracket errors
    NoOpenedBracket {
        closed: String,
    },
    WrongClosedBracket {
        expected: String,
        found: String,
    },
}
impl Error {
    pub fn new(pos: Position, error: ErrorCode) -> Self {
        Self { pos, error }
    }
}

static ERROR: std::sync::LazyLock<String> =
    std::sync::LazyLock::new(|| format!("{}{}", "error".bold().red(), &":".bold()));
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
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorCode::*;
        write!(
            f,
            "{}",
            match &self.error {
                UnknownOperator { operator } => format_error!(
                    self.pos,
                    "the operator {} is not known to the compiler",
                    [operator]
                ),
                ExpectedValue { found } =>
                    format_error!(self.pos, "expected a value found {}", [found]),
                MissingClosingQuotes { quote } => format_error!(
                    self.pos,
                    "the ending quotes of the quote {} were missing",
                    [format!("{}{}{}", "\"", quote, "\"".red().underline())]
                ),
                NumberContainedOnlyPrefix { number } => {
                    format_error!(
                        self.pos,
                        "there was a base prefix and nothing behind, {}",
                        [number],
                        "if you wanted to make an identifier, dont let it start with a number"
                    )
                }
                InvalidCombination { left, right } => {
                    if let Some(left) = left {
                        if let Some(right) = right {
                            format_error!(
                                self.pos,
                                "you can't combine {} with an {}",
                                [left, right]
                            )
                        } else {
                            format_error!(
                                self.pos,
                                "{} was followed by a great nothingness",
                                [left]
                            )
                        }
                    } else if let Some(right) = right {
                        format_error!(
                            self.pos,
                            "{} followed a great nothingness",
                            [right],
                            "place a value infront"
                        )
                    } else {
                        unreachable!()
                    }
                }
                ElseWithNoIf => {
                    format_error!(
                        self.pos,
                        "the else - keyword has been used without an if - block infront of it",
                        "you've to add the if block"
                    )
                }
                SecondElse => {
                    format_error!(self.pos, "there was a second else")
                }
                NoOpenedBracket { closed } => {
                    format_error!(
                        self.pos,
                        "there was a closed bracket {} but no opened one",
                        [closed]
                    )
                }
                WrongClosedBracket { expected, found } => {
                    format_error!(
                        self.pos,
                        "found the closed bracket {} but actually expected {}",
                        [found, expected]
                    )
                }
            }
        )
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

use crate::parser::Token;
use std::path::Path;

fn remove_quotes(path: &'static Path) -> String {
    String::from(
        format!("{:?}", path.as_os_str())
            .strip_prefix("\"")
            .unwrap()
            .strip_suffix("\"")
            .unwrap(),
    )
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub file: &'static Path,
    pub start_line: usize,
    pub start_char: usize,
    pub end_line: usize,
    pub end_char: usize,
}
impl Position {
    pub fn new(path: &'static Path) -> Self {
        Position {
            file: path,
            start_char: 1,
            start_line: 1,
            end_line: 1,
            end_char: 1,
        }
    }
    pub fn new_at(
        path: &'static Path,
        start_line: usize,
        start_char: usize,
        end_line: usize,
        end_char: usize,
    ) -> Self {
        Self {
            file: path,
            start_line,
            start_char,
            end_line,
            end_char,
        }
    }
    pub fn next_char(&mut self) {
        self.end_char += 1
    }
    pub fn prev_char(&mut self) {
        self.end_char -= 1
    }
    pub fn newline(&mut self) {
        self.end_line += 1;
        self.end_char = 1;
    }
    pub fn set_new_start(&mut self) {
        self.start_line = self.end_line;
        self.start_char = self.end_char;
    }
    pub fn set_end(&mut self, end_line: usize, end_char: usize) {
        self.end_line = end_line;
        self.end_char = end_char;
    }
    pub fn with_end(mut self, end_line: usize, end_char: usize) -> Self {
        self.end_line = end_line;
        self.end_char = end_char;
        self
    }
    pub fn only_end(mut self) -> Self {
        self.start_line = self.end_line;
        self.start_char = self.end_char;
        self
    }
    pub fn only_start(mut self) -> Self {
        self.end_line = self.start_line;
        self.end_char = self.start_char;
        self
    }
    pub fn one_line_higher(&self) -> Self {
        Self {
            file: self.file,
            start_line: self.start_line,
            start_char: self.start_char,
            end_line: self.end_line - 1,
            end_char: self.end_char,
        }
    }
}
impl Add<usize> for Position {
    type Output = Self;
    fn add(mut self, rhs: usize) -> Self::Output {
        self.end_char += rhs;
        self
    }
}
impl Sub<usize> for Position {
    type Output = Self;
    fn sub(mut self, rhs: usize) -> Self::Output {
        self.end_char -= rhs;
        self
    }
}
impl BitOr<Position> for Position {
    type Output = Position;
    /// combines the two positions
    fn bitor(mut self, rhs: Position) -> Self::Output {
        self.end_line = rhs.end_line;
        self.end_char = rhs.end_char;
        self
    }
}
impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.start_line == self.end_line {
            true => match self.start_char == self.end_char {
                true => write!(
                    f,
                    "at {}:{}:{}",
                    remove_quotes(self.file),
                    self.start_line,
                    self.start_char
                ),
                false => write!(
                    f,
                    "at {}:{}:{} - {}",
                    remove_quotes(self.file),
                    self.start_line,
                    self.start_char,
                    self.end_char
                ),
            },
            false => write!(
                f,
                "at {}:{}:{}-{}:{}",
                remove_quotes(self.file),
                self.start_line,
                self.start_char,
                self.end_line,
                self.end_char
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
