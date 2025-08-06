#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Loop,
    If,
    Else,
    Continue,
    Break,
    Exit,
    Return,
}
use std::fmt;
impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Keyword::*;
        let string = match self {
            Loop => "loop",
            If => "if",
            Else => "else",
            Continue => "continue",
            Break => "break",
            Exit => "exit",
            Return => "return",
        };
        write!(f, "{}", string)
    }
}
impl Keyword {
    pub fn from_str(string: &str) -> Option<Self> {
        use Keyword::*;
        match string {
            "loop" => Some(Loop),
            "if" => Some(If),
            "else" => Some(Else),
            "continue" => Some(Continue),
            "break" => Some(Break),
            "exit" => Some(Exit),
            "return" => Some(Return),
            _ => None,
        }
    }
}
