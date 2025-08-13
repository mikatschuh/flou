#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Loop,
    If,
    Else,
    Continue,
    Break,
    Exit,
    Return,
}

impl Keyword {
    pub fn display(&self) -> &'static str {
        use Keyword::*;
        match self {
            Loop => "loop",
            If => "if",
            Else => "else",
            Continue => "continue",
            Break => "break",
            Exit => "exit",
            Return => "return",
        }
    }
    pub fn from_str(string: &str) -> Option<Self> {
        use Keyword::*;
        match string {
            "loop" => Some(Loop),
            "wiederhole" => Some(Loop),
            "if" => Some(If),
            "wenn" => Some(If),
            "else" => Some(Else),
            "sonst" => Some(Else),
            "continue" => Some(Continue),
            "nächste" => Some(Continue),
            "break" => Some(Break),
            "exit" => Some(Exit),
            "verlasse" => Some(Exit),
            "return" => Some(Return),
            "zurückgeben" => Some(Return),
            _ => None,
        }
    }
}
