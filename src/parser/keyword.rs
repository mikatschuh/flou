#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Proc,
    Loop,
    If,
    Else,
    Continue,
    Break,
    Return,
}

impl Keyword {
    pub fn display(&self) -> &'static str {
        use Keyword::*;
        match self {
            Proc => "proc",
            Loop => "loop",
            If => "if",
            Else => "else",
            Continue => "continue",
            Break => "break",
            Return => "return",
        }
    }
    pub fn from_str(string: &str) -> Option<Self> {
        use Keyword::*;
        Some(match string {
            "proc" => Proc,
            "prozedur" => Proc,
            "loop" => Loop,
            "wiederhole" => Loop,
            "if" => If,
            "wenn" => If,
            "else" => Else,
            "sonst" => Else,
            "continue" => Continue,
            "nächste" => Continue,
            "break" => Break,
            "verlasse" => Break,
            "return" => Return,
            "zurückgeben" => Return,
            _ => return None,
        })
    }
}
