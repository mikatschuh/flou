pub mod binary_op;
pub mod chained_op;
pub mod keyword;
pub mod token;
pub mod unary_op;

use arrayvec::ArrayVec;
use std::str::CharIndices;
use token::Token;

use crate::{
    error::{ErrorCode, Errors, Position, Span},
    parser::tokenizing::token::TokenKind,
    utilities::Rc,
};
#[derive(Debug, Clone)]
pub struct Tokenizer<'src> {
    span: Span,
    section: Section,
    pos_state: PositionState,
    op: TokenKind,
    text: &'src str,
    chars: CharIndices<'src>,
    /// The front is where new tokens are added
    buffer: ArrayVec<Token<'src>, 3>,
    errors: Rc<Errors<'src>>,
}
#[derive(Clone, Copy, Debug)]
struct Section {
    start: usize,
    end: usize,
}
impl Section {
    fn beginning() -> Self {
        Self { start: 0, end: 0 }
    }
    fn get(self, text: &str) -> &str {
        &text[self.start..=self.end]
    }
}
#[derive(Clone, PartialEq, Debug)]
pub struct EscapeSequenceConfusion {
    pos: Span,
    sequence: String,
}
#[derive(Clone, Copy, PartialEq, Debug)]
enum PositionState {
    SOF,
    JustAfterNewLine,
    WithinLine,
}
impl PositionState {
    fn step(&mut self, pos: &mut Position, c: char) {
        use PositionState::*;
        match self {
            SOF => {}
            JustAfterNewLine => pos.next_line(),
            WithinLine => *pos += 1,
        }
        match c {
            '\n' => *self = JustAfterNewLine,
            _ => *self = WithinLine,
        }
    }
}
impl<'src> Iterator for Tokenizer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.buffer.is_empty() {
            self.restock_tokens();
        }
        self.buffer.pop()
    }
}
impl<'src> Tokenizer<'src> {
    pub fn new(text: &'src str, errors: Rc<Errors<'src>>) -> Self {
        Tokenizer {
            span: Span::beginning(),
            section: Section::beginning(),
            pos_state: PositionState::SOF,
            op: TokenKind::Empty,
            chars: text.char_indices(),
            text: text.into(),
            buffer: ArrayVec::new(),
            errors,
        }
    }
    pub fn peek(&mut self) -> Option<&Token<'src>> {
        if self.buffer.is_empty() {
            self.restock_tokens();
        }
        self.buffer.last()
    }
}

impl<'src> Tokenizer<'src> {
    fn next_char(&mut self) -> Option<char> {
        let Some((i, c)) = self.chars.next() else {
            return None;
        };
        self.pos_state.step(self.span.end_mut(), c);
        self.section.end = i;
        Some(c)
    }
    #[inline]
    fn submit_op(&mut self) {
        self.buffer.push(Token {
            span: self.span,
            src: self.section.get(self.text),
            kind: self.op,
        });
    }
    fn restock_tokens(&mut self) {
        while let Some(c) = self.next_char() {
            if c.is_whitespace() {
                self.submit_current(
                    self.span - 1, // to ignore the whitespace
                    Section {
                        start: self.section.start,
                        end: self.section.end - c.len_utf8(), // to ignore the whitespace
                    },
                );
                continue;
            }
            if c == '"' {
                // the tokens need to be submitted in the opposite order
                // for the buffer to function correctly
                let span = self.span;
                let section = self.section;
                self.span.start = self.span.end;
                self.quote();
                self.submit_current(span, section);
                return;
            }
            if !matches!(self.op, TokenKind::Empty) {
                if self.op == TokenKind::Slash && c == '/' {
                    self.op = TokenKind::Empty;

                    self.span.start = self.span.end - c.len_utf8(); // ignore slash
                    self.section.start = self.section.end - c.len_utf8();

                    self.comment();
                    continue;
                }
                if let Some(new_op) = self.op.add(c) {
                    self.op = new_op;
                    continue;
                } else {
                    self.submit_op();
                    self.op = TokenKind::Empty;
                    self.section.start = self.section.end; // if the next thing was an identifier
                }
            }
            if let Some(new_op) = TokenKind::Empty.add(c) {
                self.op = new_op;
                self.section.start = self.section.end;
            }
        }
        self.submit_current(self.span, self.section);
    }
    fn comment(&mut self) {
        while let Some(c) = self.next_char() {
            match c {
                '\n' => {
                    return;
                }
                _ => {}
            }
        }
    }
    fn quote(&mut self) {
        while let Some(c) = self.next_char() {
            match c {
                '"' => {
                    self.buffer.push(Token {
                        span: self.span,
                        src: self.section.get(self.text),
                        kind: TokenKind::Quote,
                    });
                    return;
                } // quote ends
                _ => {}
            }
        }
        self.errors.push(
            self.span,
            ErrorCode::MissingClosingQuotes {
                quote: self.section.get(self.text),
            },
        );
        self.buffer.push(Token {
            span: self.span,
            src: self.section.get(self.text),
            kind: TokenKind::Quote,
        })
    }
    fn submit_current(&mut self, span: Span, section: Section) {
        if !matches!(self.op, TokenKind::Empty) {
            self.buffer.push(Token {
                span,
                src: section.get(self.text),
                kind: self.op,
            })
        } else {
            self.buffer.push(Token {
                span,
                src: section.get(self.text),
                kind: TokenKind::Ident,
            })
        }
    }
}

pub fn with_written_out_escape_sequences(string: &str) -> String {
    let mut output_string = String::new();
    for c in string.chars() {
        match c {
            '\n' => output_string += "\\n",
            '\t' => output_string += "\\t",
            '\u{0008}' => output_string += "\\b",
            '\u{000C}' => output_string += "\\f",
            '\\' => output_string += "\\\\",
            '"' => output_string += "\\\"",
            _ => output_string.push(c),
        }
    }
    output_string
}
pub fn resolve_escape_sequences(quote: &str) -> (String, Vec<EscapeSequenceConfusion>) {
    #[derive(PartialEq)]
    enum ParsingEscapeSequence {
        False,
        True,
        Whitespace,
    }
    let mut output_string = String::new();
    let mut escape_sequence = ParsingEscapeSequence::False;
    let mut confusions: Vec<EscapeSequenceConfusion> = vec![];
    let mut relative_position = Position::at(1, 0);
    for c in quote.chars().skip(1) {
        use ParsingEscapeSequence::*;
        if let True = escape_sequence {
            match c {
                'n' => output_string.push('\n'),
                't' => output_string.push('\t'),
                'b' => output_string.push('\u{0008}'),
                'f' => output_string.push('\u{000C}'),
                '\\' => output_string.push('\\'),
                '"' => output_string.push('"'),
                '\n' => {
                    escape_sequence = Whitespace;
                    output_string.push('\n');
                    continue;
                }
                _ => {
                    output_string += "\\";
                    output_string.push(c);
                    confusions.push(EscapeSequenceConfusion {
                        pos: relative_position - 1 - relative_position,
                        sequence: format!("\\{}", c),
                    })
                }
            }
            escape_sequence = False; // setting the escape_sequence to false if it was true previously
        } else {
            if escape_sequence == Whitespace {
                if c == '\t' || c == ' ' {
                    continue;
                } else {
                    escape_sequence = False;
                }
            }
            match c {
                '\\' => escape_sequence = True,
                '\n' => {
                    output_string.push('\n');
                    relative_position.next_line();
                }
                _ => {
                    output_string.push(c);
                    relative_position += 1
                }
            }
        }
    }
    output_string.pop();
    (output_string, confusions)
}
#[test]
fn text() {
    use std::path::Path;
    use TokenKind::*;
    let errors = Rc::new(Errors::empty(Path::new("example.flou")));
    assert!(
        Tokenizer::new("+++*===!>|", errors.clone())
            .into_iter()
            .collect::<Vec<_>>()
            == vec![
                Token::new(Span::at(1, 1, 2, 1), "++", PlusPlus),
                Token::new(Span::at(3, 1, 3, 1), "+", Plus),
                Token::new(Span::at(4, 1, 5, 1), "*=", StarEqual),
                Token::new(Span::at(6, 1, 7, 1), "==", EqualEqual),
                Token::new(Span::at(8, 1, 11, 1), "!>|", NotRightPipe)
            ]
    )
}
