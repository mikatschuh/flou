#[cfg(test)]
pub mod test;
#[allow(dead_code)]
pub mod token;

use std::{iter::FusedIterator, str::CharIndices, vec::IntoIter};
use token::Token;

use crate::{
    error::{ErrorCode, Errors, Position, Span},
    parser::{keyword::Keyword, tokenizing::token::TokenKind},
    utilities::{ArrayQueue, Rc},
};

const BUFFER_SIZE: usize = 2;
const BUFFER_LOG_2: usize = 1;

#[derive(Debug, Clone)]
pub struct Tokenizer<'src> {
    span: Span,
    pos_state: PositionState,

    state: State,

    text: &'src str,
    chars: CharIndices<'src>,

    start_i: usize,
    i: usize,
    next_i: usize,

    buffer: ArrayQueue<Token<'src>, BUFFER_SIZE, BUFFER_LOG_2>,

    errors: Rc<Errors<'src>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum State {
    Op(TokenKind),
    Id,
    Nothing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EscapeSequenceConfusion {
    pos: Span,
    sequence: String,
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum PositionState {
    SoF,
    JustAfterNewLine,
    WithinLine,
}

impl PositionState {
    fn step(&mut self, pos: &mut Position, c: char) {
        use PositionState::*;
        match self {
            SoF => {}
            JustAfterNewLine => pos.next_line(),
            WithinLine => *pos += 1,
        }
        *self = match c {
            '\n' => JustAfterNewLine,
            _ => WithinLine,
        }
    }
}
impl<'src> Iterator for Tokenizer<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.buffer.is_empty() {
            self.restock_tokens();
        }
        self.buffer.pop_front()
    }
}
impl<'src> FusedIterator for Tokenizer<'src> {}

impl<'src> Tokenizer<'src> {
    pub fn consume_while(&mut self, mut predicate: impl FnMut(&Token) -> bool) -> IntoIter<Token> {
        let mut tokens = Vec::new();
        while self.peek().is_some_and(&mut predicate) {
            tokens.push(unsafe { self.next().unwrap_unchecked() });
        }
        tokens.into_iter()
    }
}

impl<'src> Tokenizer<'src> {
    pub fn new(text: &'src str, errors: Rc<Errors<'src>>) -> Self {
        Tokenizer {
            span: Span::beginning(),
            pos_state: PositionState::SoF,

            state: State::Nothing,

            text,
            chars: text.char_indices(),

            start_i: 0,
            i: 0,
            next_i: 0,

            buffer: ArrayQueue::new(),

            errors,
        }
    }

    pub fn peek(&mut self) -> Option<&Token<'src>> {
        if self.buffer.is_empty() {
            self.restock_tokens();
        }
        self.buffer.first()
    }

    pub fn next_if(&mut self, predicate: impl FnOnce(&Token<'src>) -> bool) -> Option<Token<'src>> {
        if self.buffer.is_empty() {
            self.restock_tokens();
        }

        if let Some(tok) = self.buffer.first() {
            if predicate(tok) {
                return self.buffer.pop_front();
            }
        }
        None
    }

    pub fn next_is(&mut self, predicate: impl FnOnce(&Token<'src>) -> bool) -> bool {
        if self.buffer.is_empty() {
            self.restock_tokens();
        }
        if let Some(tok) = self.buffer.first() {
            predicate(tok)
        } else {
            false
        }
    }

    /// Method for buffering a token. If the buffer is full (or this is called twice in a row)
    /// a panic is invocated.
    pub fn buffer(&mut self, token: Token<'src>) {
        self.buffer.push_front(token)
    }
}

impl<'src> Tokenizer<'src> {
    fn next_char(&mut self) -> Option<char> {
        let (i, c) = self.chars.next()?;
        self.pos_state.step(self.span.end_mut(), c);
        self.i = self.next_i;
        self.next_i = i + c.len_utf8();
        Some(c)
    }

    #[inline]
    fn set_op(&mut self, op: TokenKind) {
        self.span.start = self.span.end;
        self.start_i = self.i;
        self.state = State::Op(op)
    }
    #[inline]
    fn set_id(&mut self) {
        self.span.start = self.span.end;
        self.start_i = self.i;
        self.state = State::Id
    }

    fn restock_tokens(&mut self) {
        while let Some(c) = self.next_char() {
            if c.is_whitespace() {
                self.submit_current(self.span - 1, self.i); // -1 to ignore the whitespace
            } else {
                if c == '"' {
                    self.submit_current(self.span - 1, self.i); // -1 to ignore the quotation mark
                    self.quote(self.i);
                    return;
                }
                if let State::Op(ref mut token) = self.state {
                    if *token == TokenKind::Slash && c == '/' {
                        self.comment();

                        self.state = State::Nothing;
                        continue;
                    } else if let Some(new_token) = token.add(c) {
                        *token = new_token;
                        continue;
                    } else {
                        self.submit_current(self.span - 1, self.i);
                    }
                }
                if let Some(new_token) = TokenKind::new(c) {
                    self.submit_current(self.span - 1, self.i); // incase the previous one was an identifier
                    self.set_op(new_token);
                } else if let State::Nothing = self.state {
                    self.set_id();
                }
            }
            if self.buffer.len() > BUFFER_SIZE - 2 {
                return;
            }
        }
        self.submit_current(self.span, self.next_i);
    }

    fn comment(&mut self) {
        while !matches!(self.next_char(), Some('\n')) {}
    }

    fn quote(&mut self, start_i: usize) {
        self.span.start = self.span.end;
        while let Some(c) = self.next_char() {
            if c == '"' {
                self.buffer.push_back(Token {
                    span: self.span.start - self.span.end + 1,
                    src: &self.text[start_i..self.next_i],
                    kind: TokenKind::Quote,
                });
                return;
            } // quote ends
        }
        self.errors.push(
            self.span,
            ErrorCode::NoClosingQuotes {
                quote: &self.text[start_i..self.next_i],
            },
        );
        self.buffer.push_back(Token {
            span: self.span,
            src: &self.text[start_i..self.next_i],
            kind: TokenKind::Quote,
        })
    }

    fn submit_current(&mut self, span: Span, end_i: usize) {
        match self.state {
            State::Op(token) => self.buffer.push_back(Token {
                span: span.start - span.end + 1,
                src: &self.text[self.start_i..end_i],
                kind: token,
            }),
            State::Id => self.buffer.push_back(Token {
                span: span.start - span.end + 1,
                kind: Keyword::from_str(&self.text[self.start_i..end_i])
                    .map(TokenKind::Keyword)
                    .unwrap_or(TokenKind::Ident),
                src: &self.text[self.start_i..end_i],
            }),
            State::Nothing => return, // skip the reassignment
        }
        self.state = State::Nothing
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
                        sequence: format!("\\{c}"),
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
