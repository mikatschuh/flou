pub mod binary_op;
pub mod chained_op;
pub(super) mod into_op;
pub(super) mod keyword;
pub mod num;
pub mod unary_op;

use crate::{error::*, parser::Token};

pub struct Tokenizer {
    pos: Position,
    state: State,
}
enum State {
    Comment(String),
    Quote {
        escape_sequence: ParsingEscapeSequence,
        quote: String,
        confusions: Vec<EscapeSequenceConfusion>,
    },
    Id(String),
    Op(String),
    Nothing,
}
#[derive(PartialEq)]
enum ParsingEscapeSequence {
    False,
    True,
    Whitespace,
}
#[derive(Clone, PartialEq, Debug)]
pub struct EscapeSequenceConfusion {
    pos: Position,
    sequence: String,
}
impl Tokenizer {
    pub fn new(path: &'static std::path::Path) -> Tokenizer {
        Tokenizer {
            pos: Position::new(path),
            state: State::Nothing,
        }
    }
    pub fn process_char(
        &mut self,
        errors: &mut Errors,
        c: char,
        push: &mut impl FnMut(&mut Errors, Position, Token),
    ) {
        if !self.comment(errors, c, push) && !self.quote(errors, c, push) {
            match c {
                ' ' | '\t' => {
                    self.finish(errors, push);
                }
                '\n' => {
                    self.finish(errors, push);
                    self.pos.set_new_start();
                }
                '"' => {
                    self.finish(errors, push);
                    self.state = State::Quote {
                        escape_sequence: ParsingEscapeSequence::False,
                        quote: String::new(),
                        confusions: vec![],
                    };
                    self.pos.set_new_start();
                }
                '(' => {
                    self.finish(errors, push);
                    self.pos.set_new_start();
                    push(errors, self.pos, Token::OpenBracket { squared: false });
                }
                ')' => {
                    self.finish(errors, push);
                    self.pos.set_new_start();
                    push(errors, self.pos, Token::ClosedBracket { squared: false })
                }
                '[' => {
                    self.finish(errors, push);
                    self.pos.set_new_start();
                    push(errors, self.pos, Token::OpenBracket { squared: true })
                }
                ']' => {
                    self.finish(errors, push);
                    self.pos.set_new_start();
                    push(errors, self.pos, Token::ClosedBracket { squared: true })
                }
                '{' => {
                    self.finish(errors, push);
                    self.pos.set_new_start();
                    push(errors, self.pos, Token::OpenCurly)
                }
                '}' => {
                    self.finish(errors, push);
                    self.pos.set_new_start();
                    push(errors, self.pos, Token::ClosedCurly)
                }
                ',' => {
                    self.finish(errors, push);
                    self.pos.set_new_start();
                    push(errors, self.pos, Token::Comma)
                }
                _ => match into_op::char_is_op(c) {
                    true => match self.state {
                        State::Op(ref mut op) => {
                            if op.ends_with('/') && c == '/' {
                                self.state = State::Comment(String::new());
                                self.pos.prev_char();
                                self.pos.set_new_start();
                                self.pos.next_char();
                            } else {
                                op.push(c)
                            }
                        }
                        _ => {
                            self.finish(errors, push);
                            self.state = State::Op(String::from(c))
                        }
                    },
                    false => match self.state {
                        State::Id(ref mut id) => id.push(c),
                        State::Nothing => self.state = State::Id(String::from(c)),
                        _ => {
                            self.finish(errors, push);
                            self.state = State::Id(String::from(c))
                        }
                    },
                },
            }
        }
        match c {
            '\n' => self.pos.newline(),
            _ => self.pos.next_char(),
        }
    }
    fn comment(
        &mut self,
        _: &mut Errors,
        c: char,
        _: &mut impl FnMut(&mut Errors, Position, Token),
    ) -> bool {
        if let State::Comment(ref mut comment) = self.state {
            match c {
                '\n' => {
                    self.state = State::Nothing;
                }
                _ => {
                    comment.push(c);
                }
            }
            true
        } else {
            false
        }
    }
    fn quote(
        &mut self,
        errors: &mut Errors,
        c: char,
        push: &mut impl FnMut(&mut Errors, Position, Token),
    ) -> bool {
        if let State::Quote {
            // return from the function - no more processing except pushing to quote
            ref mut escape_sequence,
            ref mut quote,
            ref mut confusions,
        } = self.state
        {
            use ParsingEscapeSequence::*;
            if let True = *escape_sequence {
                match c {
                    'n' => quote.push('\n'),
                    't' => quote.push('\t'),
                    'b' => quote.push('\u{0008}'),
                    'f' => quote.push('\u{000C}'),
                    '\\' => quote.push('\\'),
                    '"' => quote.push('"'),
                    '\n' => {
                        *escape_sequence = Whitespace;
                        quote.push('\n')
                    }
                    _ => {
                        *quote += "\\";
                        quote.push(c);
                        let mut pos = self.pos.only_end() - 1;
                        pos.next_char();
                        confusions.push(EscapeSequenceConfusion {
                            pos: self.pos,
                            sequence: format!("\\{}", c),
                        })
                    }
                }
                *escape_sequence = False; // setting the escape_sequence to false if it was true previously
            } else {
                if let Whitespace = *escape_sequence {
                    if c == '\t' || c == ' ' {
                        return true;
                    } else {
                        *escape_sequence = False;
                    }
                }
                match c {
                    '\\' => *escape_sequence = True,
                    '"' => {
                        push(
                            errors,
                            self.pos,
                            Token::Quote {
                                quote: std::mem::take(quote),
                                confusions: vec![],
                            },
                        );
                        self.state = State::Nothing;
                    } // quote ends
                    _ => quote.push(c),
                }
            }
            true
        } else {
            false
        }
    }
    pub fn finish(
        &mut self,
        errors: &mut Errors,
        push: &mut impl FnMut(&mut Errors, Position, Token),
    ) {
        match self.state {
            State::Id(ref mut id) => {
                if id.len() != 0 {
                    push(errors, self.pos - 1, Token::Val(std::mem::take(id)))
                }
                self.state = State::Nothing;
            }
            State::Op(ref mut op) => {
                if op.len() != 0 {
                    if let Some(op) = into_op::PREFIX_UNARY_OPS.get(op.as_str()) {
                        push(errors, self.pos - 1, Token::PreUnary(*op))
                    } else if let Some(op) = into_op::BINARY_OPS.get(op.as_str()) {
                        push(errors, self.pos - 1, Token::Binary(*op))
                    } else if let Some(op) = into_op::POSTFIX_UNARY_OPS.get(op.as_str()) {
                        push(errors, self.pos - 1, Token::PostUnary(*op))
                    } else if let Some(op) = into_op::CHAINED_OPS.get(op.as_str()) {
                        push(errors, self.pos - 1, Token::ChainedOp(*op))
                    } else {
                        push(errors, self.pos - 1, Token::UnknownOp(std::mem::take(op)))
                    }
                }
                self.state = State::Nothing;
            }
            State::Quote { ref mut quote, .. } => errors.push(Error::new(
                self.pos.one_line_higher(),
                ErrorCode::MissingClosingQuotes {
                    quote: with_written_out_escape_sequences(quote),
                },
            )),
            _ => {}
        }
    }
    pub fn end_of_file(
        &mut self,
        errors: &mut Errors,
        push: &mut impl FnMut(&mut Errors, Position, Token),
    ) {
        if let State::Quote { ref quote, .. } = self.state {
            errors.push(Error::new(
                self.pos.one_line_higher(),
                ErrorCode::MissingClosingQuotes {
                    quote: with_written_out_escape_sequences(quote),
                },
            ))
        }
        push(errors, self.pos, Token::EoF)
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
