use crate::parser::{
    tokenizing::token::{Token, TokenKind},
    Parser,
};
#[allow(unused)]
use crate::{
    error::*,
    format_error_quote_arg,
    parser::{
        binary_op::BinaryOp,
        tree::{Node, NodeBox, NodeWrapper, Note},
    },
    typing::{
        NumberKind::{self, *},
        NumberType, Type,
    },
};
use num::BigUint;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Base {
    Binary = 2,
    Seximal = 6,
    Octal = 8,
    Decimal = 10,
    Dozenal = 12,
    Hexadecimal = 16,
}
use Base::*;

impl<'src> Parser<'src> {
    /// Function to parse any literal into an AST-Node.
    /// Format:
    /// - whitespaces are only for formatting and precedence
    /// - content in brackets is optional
    /// ```
    /// N = (0  b/s/o/d/x) DIGITS
    ///
    /// numbers = N  .DIGITS  ((u/i/f N) / e / p)
    /// ```
    /// Meaning of prefixes:
    /// ```
    /// b => Binary
    /// s => Seximal
    /// o => Octal
    /// d => Dozenal
    /// x => Hexadecimal
    /// ```
    /// If instead of a base specifier, a digit is given, its assumed that the leading zero
    /// was just a typo. The number will be continued regulary.
    pub(super) fn try_to_make_number<'caller>(
        &mut self,
        span: Span,
        ident: &'src str,
    ) -> Result<NodeBox<'src>, Option<&'src str>> {
        let divisor_equation = |base, digits_after_dot| -> BigUint {
            (base as u32).pow(digits_after_dot as u32).into()
        };

        let mut ident = ident.as_bytes();
        if ident[0] == b'_' || ident[ident.len() - 1] == b'_' {
            return Err(None);
        }

        let (base, mut number) = parse_number(&mut ident);

        let divisor = if !ident.is_empty() && ident[0] == b'.' {
            ident = &ident[1..];
            match parse_digits(&mut number, base as u8, &mut ident) {
                0 => None,
                num_digits => {
                    let divisor = divisor_equation(base, num_digits);
                    Some(divisor)
                }
            }
        } else {
            None
        };

        let Some(number) = number else {
            return Err(None);
        };

        let exp = if !ident.is_empty() && (ident[0] == b'e' || ident[0] == b'p') {
            let suffix = ident;
            ident = &ident[1..];
            if !ident.is_empty() {
                self.tokenizer.buffer(Token {
                    span,
                    src: unsafe { str::from_utf8_unchecked(ident) },
                    kind: TokenKind::Ident,
                });
                ident = &[]
            }
            if let Some(exp) = self.parse_expr(BinaryOp::Pow { grade: 0 }.binding_pow()) {
                Some(exp)
            } else {
                return Err(Some(unsafe { str::from_utf8_unchecked(suffix) }));
            }
        } else {
            None
        };
        let num_type = self
            .type_parser
            .parse_number_type(ident)
            .unwrap_or(NumberType {
                kind: Arbitrary,
                size: None,
            });

        let base_node = match divisor {
            Some(divisor) => {
                let left =
                    self.make_node(NodeWrapper::new(span).with_node(Node::Literal { val: number }));

                let right = self
                    .make_node(NodeWrapper::new(span).with_node(Node::Literal { val: divisor }));

                self.make_node(
                    NodeWrapper::new(span)
                        .with_node(Node::Binary {
                            op: BinaryOp::Div,
                            lhs: left,
                            rhs: right,
                        })
                        .with_type(num_type.into()),
                )
            }
            None => self.make_node(
                NodeWrapper::new(span)
                    .with_node(Node::Literal { val: number })
                    .with_type(num_type.into()),
            ),
        };
        Ok(match exp {
            Some(exp) => {
                let base = self.make_node(NodeWrapper::new(span).with_node(Node::Literal {
                    val: (base as u8).into(),
                }));
                let right = self.make_node(NodeWrapper::new(span).with_node(Node::Binary {
                    op: BinaryOp::Pow { grade: 0 },
                    lhs: base,
                    rhs: exp,
                }));
                self.make_node(NodeWrapper::new(span).with_node(Node::Binary {
                    op: BinaryOp::Mul,
                    lhs: base_node,
                    rhs: right,
                }))
            }
            None => base_node,
        })
    }
}

/// Defaults to Decimal
fn parse_base_prefix(num: &mut &[u8]) -> Base {
    if num.len() >= 2 {
        let base = match &num[..2] {
            b"0b" => Binary,
            b"0s" => Seximal,
            b"0o" => Octal,
            b"0d" => Dozenal,
            b"0x" => Hexadecimal,
            _ => return Decimal,
        };
        *num = &num[2..];
        base
    } else {
        Decimal
    }
}

fn parse_digits(number: &mut Option<BigUint>, radix: u8, input: &mut &[u8]) -> usize {
    let mut num_digits = 0;

    while !input.is_empty() {
        if let Some(digit) = to_digit(input[0], radix) {
            if let Some(number) = number {
                *number *= BigUint::from(radix);
                *number += BigUint::from(digit);
            } else {
                *number = Some(BigUint::from(digit));
            }
            num_digits += 1;
            *input = &input[1..];
        } else if input[0] == b'_' {
            *input = &input[1..];
        } else {
            return num_digits;
        }
    }
    num_digits
}

fn to_digit(c: u8, radix: u8) -> Option<u8> {
    debug_assert!(radix <= 36);

    if c.is_ascii_digit() && c <= b'0' - 1 + radix {
        return Some(c - b'0');
    }

    let c = c | 0b0010_0000; // OR the third bit into c - this has the equivalent of making it lowercase

    if c.is_ascii_lowercase() && c <= b'a' - 11 + radix {
        return Some(c - b'a' + 10);
    }
    None
}

pub fn parse_number(num: &mut &[u8]) -> (Base, Option<BigUint>) {
    let base = parse_base_prefix(num);
    let mut number = None;
    parse_digits(&mut number, base as u8, num);
    (base, number)
}
