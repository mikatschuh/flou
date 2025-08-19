use std::{iter::Peekable, str::CharIndices};

use crate::parser::{
    tokenizing::token::{Token, TokenKind},
    Flags, Parser,
};
#[allow(unused)]
use crate::{
    error::*,
    format_error_quote_arg,
    parser::binary_op::BinaryOp,
    tree::{Node, NodeId, NodeWrapper, NodeWrapping, Note, Tree},
    typing::{NumberType, Type},
};
use num::BigUint;
use NumberType::*;
use Type::*;

#[cfg(target_pointer_width = "64")]
const SYSTEM_SIZE: usize = 64;

#[cfg(target_pointer_width = "32")]
const SYSTEM_SIZE: usize = 32;

#[cfg(target_pointer_width = "16")]
const SYSTEM_SIZE: usize = 16;

macro_rules! cfg_system_width {
    ($expr32:expr, $expr64:expr) => {{
        #[cfg(target_pointer_width = "64")]
        {
            $expr64
        }
        #[cfg(not(target_pointer_width = "64"))]
        {
            $expr32
        }
    }};
}

#[derive(Clone, Copy, PartialEq)]
enum Base {
    Binary = 2,
    Seximal = 6,
    Octal = 8,
    Decimal = 10,
    Dozenal = 12,
    Hexadecimal = 16,
}
use Base::*;

impl<'src, W: NodeWrapping<'src> + 'src> Parser<'src, W> {
    /// Function to parse any literal into an AST-Node.
    /// Format:
    /// - whitespaces are only for formatting and precedence
    /// - content in brackets is optional
    /// ```
    /// N = (0  b/s/o/d/x) DIGITS
    ///
    /// numbers = N  .DIGITS  ((u/i/f N) / e / i)
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
    pub(super) fn convert_to_num_if_possible<'caller>(
        &mut self,
        span: Span,
        ident: &'src str,
        flags: Flags<'src, 'caller>,
    ) -> NodeId<'src> {
        debug_assert_ne!(ident, "");
        debug_assert_ne!(ident, "..");

        self.try_to_make_number(span, ident, flags)
            .unwrap_or_else(|e| match e {
                Some(suffix) => self.tree.add(
                    W::new(span)
                        .with_node(Node::Ident(self.internalizer.get(ident)))
                        .with_note(suffix.into()),
                ),
                None => self
                    .tree
                    .add(W::new(span).with_node(Node::Ident(self.internalizer.get(ident)))),
            })
    }

    fn try_to_make_number<'caller>(
        &mut self,
        span: Span,
        ident: &'src str,
        flags: Flags<'src, 'caller>,
    ) -> Result<NodeId<'src>, Option<&'src str>> {
        let divisor_equation = |base, digits_after_dot| -> BigUint {
            (base as u8).pow(digits_after_dot as u32).into()
        };

        let mut chars = ident.char_indices().peekable();
        debug_assert_ne!(chars.peek().unwrap().1, '.');

        let (base, mut number) = match parse_number(&mut chars) {
            (_, None) => return Err(None),
            (base, Some(number)) => (base, number),
        };
        let divisor = if let Some((_, '.')) = chars.peek() {
            chars.next();
            match parse_digits(base as u32, &mut chars) {
                (_, None) => None,
                (num_digits, Some(after_dot)) => {
                    let divisor = divisor_equation(base, num_digits);
                    number *= divisor.clone();
                    number += after_dot;
                    Some(divisor)
                }
            }
        } else {
            None
        };
        let exp = if let Some((i, 'e')) = chars.peek() {
            let i = *i;
            chars.next();
            if let Some((i, _)) = chars.peek() {
                self.tokenizer.buffer(Token {
                    span,
                    src: &ident[*i..],
                    kind: TokenKind::Ident,
                });
                chars = "".char_indices().peekable();
            }
            if let Some(exp) = self.parse_expr(u8::MAX, flags) {
                Some(exp)
            } else {
                return Err(Some(&ident[i..]));
            }
        } else {
            None
        };

        let ty = parse_type_suffix(&mut chars);

        if let Some((i, _)) = chars.next() {
            return Err(Some(&ident[i..]));
        }

        let node = match divisor {
            Some(divisor) => {
                let left = self
                    .tree
                    .add(W::new(span).with_node(Node::Literal { val: number }));

                let right = self
                    .tree
                    .add(W::new(span).with_node(Node::Literal { val: divisor }));

                self.tree.add(W::new(span).with_node(Node::Binary {
                    op: BinaryOp::Div,
                    left,
                    right,
                }))
            }
            None => self
                .tree
                .add(W::new(span).with_node(Node::Literal { val: number })),
        };
        Ok(match exp {
            Some(exp) => self.tree.add(
                W::new(span)
                    .with_node(Node::Binary {
                        op: BinaryOp::Pow,
                        left: node,
                        right: exp,
                    })
                    .with_type(ty.into()),
            ),
            None => {
                *self.tree[node].type_mut() = Some(ty.into());
                node
            }
        })
    }
}

/// Defaults to Decimal
fn parse_base_prefix(chars: &mut Peekable<CharIndices>) -> Base {
    if let Some((_, '0')) = chars.peek() {
        let mut base_chars = chars.clone();
        base_chars.next();
        let Some((_, second)) = base_chars.peek() else {
            return Decimal;
        };
        if let 'b' | 's' | 'o' | 'd' | 'x' | '_' = *second {
            chars.next();
            return match chars.next().unwrap().1 {
                'b' => Binary,
                's' => Seximal,
                'o' => Octal,
                'd' => Dozenal,
                'x' => Hexadecimal,
                '_' => Decimal,
                _ => unreachable!(),
            };
        }
    }
    Decimal
}

fn parse_digits(radix: u32, chars: &mut Peekable<CharIndices>) -> (usize, Option<BigUint>) {
    let mut num_digits = 0;
    let mut number: Option<BigUint> = None;

    while let Some((_, c)) = chars.peek() {
        if let Some(digit) = c.to_digit(radix) {
            if let Some(ref mut number) = number {
                chars.next();
                *number *= BigUint::from(radix);
                *number |= BigUint::from(digit);
                num_digits += 1;
            } else {
                chars.next();
                number = Some(BigUint::from(digit));
                num_digits += 1;
            }
        } else {
            return (num_digits, number);
        }
    }
    (num_digits, number)
}

fn parse_number(chars: &mut Peekable<CharIndices>) -> (Base, Option<BigUint>) {
    let base = parse_base_prefix(chars);
    let (_, number) = parse_digits(base as u32, chars);
    (base, number)
}

fn parse_type_suffix(chars: &mut Peekable<CharIndices>) -> NumberType {
    let Some((_, c)) = chars.peek() else {
        return Arbitrary;
    };
    match c {
        'u' => {
            chars.next();
            let number = parse_number(chars).1;
            Unsigned(number)
        }
        'i' => {
            chars.next();
            let number = parse_number(chars).1;
            Signed(number)
        }
        'f' => {
            chars.next();
            let number = parse_number(chars).1;
            Float(number)
        }
        _ => Arbitrary,
    }
}

fn last_char_utf8(s: &str) -> Option<char> {
    let bytes = s.as_bytes();
    let mut i = bytes.len();

    while i > 0 {
        i -= 1;
        if (bytes[i] & 0b1100_0000) != 0b1000_0000 {
            return std::str::from_utf8(&bytes[i..])
                .ok()
                .and_then(|s| s.chars().next());
        }
    }

    None
}
