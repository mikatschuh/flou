#[allow(unused)]
use std::{
    path::{Path, PathBuf},
    str::Chars,
};

use crate::tree::Bracket;
#[allow(unused)]
use crate::{
    error::*,
    format_error_quote_arg,
    parser::tokenizing::binary_op::BinaryOp,
    tree::{Node, NodeId, NodeWrapper, NodeWrapping, Note, Tree},
    typing::{NativNumber, Type},
};
use colored::Colorize;
use num::{pow, BigUint};
use NativNumber::*;
use Type::*;

const SYSTEM_SIZE: usize = 64;
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
#[derive(Debug, Clone, PartialEq)]
pub enum NumberParsingNote {
    UnknownBasePrefix { prefix: char },
    UnknownSuffix { suffix: String },
    InvalidCharacter { character: char },
    InvalidComplexSuffix { suffix: String },
    InvalidSizeSpecifier { suffix: String, specifier: String },
    TooBigSizeSpecifier { specifier: BigUint },
}
use NumberParsingNote::*;
impl NumberParsingNote {
    pub fn from_unknown_char(number_so_far: BigUint, c: char) -> Self {
        // checks if number is zero
        if let None = number_so_far.trailing_zeros() {
            return UnknownBasePrefix { prefix: c };
        } else {
            return InvalidCharacter { character: c };
        }
    }
    pub fn get_msg(self) -> String {
        match self {
            UnknownBasePrefix { prefix } => format!(
                "this wasn't parsed as a number, because the base {} isn't known to the compiler",
                format!(" \"0{}\" ", prefix).bold()
            ),
            UnknownSuffix { suffix } => format!(
                "this wasn't parsed as a number, because the suffix {} is unknown to the compiler",
                format_error_arg!(suffix),
            ),
            InvalidCharacter { character } => {
                if let 'a' | 'b' | 'c' | 'd' | 'e' | 'f' = character {
                    format!(
                    "this wasn't parsed as a number, because it contained a non-base-conform character, {} to type a hexadecimal literal prefix it with {}",
                    format!(" '{}' ", character).bold(), format!(" '0x' ").bold()
                )
                } else {
                    format!(
                    "this wasn't parsed as a number, because it contained a non-base-conform character, {}",
                    format!(" '{}' ", character).bold()
                )
                }
            }
            InvalidComplexSuffix { suffix } => format!(
                "this wasn't parsed as a number, because the suffix {} wasn't a valid complex-number-suffix",
                format_error_quote_arg!(suffix)
            ),
            InvalidSizeSpecifier { suffix, specifier } => format!(
                "this wasn't parsed as a number, because the size specifier {} is invalid, valid forms are {} and {}",
                format_error_arg!(specifier),
                format_error_arg!(suffix, 'x'),
                format_error_arg!(suffix, 'N'),
            ),
            TooBigSizeSpecifier { specifier } => format!(
                "this wasn't parsed as a number, because the given number_width of {} is too big",
                format_error_arg!(specifier)
            ),
        }
    }
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
/// Function to parse any literal into an AST-Node. The numbers have three parts:
/// BASE_PREFIX - CONTENT - IMAGINARY_SUFFIX | SIZE_SUFFIX
/// note: Underscores can be used throughout the hole content for readability
/// To indicate a base will be given, the number shall start with 0. After the 0, the
/// base specifier comes:
/// ```
/// b => Binary
/// s => Seximal
/// o => Octal
/// d => Dozenal
/// x => Hexadecimal
/// ```
/// If instead of a base specifier, a number digit is given, its assumed that the leading zero
/// was just a typo. The number will be continued regulary.
pub fn value_to_node<'a, Wrapper: NodeWrapping>(
    name: &'a str,
    pos: Span,
    tree: &mut Tree<Wrapper>,
) -> Wrapper {
    let mut str = &name[..];
    let is_imaginary = if let Some('i') = last_char_utf8(str) {
        str = &str[0..str.len() - 1]; // cut out the last char
        true
    } else {
        false
    };
    let mut divisor: Option<usize> = None;
    match match parse_integer(None, str.chars()) {
        Ok(Ok((number, suffix))) => Ok((number.1, suffix)),
        Ok(Err((integer, base, c, chars))) => {
            || -> Result<(BigUint, Type), Option<Note>> {
                if c == '.' {
                    // check if there wasnt a suffix already, and if the character is a dot
                    let ((leading_zeros, after_comma), integer_type) =
                        match parse_integer(Some(base), chars.clone())? {
                            Ok((number, suffix)) => (number, suffix),
                            Err(_) => return Err(None),
                        };
                    let after_comma_len =
                        after_comma.to_radix_le(base as u32).len() + leading_zeros;
                    divisor = Some(pow(base as usize, after_comma_len));
                    Ok((
                        integer.1 * BigUint::from(pow(base as u32, after_comma_len)) + after_comma,
                        integer_type,
                    ))
                } else {
                    Err(Some(Note::NumberParsingNote(
                        NumberParsingNote::from_unknown_char(integer.1, c),
                    )))
                }
            }()
        }
        Err(note) => Err(note),
    } {
        Ok((number, suffix)) => match divisor {
            Some(divisor) => {
                let number = tree.add(Wrapper::new(pos).with_node(Node::Literal {
                    val: number,
                    imaginary_coefficient: is_imaginary,
                }));
                let divisor = tree.add(Wrapper::new(pos).with_node(Node::Literal {
                    val: divisor.into(),
                    imaginary_coefficient: false,
                }));
                let ratio = tree.add(
                    Wrapper::new(pos)
                        .with_node(Node::BinaryOp {
                            op: BinaryOp::Div,
                            left: number,
                            right: divisor,
                        })
                        .with_type(suffix),
                );
                Wrapper::new(pos).with_node(Node::Brackets {
                    kind: Bracket::Round,
                    content: ratio,
                })
            }
            None => Wrapper::new(pos)
                .with_node(Node::Literal {
                    val: number,
                    imaginary_coefficient: is_imaginary,
                })
                .with_type(suffix),
        },
        Err(Some(comment)) => Wrapper::new(pos)
            .with_node(Node::Id(name.to_owned()))
            .add_note(comment),
        Err(None) => Wrapper::new(pos).with_node(Node::Id(name.to_owned())),
    }
}
fn parse_integer(
    fixed_base: Option<Base>,
    chars: Chars,
) -> Result<Result<((usize, BigUint), Type), ((usize, BigUint), Base, char, Chars)>, Option<Note>> {
    match parse_digits(fixed_base, chars) {
        Ok(number) => match number {
            Some(number) => Ok(Ok((number, Number(Arbitrary)))),
            None => Err(None),
        },
        Err((current, base, mut c, mut chars)) => {
            if let Some(number) = current {
                // parse type suffix
                if let 'u' | 'i' | 'f' | 'c' = c {
                    let mut str_suffix = c.to_string();
                    let complex = if c == 'c' {
                        if let Some(next) = chars.next() {
                            c = next;
                            str_suffix.push(c);
                            true
                        } else {
                            return Ok(Ok((number, ComplexNumber(Arbitrary))));
                        }
                    } else {
                        false
                    };
                    let size: Option<usize> = match match parse_digits(None, chars.clone()) {
                        Ok(number) => number,
                        Err((_, _, non_numerical_char, _)) => {
                            if non_numerical_char == 'x' && chars.clone().count() == 1 {
                                Some((0, SYSTEM_SIZE.into()))
                            // handle system size
                            } else {
                                return Err(Some(Note::NumberParsingNote(InvalidSizeSpecifier {
                                    suffix: str_suffix,
                                    specifier: chars.clone().collect(),
                                })));
                            }
                        }
                    } {
                        Some(big_int) => {
                            assert_eq!(BigUint::from(0u32).to_u32_digits(), vec![0]);
                            let digits = cfg_system_width!(
                                big_int.iter_u32_digits(),
                                big_int.1.iter_u64_digits()
                            )
                            .collect::<Vec<_>>();
                            if digits.len() != 1 {
                                return Err(Some(Note::NumberParsingNote(TooBigSizeSpecifier {
                                    specifier: big_int.1,
                                })));
                            }
                            Some(digits[0] as usize)
                        }
                        None => None,
                    };
                    let number_type = match c {
                        'u' => Unsigned(size), // unsigned
                        'i' => Signed(size),   // signed
                        'f' => Float(size),    // floating point type
                        _ => {
                            return if complex {
                                Err(Some(Note::NumberParsingNote(InvalidComplexSuffix {
                                    suffix: format!("c{}", chars.collect::<String>()),
                                })))
                            } else {
                                Err(Some(Note::NumberParsingNote(UnknownSuffix {
                                    suffix: format!("{}{}", c, chars.collect::<String>()),
                                })))
                            }
                        }
                    };
                    Ok(Ok((
                        number,
                        if complex {
                            ComplexNumber(number_type)
                        } else {
                            Number(number_type)
                        },
                    )))
                } else {
                    Ok(Err((number, base, c, chars)))
                }
            } else {
                return Err(None);
            }
        }
    }
}
/// A general purpose function for unsigned integer parsing.
/// - "_" can be used in numbers for readability reasons
/// If the function cannot parse a character, it calls the fall_back with the current number,
/// if existing, the problematic char and the remaining chars and returns the result. If the
/// string didnt contain a number nor anything other (for example: "_", "" or "___") None is returned.
fn parse_digits(
    fixed_base: Option<Base>,
    mut chars: Chars,
) -> Result<Option<(usize, BigUint)>, (Option<(usize, BigUint)>, Base, char, Chars)> {
    let mut result: Option<(usize, BigUint)> = None;

    let mut base = Base::Decimal; // default to decimal
    if let Some(given_base) = fixed_base {
        base = given_base
    }

    while let Some(c) = chars.next() {
        if let Some(ref mut unwrapped_result) = result {
            if let Some(num) = c.to_digit(base as u32) {
                if unwrapped_result.1 == 0u32.into() && num == 0 {
                    unwrapped_result.0 += 1;
                }
                unwrapped_result.1 =
                    &unwrapped_result.1 * &BigUint::from(base as usize) + BigUint::from(num)
            } else
            // the character doesnt match the base
            if c != '_' {
                return Err((result, base, c, chars)); // call the fallback
            }
        } else if c == '0' && fixed_base == None {
            let Some(second_char) = chars.next() else {
                return Ok(Some((1, 0_u32.into()))); // its just zero
            };
            match second_char {
                'b' => base = Base::Binary,
                's' => base = Base::Seximal,
                'o' => base = Base::Octal,
                'd' => base = Base::Dozenal,
                'x' => base = Base::Hexadecimal,
                _ => {
                    if let Some(num) = second_char.to_digit(base as u32) {
                        result = Some((1, num.into()));
                    } else if second_char != '_' {
                        return Err((result, base, c, chars)); // call the fallback
                    }
                }
            }
        } else {
            if let Some(num) = c.to_digit(base as u32) {
                result = Some((if num == 0 { 1 } else { 0 }, num.into()));
                // covering cases: x | ___x_____
            } else {
                return Err((result, base, c, chars)); // call the fallback
            }
        }
    }
    Ok(result)
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
#[test]
fn test_number_parsing() {
    let mut node_buffer = Tree::<NodeWrapper>::new();
    let pos = Span::beginning();
    let tests = [(
        "1",
        NodeWrapper::new(pos)
            .with_node(Node::Literal {
                val: 1u32.into(),
                imaginary_coefficient: false,
            })
            .with_type(Number(NativNumber::Arbitrary)),
        "0xA",
        NodeWrapper::new(pos)
            .with_node(Node::Literal {
                val: 10u32.into(),
                imaginary_coefficient: false,
            })
            .with_type(Number(NativNumber::Arbitrary)),
        "0d10",
        NodeWrapper::new(pos)
            .with_node(Node::Literal {
                val: 12u32.into(),
                imaginary_coefficient: false,
            })
            .with_type(Number(NativNumber::Arbitrary)),
        "0s100_u0x27_i",
        NodeWrapper::new(pos)
            .with_node(Node::Literal {
                val: 36u32.into(),
                imaginary_coefficient: true,
            })
            .with_type(Number(NativNumber::Unsigned(Some(39)))),
        "0b100_ui",
        NodeWrapper::new(pos)
            .with_node(Node::Literal {
                val: 4u32.into(),
                imaginary_coefficient: true,
            })
            .with_type(Number(NativNumber::Unsigned(None))),
    )];
    for test in tests {
        let node = value_to_node(test.0.into(), pos, &mut node_buffer);
        assert_eq!(node, test.1);
    }
}
