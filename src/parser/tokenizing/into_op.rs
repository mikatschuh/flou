use crate::{
    error::{Error, ErrorCode, Errors, Position},
    parser::{
        tokenizing::{
            binary_op::BinaryOp::{self, *},
            chained_op::ChainedOp::{self, *},
            unary_op::{PostfixUnaryOp, PrefixUnaryOp},
        },
        Token,
    },
};

use std::{collections::HashMap, sync::LazyLock};

pub(super) fn char_is_op(c: char) -> bool {
    if !c.is_ascii() {
        return c == '¬';
    }
    let ch = c as u8;

    (0x0021 <= ch && ch <= 0x002D)        // !"#$%&'()*+,-
        || ch == 0x002F                   // /
        || (0x003A <= ch && ch <= 0x0040) // :;<=>?@
        || (0x005B <= ch && ch <= 0x005E) // [\]
        || (0x007B <= ch && ch <= 0x007E) // {|}~
}
pub(super) fn handled_operator<F: FnMut(&mut Errors, Position, Token)>(
    errors: &mut Errors,
    op: &str,
    pos: Position,
    push: &mut F,
) -> bool {
    if let Some(op) = PREFIX_UNARY_OPS.get(op) {
        push(errors, pos, Token::PreUnary(op.clone()));
    } else if let Some(op) = BINARY_OPS.get(op) {
        push(errors, pos, Token::Binary(op.clone()));
    } else if let Some(op) = POSTFIX_UNARY_OPS.get(op) {
        push(errors, pos, Token::PostUnary(op.clone()));
    } else if let Some(op) = CHAINED_OPS.get(op) {
        push(errors, pos, Token::ChainedOp(op.clone()));
    } else {
        return false;
    }
    true
}
pub fn split_operator<F: FnMut(&mut Errors, Position, Token)>(
    errors: &mut Errors,
    op: &mut String,
    pos: Position,
    push: &mut F,
) {
    let mut stripped_chars = 0;
    let previous_len = op.len();
    let mut len = previous_len;
    loop {
        let mut operators_found = 0_usize;
        for i in (stripped_chars == 0) as usize..len {
            let slice = &op[stripped_chars..(len - i) + stripped_chars];
            if handled_operator(errors, slice, pos, push) {
                stripped_chars += slice.len();
                len = previous_len - stripped_chars;
                operators_found += 1;
                break;
            }
        }
        if operators_found == 0 {
            break;
        }
    }
    if len != 0 {
        errors.push(Error::new(
            pos,
            ErrorCode::UnknownOperator {
                operator: std::mem::take(op),
            },
        ))
    }
}
pub(super) static PREFIX_UNARY_OPS: LazyLock<HashMap<&'static str, PrefixUnaryOp>> =
    LazyLock::new(|| {
        HashMap::from([
            ("->", PrefixUnaryOp::Ref),
            ("!", PrefixUnaryOp::Not),
            ("-", PrefixUnaryOp::Neg),
            ("+", PrefixUnaryOp::Pos),
        ])
    });
pub(super) static POSTFIX_UNARY_OPS: LazyLock<HashMap<&'static str, PostfixUnaryOp>> =
    LazyLock::new(|| {
        HashMap::from([
            ("++", PostfixUnaryOp::Increment),
            ("--", PostfixUnaryOp::Decrement),
        ])
    });
pub(super) static BINARY_OPS: LazyLock<HashMap<&'static str, BinaryOp>> = LazyLock::new(|| {
    HashMap::from([
        ("=", Equation),
        ("+", Add),
        ("-", Sub),
        ("*", Mul),
        ("/", Div),
        ("%", Mod),
        ("|", BitwiseOr),
        ("&", BitwiseAnd),
        (">|", BitwiseXor),
        ("||", Or),
        ("&&", And),
        (":=", Write),
        ("=|=", Swap),
    ])
});
pub(super) static CHAINED_OPS: LazyLock<HashMap<&'static str, ChainedOp>> = LazyLock::new(|| {
    HashMap::from([
        ("==", Equal),
        ("!=", NonEqual),
        ("<", Smaller),
        (">=", GreaterOrEqual),
        (">", Greater),
        ("<=", SmallerOrEqual),
    ])
});
