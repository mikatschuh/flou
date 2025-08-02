use crate::{
    error::{ErrorCode, Errors, Position},
    parser::{
        tokenizing::{
            binary_op::BinaryOp::{self, *},
            chained_op::ChainedOp::{self, *},
            unary_op::{PostfixUnaryOp, PrefixUnaryOp},
        },
        Token,
    },
};

use std::{collections::HashMap, sync::LazyLock, vec::IntoIter};

pub(super) fn char_is_op(c: char) -> bool {
    if "¬×÷·¡¿‹›«»•–—¦©†‡ˆ‰".contains(c) {
        return true;
    } else if !c.is_ascii() {
        return false;
    }
    let c = c as u8;

    (0x21 <= c && c <= 0x2D)        // !"#$%&'()*+,-
        || c == 0x2F                // /
        || (0x3A <= c && c <= 0x40) // :;<=>?@
        || (0x5B <= c && c <= 0x5E) // [\]^
        || (0x7B <= c && c <= 0x7E) // {|}~
}
pub(super) fn handled_operator(
    op: &str,
    pos: Position,
    mut push: impl FnMut(Position, Token),
) -> bool {
    if let Some(op) = PREFIX_UNARY_OPS.get(op) {
        push(pos, Token::PreUnary(op.clone()));
    } else if let Some(op) = BINARY_OPS.get(op) {
        push(pos, Token::Binary(op.clone()));
    } else if let Some(op) = POSTFIX_UNARY_OPS.get(op) {
        push(pos, Token::PostUnary(op.clone()));
    } else if let Some(op) = CHAINED_OPS.get(op) {
        push(pos, Token::ChainedOp(op.clone()));
    } else {
        return false;
    }
    true
}
pub fn split_operator(
    errors: &mut Errors,
    op: String,
    pos: Position,
) -> IntoIter<(Position, Token)> {
    let mut operators = vec![];
    let mut stripped_chars = 0;
    let previous_len = op.len();
    let mut len = previous_len;
    loop {
        let mut operators_found = 0_usize;
        for i in (stripped_chars == 0) as usize..len {
            let slice = &op[stripped_chars..(len - i) + stripped_chars];
            if handled_operator(slice, pos, |pos, token| operators.push((pos, token))) {
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
        errors.push(pos, ErrorCode::UnknownOperator { op });
    }
    operators.into_iter()
}
pub(super) static PREFIX_UNARY_OPS: LazyLock<HashMap<&'static str, PrefixUnaryOp>> =
    LazyLock::new(|| {
        HashMap::from([
            ("->", PrefixUnaryOp::Ref),
            ("!", PrefixUnaryOp::Not),
            ("-", PrefixUnaryOp::Neg),
            ("+", PrefixUnaryOp::Pos),
            ("'", PrefixUnaryOp::LfT),
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
        ("·", Dot),
        ("><", Cross),
        ("^", Power),
        ("|", BitOr),
        ("&", BitAnd),
        (">|", BitXor),
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
