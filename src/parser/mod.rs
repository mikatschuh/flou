use crate::{error::Errors, utilities::Rc};
use std::path::Path;
use tokenizing::Tokenizer;

#[allow(dead_code)]
pub mod binary_op;
#[allow(dead_code)]
pub mod chained_op;
#[allow(dead_code)]
pub mod item;
#[allow(dead_code)]
pub mod keyword;
#[allow(dead_code)]
pub mod num;
#[allow(dead_code)]
pub mod str_ref;
#[allow(dead_code)]
pub mod tokenizing;
#[allow(dead_code)]
pub mod unary_op;

#[macro_export]
macro_rules! unpack {
    ($pat:pat = $expr:expr => $body:expr) => {
        if let $pat = $expr {
            $body
        } else {
            unreachable!()
        }
    };
    ($expr:expr => $pat:pat => $body:expr) => {
        if let $pat = $expr {
            $body
        } else {
            unreachable!()
        }
    };
}

#[allow(unused)]
pub fn parse<'a>(text: &'a str, path: &'static Path) -> (String, Rc<Errors<'a>>) {
    let errors = Rc::new(Errors::empty(path));

    let mut tokenizer = Tokenizer::new(text, errors.clone());

    (todo!().to_string(), errors)
}
