use crate::{
    error::{Error, Errors},
    parser::{intern::Internalizer, tokenizing::resolve_escape_sequences},
    tree::{Node, NodeWrapper, NodeWrapping, Note, Tree},
    utilities::Rc,
};
use std::path::Path;
use tokenizing::{
    token::{Token, TokenKind},
    Tokenizer,
};

#[allow(dead_code)]
pub mod binary_op;
#[allow(dead_code)]
pub mod chained_op;
#[allow(dead_code)]
pub mod intern;
#[allow(dead_code)]
pub mod item;
#[allow(dead_code)]
pub mod keyword;
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

pub fn parse<'src>(text: &'src str, path: &'static Path) -> (String, Rc<Errors<'src>>) {
    let errors = Rc::new(Errors::empty(path));
    let internalizer = Rc::new(Internalizer::new());

    let tree: Tree<NodeWrapper> = Parser::new(
        Tokenizer::new(text, errors.clone()),
        internalizer.clone(),
        errors.clone(),
    )
    .parse_expr();
    (tree.to_string(&internalizer), errors)
}

struct Parser<'src, W: NodeWrapping<'src>> {
    tokenizer: Tokenizer<'src>,
    errors: Rc<Errors<'src>>,
    internalizer: Rc<Internalizer<'src>>,
    tree: Tree<'src, W>,
}
impl<'src, W: NodeWrapping<'src>> Parser<'src, W> {
    fn new(
        tokenizer: Tokenizer<'src>,
        internalizer: Rc<Internalizer<'src>>,
        errors: Rc<Errors<'src>>,
    ) -> Self {
        Self {
            tokenizer,
            errors,
            internalizer,
            tree: Tree::new(),
        }
    }
    fn parse_expr(&mut self) -> Tree<'src, W> {
        use TokenKind::*;
        let mut lhs = match self.tokenizer.next() {
            Some(token) => match token.kind {
                Ident => self.tree.add(W::new(
                    token.span,
                    Node::Ident(self.internalizer.get(token.src)),
                )),
                Quote => {
                    let (string, confusions) = resolve_escape_sequences(token.src);
                    self.tree.add(
                        W::new(token.span, Node::Quote(string)).add_notes(
                            confusions
                                .into_iter()
                                .map(|confusion| Note::EscapeSequenceConfusion(confusion))
                                .collect(),
                        ),
                    )
                }
                _ => todo!(),
            },
            None => {
                todo!()
            }
        };
        todo!()
    }
}
