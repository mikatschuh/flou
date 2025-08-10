use crate::{
    comp,
    error::{ErrorCode, Errors},
    parser::{intern::Internalizer, tokenizing::resolve_escape_sequences},
    tree::{Bracket, Node, NodeId, NodeWrapper, NodeWrapping, Note, Tree},
    utilities::Rc,
};
use std::path::Path;
use tokenizing::{
    token::{Token, TokenKind},
    Tokenizer,
};

#[allow(dead_code)]
pub mod binary_op;
mod binding_pow;
#[allow(dead_code)]
pub mod intern;
#[allow(dead_code)]
pub mod item;
#[allow(dead_code)]
pub mod keyword;
#[cfg(test)]
mod test;
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

    let mut parser = Parser::<NodeWrapper>::new(
        Tokenizer::new(text, errors.clone()),
        internalizer.clone(),
        errors.clone(),
    );
    let root = parser.parse_expr(0).unwrap();

    (parser.tree().to_string(root, &internalizer), errors)
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
    fn tree(self) -> Tree<'src, W> {
        self.tree
    }
    fn parse_expr(&mut self, min_bp: u8) -> Option<NodeId<'src>> {
        use TokenKind::*;
        let mut token = self.tokenizer.next()?;
        let mut lhs = match token.kind {
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
            OpenParen => {
                let content = self.parse_expr(0)?; // lowest except for commas
                if let Some(Token {
                    kind: ClosedParen, ..
                }) = self.tokenizer.peek()
                {
                    let span = self.tokenizer.next().unwrap().span;
                    *self.tree[content].span_mut() = token.span - span;
                    content
                } else if let Some(Token {
                    kind: ClosedBracket,
                    ..
                }) = self.tokenizer.peek()
                {
                    let span = self.tokenizer.next().unwrap().span;
                    self.errors.push(
                        token.span - span,
                        ErrorCode::WrongClosedBracket {
                            expected: Bracket::Round,
                            found: Bracket::Squared,
                        },
                    );
                    content
                } else {
                    self.errors.push(
                        token.span,
                        ErrorCode::NoClosedBracket {
                            opened: Bracket::Round,
                        },
                    );
                    content
                }
            }
            _ => match token.kind.as_prefix() {
                Some(op) => {
                    let operand = self.parse_expr(op.binding_pow())?;
                    self.tree
                        .add(W::new(token.span, Node::Unary { op, operand }))
                }
                _ => todo!(),
            },
        };
        loop {
            let Some(Token { kind: op, .. }) = self.tokenizer.peek() else {
                break;
            };

            if let Some(op) = op.as_infix() {
                if op.binding_pow().0 < min_bp {
                    break;
                }
                token = self.tokenizer.next()?;
                let rhs = self.parse_expr(op.binding_pow().1)?;
                if op.is_chained() {
                    let mut chain = comp::Vec::new([(op, rhs); 1]);
                    loop {
                        if let Some(op) = self.tokenizer.peek().and_then(|op| op.kind.as_infix()) {
                            if op.binding_pow().0 < min_bp {
                                break;
                            };
                            if op.is_chained() {
                                token = self.tokenizer.next()?;
                                let rhs = self.parse_expr(op.binding_pow().1)?;
                                chain.push((op, rhs));
                                continue;
                            }
                        }
                        break;
                    }
                    lhs = self.tree.add(W::new(
                        token.span,
                        Node::Chain {
                            first: lhs,
                            additions: chain,
                        },
                    ))
                } else {
                    lhs = self.tree.add(W::new(
                        token.span,
                        Node::Binary {
                            op,
                            left: lhs,
                            right: rhs,
                        },
                    ));
                }
            } else if let Some(op) = op.as_postfix() {
                if op.binding_pow() < min_bp {
                    break;
                }
                token = self.tokenizer.next()?;
                lhs = self
                    .tree
                    .add(W::new(token.span, Node::Unary { op, operand: lhs }));
            } else {
                break;
            }
        }
        Some(lhs)
    }
}
