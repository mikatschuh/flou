use crate::{
    comp,
    error::{ErrorCode, Errors, Span},
    parser::{binary_op::BinaryOp, intern::Internalizer, tokenizing::resolve_escape_sequences},
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

    let (root, tree) = Parser::<NodeWrapper>::new(
        Tokenizer::new(text, errors.clone()),
        internalizer.clone(),
        errors.clone(),
    )
    .parse();

    (tree.to_string(root, &internalizer), errors)
}

struct Parser<'src, W: NodeWrapping<'src>> {
    tokenizer: Tokenizer<'src>,
    errors: Rc<Errors<'src>>,
    internalizer: Rc<Internalizer<'src>>,
    tree: Tree<'src, W>,
}
impl<'src, W: NodeWrapping<'src> + 'src> Parser<'src, W> {
    #[inline]
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
    #[inline]
    fn parse(mut self) -> (NodeId<'src>, Tree<'src, W>) {
        (
            self.parse_expr(0, InsideOfBrackets::False).unwrap(),
            self.tree,
        )
    }

    fn parse_expr(
        &mut self,
        min_bp: u8,
        inside_of_brackets: InsideOfBrackets,
    ) -> Option<NodeId<'src>> {
        use InsideOfBrackets::*;
        use TokenKind::*;
        let token = self.tokenizer.next()?;
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
            Open(own_bracket) => {
                let content = self.parse_expr(0, BeforeComma)?; // lowest except for commas
                self.handle_closed_bracket(content, token.span, own_bracket);
                content
            }
            _ => match token.kind.as_prefix() {
                Some(op) => {
                    let operand = self.parse_expr(op.binding_pow(), False)?;
                    self.tree
                        .add(W::new(token.span, Node::Unary { op, operand }))
                }
                _ => todo!(),
            },
        };
        while let Some(Token { kind, .. }) = self.tokenizer.peek() {
            if let Closed(..) = kind {
                return Some(lhs);
            } else if let Open(bracket) = *kind {
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    Bracket::Curly => todo!(),
                };
                if op.binding_pow().0 < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next()?;
                let rhs = self.parse_expr(0, BeforeComma)?;
                self.handle_closed_bracket(rhs, span, bracket);
                lhs = self.tree.add(W::new(
                    span,
                    Node::Binary {
                        op,
                        left: lhs,
                        right: rhs,
                    },
                ));
            } else if let Comma = *kind {
                match inside_of_brackets {
                    False => {
                        if 6 < min_bp {
                            return Some(lhs);
                        }
                        let Token { span, .. } = self.tokenizer.next()?;
                        let rhs = self.parse_expr(7, False)?;
                        let mut chain = comp::Vec::new([lhs, rhs]);
                        while let Some(Token { kind: Comma, .. }) = self.tokenizer.peek() {
                            _ = self.tokenizer.next();
                            let rhs = self.parse_expr(7, False)?;
                            chain.push(rhs)
                        }
                        // let span = chain.first().get_wrapper(&self.tree).span()
                        //    - chain.last().get_wrapper(&self.tree).span();
                        lhs = self.tree.add(W::new(span, Node::List(chain)))
                    }
                    BeforeComma => {}
                    AfterComma => return Some(lhs),
                }
            } else if let Some(op) = kind.as_infix() {
                if op.binding_pow().0 < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next()?;
                let rhs = self.parse_expr(op.binding_pow().1, False)?;
                if op.is_chained() {
                    let mut chain = comp::Vec::new([(op, rhs)]);
                    while let Some(op) = self.tokenizer.peek().and_then(|op| op.kind.as_infix()) {
                        if op.binding_pow().0 < min_bp {
                            break;
                        };
                        if op.is_chained() {
                            _ = self.tokenizer.next()?;
                            let rhs = self.parse_expr(op.binding_pow().1, False)?;
                            chain.push((op, rhs));
                            continue;
                        }
                        break;
                    }
                    lhs = self.tree.add(W::new(
                        span,
                        Node::Chain {
                            first: lhs,
                            additions: chain,
                        },
                    ))
                } else {
                    lhs = self.tree.add(W::new(
                        span,
                        Node::Binary {
                            op,
                            left: lhs,
                            right: rhs,
                        },
                    ));
                }
            } else if let Some(op) = kind.as_postfix() {
                if op.binding_pow() < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next()?;
                lhs = self
                    .tree
                    .add(W::new(span, Node::Unary { op, operand: lhs }));
            } else {
                return Some(lhs);
            }
        }
        // EOF
        Some(lhs)
    }
    fn handle_closed_bracket(
        &mut self,
        content: NodeId<'src>,
        own_span: Span,
        own_bracket: Bracket,
    ) {
        use TokenKind::*;
        if matches!(self.tokenizer.peek(), Some(Token { kind: Closed(bracket), .. }) if *bracket == own_bracket)
        {
            let span = self.tokenizer.next().unwrap().span;
            *self.tree[content].span_mut() = own_span - span;
        } else if matches!(
            self.tokenizer.peek(),
            Some(Token {
                kind: Closed(..),
                ..
            })
        ) {
            let span = self.tokenizer.next().unwrap().span;
            *self.tree[content].span_mut() = own_span - span;
            self.errors.push(
                own_span - span,
                ErrorCode::WrongClosedBracket {
                    expected: Bracket::Round,
                    found: Bracket::Squared,
                },
            );
        } else {
            self.errors.push(
                own_span,
                ErrorCode::NoClosedBracket {
                    opened: Bracket::Round,
                },
            );
        }
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum InsideOfBrackets {
    False,
    BeforeComma,
    AfterComma,
}
