use crate::{
    comp,
    error::{ErrorCode, Errors, Position, Span},
    parser::{
        binary_op::BinaryOp,
        intern::{Internalizer, Symbol},
        tokenizing::resolve_escape_sequences,
    },
    tree::{Bracket, Jump, Node, NodeId, NodeWrapper, NodeWrapping, Note, Path, Tree},
    utilities::{Rc, Ref},
};
use tokenizing::{
    token::{Token, TokenKind},
    Tokenizer,
};

pub mod binary_op;
mod binding_pow;
pub mod intern;
#[allow(dead_code)]
pub mod item;
#[allow(dead_code)]
pub mod keyword;
pub mod num;
#[cfg(test)]
mod test;
pub mod tokenizing;
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

pub fn parse<'src>(text: &'src str, path: &'static std::path::Path) -> (String, Rc<Errors<'src>>) {
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
            self.parse_path(Position::beginning(), 0, false).content,
            self.tree,
        )
    }

    fn parse_expr<'caller>(
        &mut self,
        min_bp: u8,
        flags: Flags<'src, 'caller>,
    ) -> Option<NodeId<'src>> {
        use Location::*;
        use TokenKind::*;
        let start: Position;
        let mut lhs = match self.tokenizer.peek()?.kind {
            Ident => {
                let Token { span, src, .. } = self.tokenizer.next().unwrap();
                start = span.start;
                if src.starts_with('.') {
                    if let ".." = src {
                        self.tree.add(W::new(span).with_node(Node::Placeholder))
                    } else {
                        self.tree.add(W::new(span).with_node(Node::Field(
                            self.internalizer.get(src.strip_prefix('.').unwrap()),
                        )))
                    }
                } else {
                    self.convert_to_num_if_possible(span, src, flags)
                }
            }
            Quote => {
                let Token { span, src, .. } = self.tokenizer.next().unwrap();
                start = span.start;
                let (string, confusions) = resolve_escape_sequences(src);
                self.tree.add(
                    W::new(span).with_node(Node::Quote(string)).with_notes(
                        confusions
                            .into_iter()
                            .map(Note::EscapeSequenceConfusion)
                            .collect(),
                    ),
                )
            }
            Keyword(keyword) => {
                let Token { span, src, .. } = self.tokenizer.next().unwrap();
                start = span.start;
                use keyword::Keyword::*;
                match keyword {
                    If | Loop => {
                        let condition = self.pop_expr(span.end + 1, 4, flags.outside_of_brackets());
                        let then_body = self.parse_path(span.end + 1, 4, flags.in_brackets);
                        let else_body = if self
                            .tokenizer
                            .peek()
                            .is_some_and(|x| x.kind == Keyword(Else))
                        {
                            let Token { span, .. } = self.tokenizer.next().unwrap();
                            Some(self.parse_path(span.end + 1, 4, flags.in_brackets))
                        } else {
                            None
                        };
                        self.tree.add(W::new(span).with_node(Node::Conditional {
                            condition,
                            looping: keyword == Loop,
                            then_body,
                            else_body,
                        }))
                    }
                    Else => {
                        self.errors.push(span, ErrorCode::LonelyElse);
                        self.parse_expr(min_bp, flags)?
                    }
                    Continue => match flags.loc {
                        Path(jump) => {
                            let layers = 1 + self // +1 because we already saw continue ones
                                .tokenizer
                                .consume_while(|token| token.kind == Keyword(Continue))
                                .count();

                            jump.write(Some(Box::new(Jump::Continue { layers })));
                            return None;
                        }
                        FuncArgType => {
                            self.errors
                                .push(span, ErrorCode::JumpInsideFuncArg { keyword: src });
                            self.parse_expr(min_bp, flags)?
                        }
                    },
                    Exit => match flags.loc {
                        Path(jump) => {
                            let layers = 1 + self
                                .tokenizer
                                .consume_while(|token| token.kind == Keyword(Exit))
                                .count();
                            jump.write(Some(Box::new(Jump::Exit {
                                layers,
                                val: self.parse_path(span.end + 1, 4, flags.in_brackets),
                            })));
                            return None;
                        }
                        FuncArgType => {
                            self.errors
                                .push(span, ErrorCode::JumpInsideFuncArg { keyword: src });
                            self.parse_expr(min_bp, flags)?
                        }
                    },
                    Break => todo!(),
                    Return => match flags.loc {
                        Path(jump) => {
                            jump.write(Some(Box::new(Jump::Return {
                                val: self.parse_path(span.end + 1, 4, flags.in_brackets),
                            })));
                            return None;
                        }
                        FuncArgType => {
                            self.errors
                                .push(span, ErrorCode::JumpInsideFuncArg { keyword: src });
                            self.parse_expr(min_bp, flags)?
                        }
                    },
                }
            }
            Tick => {
                let Token {
                    span: tick_span, ..
                } = self.tokenizer.next().unwrap();
                start = tick_span.start;
                let Some((ident_span, symbol)) = self.pop_identifier(tick_span.end + 1) else {
                    return self.parse_expr(min_bp, flags);
                };
                self.tree
                    .add(W::new(start - ident_span).with_node(Node::Lifetime(symbol)))
            }
            Open(own_bracket) => {
                let Token { span, .. } = self.tokenizer.next().unwrap();
                start = span.start;
                if let Some(content) =
                    self.parse_expr(0, flags.in_brackets_if(own_bracket != Bracket::Curly))
                {
                    let end = self.handle_closed_bracket(span.end + 1, span, own_bracket);
                    self.tree[content].span_mut().end = end.end;
                    content
                } else {
                    let end = self.handle_closed_bracket(span.end + 1, span, own_bracket);
                    self.tree.add(W::new(start - end).with_node(Node::Unit))
                }
            }
            Closed(..) => return None,
            kind => match kind.as_prefix() {
                Some(op) => {
                    let Token { span, .. } = self.tokenizer.next().unwrap();
                    start = span.start;
                    let operand = self.pop_expr(span.end + 1, op.binding_pow(), flags);
                    self.tree
                        .add(W::new(span).with_node(Node::Unary { op, val: operand }))
                }
                _ => {
                    start = self.tokenizer.peek().unwrap().span.start;
                    self.tree.add(W::new(start.into()))
                }
            },
        };
        while let Some(Token { kind, .. }) = self.tokenizer.peek() {
            if let Closed(..) = kind {
                return Some(lhs);
            } else if let Open(Bracket::Round | Bracket::Squared) = *kind {
                let bracket = match *kind {
                    Open(bracket) => bracket,
                    _ => unreachable!(),
                };
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    _ => unreachable!(),
                };
                if op.binding_pow().0 < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next().unwrap();
                let rhs = self.parse_expr(0, flags.in_brackets()).unwrap_or_else(|| {
                    self.tree
                        .add(W::new(span - (span + 1)).with_node(Node::Unit))
                });
                let end = self.handle_closed_bracket(span.end + 1, span, bracket);
                self.tree[rhs].span_mut().end = end.end;
                lhs = self.tree.add(W::new(start - end).with_node(Node::Binary {
                    op,
                    left: lhs,
                    right: rhs,
                }));
            } else if let Comma = *kind {
                let bp = if flags.in_brackets { (0, 1) } else { (15, 16) };
                if bp.0 < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next().unwrap();
                let rhs = self.pop_expr(span.end + 1, bp.1, flags);
                let mut chain = comp::Vec::new([lhs, rhs]);

                while let Some(Token { kind: Comma, .. }) = self.tokenizer.peek() {
                    let Token { span, .. } = self.tokenizer.next().unwrap();
                    chain.push(self.pop_expr(span.end + 1, bp.1, flags));
                }

                lhs = self.tree.add(
                    W::new(start - self.tree[*chain.last()].span()).with_node(Node::List(chain)),
                )
            } else if let Equal = kind {
                let bp = (6, 7);
                if bp.0 < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next().unwrap();
                let rhs = self.pop_expr(span.end + 1, bp.1, flags);
                lhs =
                    self.tree.add(
                        W::new(start - self.tree[rhs].span()).with_node(Node::Binding {
                            left: lhs,
                            right: rhs,
                        }),
                    );
            } else if let Some(op) = kind.as_infix() {
                if op.binding_pow().0 < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next().unwrap();
                let rhs = self.pop_expr(span.end + 1, op.binding_pow().1, flags);
                if op.is_chained() {
                    let mut chain = comp::Vec::new([(op, rhs)]);
                    while let Some(op) = self.tokenizer.peek().and_then(|op| op.kind.as_infix()) {
                        if op.binding_pow().0 < min_bp {
                            break;
                        };
                        if op.is_chained() {
                            let Token { span, .. } = self.tokenizer.next().unwrap();
                            let rhs = self.pop_expr(span.end + 1, op.binding_pow().1, flags);
                            chain.push((op, rhs));
                            continue;
                        }
                        break;
                    }
                    lhs = self
                        .tree
                        .add(W::new(start - self.tree[chain.last().1].span()).with_node(
                            Node::Chain {
                                first: lhs,
                                additions: chain,
                            },
                        ))
                } else {
                    lhs = self
                        .tree
                        .add(
                            W::new(start - self.tree[rhs].span()).with_node(Node::Binary {
                                op,
                                left: lhs,
                                right: rhs,
                            }),
                        );
                }
            } else if let Some(op) = kind.as_postfix() {
                if op.binding_pow() < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next().unwrap();
                lhs = self
                    .tree
                    .add(W::new(start - span).with_node(Node::Unary { op, val: lhs }));
            } else if let Colon = kind {
                let bp = (4, 5);
                if bp.0 < min_bp {
                    return Some(lhs);
                }
                let Token { span, .. } = self.tokenizer.next().unwrap();
                let rhs = self.pop_expr(span.end + 1, bp.1, flags);
                let mut chain = comp::Vec::new([lhs, rhs]);
                while self
                    .tokenizer
                    .peek()
                    .is_some_and(|token| token.kind == Colon)
                {
                    _ = self.tokenizer.next();
                    let rhs = self.parse_expr(bp.1, flags)?;
                    chain.push(rhs)
                }
                lhs = self.tree.add(
                    W::new(start - self.tree[*chain.last()].span())
                        .with_node(Node::ColonStruct(chain)),
                );
            } else {
                let bp = (2, 3);
                if bp.0 < min_bp {
                    return Some(lhs);
                }

                let Some(rhs) = self.parse_expr(bp.1, flags) else {
                    return Some(lhs);
                };
                let mut chain = comp::Vec::new([lhs, rhs]);
                while let Some(rhs) = self.parse_expr(bp.1, flags) {
                    chain.push(rhs)
                }
                lhs = self.tree.add(
                    W::new(start - self.tree[*chain.last()].span())
                        .with_node(Node::Statements(chain)),
                );
            }
        } // EOF

        Some(lhs)
    }

    #[inline]
    fn parse_path(&mut self, pos: Position, min_bp: u8, in_brackets: bool) -> Path<NodeId<'src>> {
        let mut jump = None;
        Path {
            content: self
                .parse_expr(
                    min_bp,
                    Flags {
                        in_brackets,
                        loc: Location::Path(Ref::new(&mut jump)),
                    },
                )
                .unwrap_or_else(|| self.tree.add(W::new(pos.into()))),
            jump,
        }
    }

    fn handle_closed_bracket(
        &mut self,
        pos: Position,
        own_span: Span,
        own_bracket: Bracket,
    ) -> Span {
        use TokenKind::*;
        if matches!(self.tokenizer.peek(), Some(Token { kind: Closed(bracket), .. }) if *bracket == own_bracket)
        {
            self.tokenizer.next().unwrap().span
        } else if let Some(Token {
            kind: Closed(found),
            ..
        }) = self.tokenizer.peek()
        {
            let found = *found;
            let span = self.tokenizer.next().unwrap().span;
            self.errors.push(
                own_span - span,
                ErrorCode::WrongClosedBracket {
                    expected: own_bracket,
                    found,
                },
            );
            span
        } else {
            self.errors.push(
                own_span,
                ErrorCode::NoClosedBracket {
                    opened: own_bracket,
                },
            );
            pos.into()
        }
    }

    /// Pops an identifier if the next token is one. If not it generates the correct error message
    /// and leaves the token there. The position indicates were the identifier is expected to go.
    fn pop_identifier(&mut self, pos: Position) -> Option<(Span, Symbol<'src>)> {
        if let Some(Token {
            kind, span, src, ..
        }) = self.tokenizer.peek()
        {
            if let TokenKind::Ident = kind {
                let Token { src, span, .. } = self.tokenizer.next().unwrap();
                Some((span, self.internalizer.get(src)))
            } else {
                self.errors
                    .push(*span, ErrorCode::ExpectedIdent { found: src });
                None
            }
        } else {
            self.errors
                .push(pos.into(), ErrorCode::ExpectedIdentFoundEOF);
            None
        }
    }

    /// Parses a value, if no value can be generated it makes an error message and returns an
    /// empty node.
    fn pop_expr<'caller>(
        &mut self,
        pos: Position,
        min_bp: u8,
        flags: Flags<'src, 'caller>,
    ) -> NodeId<'src> {
        self.parse_expr(min_bp, flags).unwrap_or_else(|| {
            self.errors.push(pos.into(), ErrorCode::ExpectedValue);
            self.tree.add(W::new(pos.into()))
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Flags<'src, 'caller> {
    pub in_brackets: bool,
    pub loc: Location<'src, 'caller>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Location<'src, 'caller> {
    Path(Ref<'caller, Option<Box<Jump<NodeId<'src>>>>>),
    FuncArgType,
}

impl<'src, 'caller> Flags<'src, 'caller> {
    #[inline]
    const fn new(jump: Ref<'caller, Option<Box<Jump<NodeId<'src>>>>>) -> Self {
        Self {
            in_brackets: false,
            loc: Location::Path(jump),
        }
    }
    fn in_brackets(mut self) -> Self {
        self.in_brackets = true;
        self
    }
    fn outside_of_brackets(mut self) -> Self {
        self.in_brackets = false;
        self
    }
    fn in_brackets_if(mut self, condition: bool) -> Self {
        self.in_brackets = condition;
        self
    }
    fn location(mut self, loc: Location<'src, 'caller>) -> Self {
        self.loc = loc;
        self
    }
}
