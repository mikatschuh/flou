use std::{mem::take, vec::IntoIter};

use crate::{
    error::{ErrorCode, Errors, Position},
    parser::{
        num::value_to_node,
        tokenizing::binary_op::{BinaryOp, BindingPow},
        tree_navigation::{Pointer, TreeNavi},
        Getting, Parser, PrattParser, Token,
    },
    tree::{Node, NodeId, NodeWrapping, ScopeId},
    Formatter,
};
impl<Wrapper: NodeWrapping> Getting<Wrapper> for PrattParser<Wrapper> {
    fn formatter(&self) -> Formatter {
        self.formatter
    }
    fn errors(&mut self) -> &mut Errors {
        &mut self.errors
    }
}
impl<Wrapper: NodeWrapping> Parser<Wrapper> for PrattParser<Wrapper> {
    #[inline]
    fn add(&mut self, val: Wrapper) -> NodeId {
        self.tree.add(val)
    }
    #[inline]
    fn set(&mut self, id: NodeId, val: Wrapper) {
        self.tree[id] = val
    }
    #[inline]
    fn get(&self, id: NodeId) -> &Wrapper {
        &self.tree[id]
    }
    #[inline]
    fn push(&mut self, scope: ScopeId, val: Wrapper) -> NodeId {
        let id = self.add(val);
        self.tree[scope].push(id);
        id
    }
    #[inline]
    fn reallocate(&mut self, id: NodeId) -> NodeId {
        self.tree.move_to_new_location(id)
    }
    #[inline]
    fn add_scope(&mut self) -> ScopeId {
        self.tree.add_scope()
    }
    #[inline]
    fn value_to_node(&mut self, string: String, pos: Position) -> Wrapper {
        value_to_node(string, pos, &mut self.tree)
    }
    #[inline]
    fn buffer(&mut self) -> &mut Vec<(Position, Token)> {
        &mut self.token_buffer
    }
    #[inline]
    fn empty_buffer(&mut self) -> IntoIter<(Position, Token)> {
        take(&mut self.token_buffer).into_iter()
    }
    fn add_val(&mut self, val: Wrapper) {
        loop {
            match self.current() {
                Pointer::Node(node) => match self.tree[node].node() {
                    Some(_) => self.move_up(),
                    None => {
                        if let Some(higher) = self.higher_node_id() {
                            higher
                                .get_wrapper_mut(&mut self.tree)
                                .pos_mut()
                                .set_end(val.pos().end_line, val.pos().end_char);
                        }
                        self.tree[node] = val;
                        return;
                    }
                },
                Pointer::Scope(scope) => {
                    let id = self.tree.add(val);
                    self.tree[scope].push(id);
                    self.move_down(id);
                    return;
                }
            }
        }
    }
    fn go_to_binding_pow(&mut self, binding_pow: f32) {
        while let Some(higher) = self.higher_node() {
            match higher {
                Node::BinaryOp { op, .. } if op.binding_pow() >= binding_pow => self.move_up(),
                Node::UnaryOp { op, .. } if op.binding_pow() >= binding_pow && !op.is_postfix() => {
                    self.move_up()
                }
                Node::List(..) if 2.0 >= binding_pow => self.move_up(),
                Node::ColonStruct(..) if -1.0 >= binding_pow => self.move_up(),
                Node::ChainedOp { additions, .. }
                    if additions.last().0.binding_pow() >= binding_pow =>
                {
                    self.move_up()
                }
                _ => return,
            }
        }
    }
    fn make_binary_operator(
        &mut self,
        pos: Position,
        binary_operator: impl Fn(NodeId, NodeId) -> Node,
    ) {
        let left = self.current_node_id().unwrap();
        let right = self.tree.add(Wrapper::new(pos.only_end() + 1));
        self.tree[left] = Wrapper::new(left.get_wrapper(&self.tree).pos() | pos + 1)
            .with_node(binary_operator(self.tree.move_to_new_location(left), right));
        self.move_down(right);
    }
    fn handle_closed_bracket(&mut self, pos: Position, bracket: &str) {
        while let Some(higher) = self.higher() {
            match higher {
                Pointer::Node(node) => {
                    match node.get(&self.tree).unwrap() // unwrapping is ok since this is the higher layer
                {
                    Node::BinaryOp { op, .. }
                    if (*op == BinaryOp::App && bracket == ")" || *op == BinaryOp::Index && bracket == "]")=> {
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::BinaryOp { op, .. } if matches!(op, BinaryOp::App | BinaryOp::Index) => {
                        self.errors.push(
                            pos,
                            ErrorCode::WrongClosedBracket {
                                expected: if *op == BinaryOp::App { ")" } else { "]" }.to_owned(),
                                found: bracket.to_owned(),
                            },
                        );
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::Brackets { squared, .. }
                        if *squared && bracket == "]" || !squared && bracket == ")" =>
                    {
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::Brackets { squared, .. } => {
                        self.errors.push(
                            pos,
                            ErrorCode::WrongClosedBracket {
                                expected: if *squared { "]" } else { ")" }.to_owned(),
                                found: bracket.to_owned(),
                            },
                        );
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::Scope(..) if bracket == "}" => {
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    Node::Scope(..) => {
                        self.errors.push(
                            pos,
                            ErrorCode::WrongClosedBracket {
                                expected: "}".to_owned(),
                                found: bracket.to_owned(),
                            },
                        );
                        self.move_up();
                        self.current_wrapper_mut()
                            .unwrap()
                            .pos_mut()
                            .set_end(pos.end_line, pos.end_char);
                        return;
                    }
                    _ => self.move_up(),
                }
                }
                Pointer::Scope(..) => self.move_up(),
            }
        }
        self.errors.push(
            pos,
            ErrorCode::NoOpenedBracket {
                closed: bracket.to_owned(),
            },
        );
    }
}
