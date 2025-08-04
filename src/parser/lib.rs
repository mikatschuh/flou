use crate::{
    error::{ErrorCode, Errors, Position},
    parser::{
        num::value_to_node,
        tokenizing::binary_op::{BinaryOp, BindingPow},
        tree_navigation::TreeNavi,
        Getting, Parser, PrattParser,
    },
    tree::{Bracket, Node, NodeId, NodeWrapping},
    unpack, Formatter,
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
    fn reallocate(&mut self, id: NodeId) -> NodeId {
        self.tree.move_to_new_location(id)
    }
    #[inline]
    fn value_to_node(&mut self, string: String, pos: Position) -> Wrapper {
        value_to_node(string, pos, &mut self.tree)
    }
    fn add_val(&mut self, val: Wrapper) {
        if self.points_to_some_node() {
            self.go_to_binding_pow(0);
            match self.current_node().unwrap() {
                Node::Statements(..) => {
                    let id = self.tree.add(val);
                    unpack!(self.current_node_mut() => Some(Node::Statements(content)) => content.push(id));
                    self.move_down(id);
                }
                _ => {
                    self.make_binary_operator(val.pos(), |left, right| {
                        Node::Statements([left, right].into())
                    });
                    *self.current_wrapper_mut() = val
                }
            }
        } else {
            if let Some(higher) = self.higher() {
                higher
                    .get_wrapper_mut(&mut self.tree)
                    .pos_mut()
                    .set_end(val.pos().end_line, val.pos().end_char);
            }
            *self.current_wrapper_mut() = val;
        }
    }
    fn go_to_binding_pow(&mut self, binding_pow: i8) {
        while let Some(higher) = self.higher_node() {
            match higher {
                Node::BinaryOp { op, .. }
                    if op.binding_pow() >= binding_pow
                        && !matches!(op, BinaryOp::App | BinaryOp::Index) =>
                {
                    self.move_up()
                }
                Node::UnaryOp { op, .. } if op.binding_pow() >= binding_pow && !op.is_postfix() => {
                    self.move_up()
                }
                Node::List(..) if 3 >= binding_pow => self.move_up(),
                Node::ColonStruct(..) if 0 >= binding_pow => self.move_up(),
                Node::ChainedOp { additions, .. }
                    if additions.last().0.binding_pow() >= binding_pow =>
                {
                    self.move_up()
                }
                Node::Statements(..) if 0 >= binding_pow => self.move_up(),
                _ => return,
            }
        }
    }
    fn make_binary_operator(
        &mut self,
        pos: Position,
        binary_operator: impl Fn(NodeId, NodeId) -> Node,
    ) {
        let left = self.current();
        let right = self.tree.add(Wrapper::new(pos.only_end() + 1));
        self.tree[left] = Wrapper::new(left.get_wrapper(&self.tree).pos() | pos + 1)
            .with_node(binary_operator(self.tree.move_to_new_location(left), right));
        self.move_down(right);
    }
    fn handle_closed_bracket(&mut self, pos: Position, bracket: Bracket) {
        while let Some(higher) = self.higher() {
            match higher.get(&self.tree).unwrap() // unwrapping is ok since this is the higher layer
            {
                Node::BinaryOp { op, .. }
                if (*op == BinaryOp::App && bracket == Bracket::Round) || (*op == BinaryOp::Index && bracket == Bracket::Squared)=> {
                    self.move_up();
                    self.current_wrapper_mut()
                        .pos_mut()
                        .set_end(pos.end_line, pos.end_char);
                    return;
                }
                Node::BinaryOp { op, .. } if matches!(op, BinaryOp::App | BinaryOp::Index) => {
                    self.errors.push(
                        pos,
                        ErrorCode::WrongClosedBracket {
                            expected: if *op == BinaryOp::App { ")" } else { "]" }.to_owned(),
                            found: bracket.display_closed().to_owned(),
                        },
                    );
                    self.move_up();
                    self.current_wrapper_mut()
                        .pos_mut()
                        .set_end(pos.end_line, pos.end_char);
                    return;
                }
                Node::Brackets { kind, .. }
                    if (*kind == bracket)
                    || (*kind == bracket)
                    || (*kind == bracket)  =>
                {
                    self.move_up();
                    self.current_wrapper_mut()
                        .pos_mut()
                        .set_end(pos.end_line, pos.end_char);
                    return;
                }
                Node::Brackets { kind, .. } => {
                    self.errors.push(
                        pos,
                        ErrorCode::WrongClosedBracket {
                            expected: kind.display_closed().to_owned(),
                            found: bracket.display_closed().to_owned(),
                        },
                    );
                    self.move_up();
                    self.current_wrapper_mut()
                        .pos_mut()
                        .set_end(pos.end_line, pos.end_char);
                    return;
                }
                _ => self.move_up(),
            }
        }
        self.errors.push(
            pos,
            ErrorCode::NoOpenedBracket {
                closed: bracket.display_closed().to_owned(),
            },
        );
    }
}
