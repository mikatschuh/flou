use crate::{
    parser::PrattParser,
    tree::{Node, NodeId, NodeWrapping, NonNullNodeId, ScopeId},
    utilities::NonEmptyVec,
};
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

pub struct ParseStack {
    layers: NonEmptyVec<Pointer>,
}
impl ParseStack {
    pub fn new(base_layer: Pointer) -> Self {
        Self {
            layers: NonEmptyVec::new(base_layer),
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub enum Pointer {
    Scope(ScopeId),
    Node(NodeId),
}
impl Pointer {
    #[inline]
    pub fn unwrap(self) -> NodeId {
        unpack!(self => Pointer::Node(id) => id)
    }
}
pub trait TreeNavi<Wrapper: NodeWrapping> {
    /// Obtains the current layer.
    fn current(&self) -> Pointer;
    /// Obtains the current node id.
    fn current_node_id(&self) -> Option<NodeId>;
    /// Obtains the current node.
    fn current_node(&self) -> Option<&Node>;
    /// Obtains the current node as mutable reference.
    fn current_node_mut(&mut self) -> Option<&mut Node>;
    /// Obtains the current wrapper.
    fn current_wrapper(&self) -> Option<&Wrapper>;
    /// Obtains a mutable reference to the current wrapper.
    fn current_wrapper_mut(&mut self) -> Option<&mut Wrapper>;
    /// Returns the upper layer if there's one.
    fn points_to_some_node(&self) -> bool;
    fn higher(&self) -> Option<Pointer>;
    /// Returns the upper layer's id if there's one.
    fn higher_node_id(&self) -> Option<NonNullNodeId>;
    /// Returns the upper layer's node if there's one.
    fn higher_node(&self) -> Option<&Node>;
    /// Makes the upper layer the new layer.
    fn move_up(&mut self);
    /// Moves down into a lower node.
    fn move_down(&mut self, new: NodeId);
    /// Moves down into a lower scope.
    fn move_into_new_scope(&mut self, new: ScopeId);
}
// Controlling the parsers pointer stack
impl<Wrapper: NodeWrapping> TreeNavi<Wrapper> for PrattParser<Wrapper> {
    #[inline]
    fn current(&self) -> Pointer {
        *self.parse_stack.layers.last()
    }
    #[inline]
    fn current_node_id(&self) -> Option<NodeId> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            Some(*node)
        } else {
            None
        }
    }
    #[inline]
    fn current_node(&self) -> Option<&Node> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            node.get(&self.tree)
        } else {
            None
        }
    }
    #[inline]
    fn current_node_mut(&mut self) -> Option<&mut Node> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            node.get_mut(&mut self.tree).as_mut()
        } else {
            None
        }
    }
    #[inline]
    fn current_wrapper(&self) -> Option<&Wrapper> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            Some(node.get_wrapper(&self.tree))
        } else {
            None
        }
    }
    #[inline]
    fn current_wrapper_mut(&mut self) -> Option<&mut Wrapper> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            Some(node.get_wrapper_mut(&mut self.tree))
        } else {
            None
        }
    }
    #[inline]
    fn points_to_some_node(&self) -> bool {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            node.is_non_empty(&self.tree)
        } else {
            false
        }
    }
    #[inline]
    fn higher(&self) -> Option<Pointer> {
        self.parse_stack.layers.penultimate().copied()
    }
    #[inline]
    fn higher_node_id(&self) -> Option<NonNullNodeId> {
        if let Some(Pointer::Node(node)) = self.parse_stack.layers.penultimate().copied() {
            Some(node.as_non_null(&self.tree))
        } else {
            None
        }
    }
    #[inline]
    fn higher_node(&self) -> Option<&Node> {
        if let Some(Pointer::Node(node)) = self.parse_stack.layers.penultimate().copied() {
            node.get(&self.tree)
        } else {
            None
        }
    }
    #[inline]
    fn move_up(&mut self) {
        self.parse_stack.layers.pop();
    }
    #[inline]
    fn move_down(&mut self, new: NodeId) {
        self.parse_stack.layers.push(Pointer::Node(new))
    }
    #[inline]
    fn move_into_new_scope(&mut self, new: ScopeId) {
        self.parse_stack.layers.push(Pointer::Scope(new))
    }
}
