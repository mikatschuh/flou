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
// Controlling the parsers pointer stack
impl<Wrapper: NodeWrapping> PrattParser<Wrapper> {
    /// Obtains the current layer.
    #[inline]
    pub fn current(&self) -> Pointer {
        *self.parse_stack.layers.last()
    }
    /// Obtains the current node id.
    #[inline]
    pub fn current_node_id(&self) -> Option<NodeId> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            Some(*node)
        } else {
            None
        }
    }
    /// Obtains the current node.
    #[inline]
    pub fn current_node(&self) -> Option<&Node> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            node.get(&self.tree)
        } else {
            None
        }
    }

    /// Obtains the current node as mutable reference.
    #[inline]
    pub fn current_node_mut(&mut self) -> Option<&mut Node> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            node.get_mut(&mut self.tree).as_mut()
        } else {
            None
        }
    }
    /// Obtains the current wrapper.
    #[inline]
    pub fn current_wrapper(&self) -> Option<&Wrapper> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            Some(node.get_wrapper(&self.tree))
        } else {
            None
        }
    }
    /// Obtains a mutable reference to the current wrapper.
    #[inline]
    pub fn current_wrapper_mut(&mut self) -> Option<&mut Wrapper> {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            Some(node.get_wrapper_mut(&mut self.tree))
        } else {
            None
        }
    }
    #[inline]
    pub fn points_to_some_node(&self) -> bool {
        if let Pointer::Node(node) = self.parse_stack.layers.last() {
            node.is_non_empty(&self.tree)
        } else {
            false
        }
    }
    /// Returns the upper layer if there's one.
    #[inline]
    pub fn higher(&self) -> Option<Pointer> {
        self.parse_stack.layers.penultimate().copied()
    }
    /// Returns the upper layer's id if there's one.
    #[inline]
    pub fn higher_node_id(&self) -> Option<NonNullNodeId> {
        if let Some(Pointer::Node(node)) = self.parse_stack.layers.penultimate().copied() {
            Some(node.as_non_null(&self.tree))
        } else {
            None
        }
    }
    /// Returns the upper layer's node if there's one.
    #[inline]
    pub fn higher_node(&self) -> Option<&Node> {
        if let Some(Pointer::Node(node)) = self.parse_stack.layers.penultimate().copied() {
            node.get(&self.tree)
        } else {
            None
        }
    }
    /// Makes the upper layer the new layer.
    #[inline]
    pub fn move_up(&mut self) {
        self.parse_stack.layers.pop();
    }
    /// Moves down into a lower node.
    #[inline]
    pub fn move_down(&mut self, new: NodeId) {
        self.parse_stack.layers.push(Pointer::Node(new))
    }
    /// Moves down into a lower scope.
    #[inline]
    pub fn move_into_new_scope(&mut self, new: ScopeId) {
        self.parse_stack.layers.push(Pointer::Scope(new))
    }
}
