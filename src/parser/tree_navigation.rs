use crate::{
    comp,
    parser::PrattParser,
    tree::{Node, NodeId, NodeWrapping, NonNullNodeId},
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
    pub layers: comp::Vec<NodeId, 1>,
}
impl ParseStack {
    pub fn new(base_layer: NodeId) -> Self {
        Self {
            layers: comp::Vec::new([base_layer]),
        }
    }
}
pub trait TreeNavi<Wrapper: NodeWrapping> {
    /// Obtains the current layer.
    fn current(&self) -> NodeId;
    /// Obtains the current node.
    fn current_node(&self) -> Option<&Node>;
    /// Obtains the current node as mutable reference.
    fn current_node_mut(&mut self) -> Option<&mut Node>;
    /// Obtains the current wrapper.
    fn current_wrapper(&self) -> &Wrapper;
    /// Obtains a mutable reference to the current wrapper.
    fn current_wrapper_mut(&mut self) -> &mut Wrapper;
    /// Returns the upper layer if there's one.
    fn points_to_some_node(&self) -> bool;
    fn higher(&self) -> Option<NodeId>;
    /// Returns the upper layer's node if there's one.
    fn higher_node(&self) -> Option<&Node>;
    /// Makes the upper layer the new layer.
    fn move_up(&mut self);
    /// Moves down into a lower node.
    fn move_down(&mut self, new: NodeId);
}
// Controlling the parsers pointer stack
impl<Wrapper: NodeWrapping> TreeNavi<Wrapper> for PrattParser<Wrapper> {
    #[inline]
    fn current(&self) -> NodeId {
        *self.parse_stack.layers.last()
    }
    #[inline]
    fn current_node(&self) -> Option<&Node> {
        self.parse_stack.layers.last().get(&self.tree)
    }
    #[inline]
    fn current_node_mut(&mut self) -> Option<&mut Node> {
        self.parse_stack
            .layers
            .last()
            .get_mut(&mut self.tree)
            .as_mut()
    }
    #[inline]
    fn current_wrapper(&self) -> &Wrapper {
        self.parse_stack.layers.last().get_wrapper(&self.tree)
    }
    #[inline]
    fn current_wrapper_mut(&mut self) -> &mut Wrapper {
        self.parse_stack
            .layers
            .last()
            .get_wrapper_mut(&mut self.tree)
    }
    #[inline]
    fn points_to_some_node(&self) -> bool {
        self.parse_stack.layers.last().is_non_empty(&self.tree)
    }
    #[inline]
    fn higher(&self) -> Option<NodeId> {
        self.parse_stack.layers.penultimate().copied()
    }
    #[inline]
    fn higher_node(&self) -> Option<&Node> {
        if let Some(node) = self.parse_stack.layers.penultimate().copied() {
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
        self.parse_stack.layers.push(new)
    }
}
