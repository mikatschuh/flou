use crate::{
    parser::{tree_navigation::Pointer, PrattParser},
    tree::NodeWrapping,
};
use std::fmt::Display;

impl<Wrapper: NodeWrapping> Display for PrattParser<Wrapper> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = "".to_owned();
        for layer in self.parse_stack.layers.iter() {
            println!(
                "\n--> {}",
                match layer {
                    Pointer::Node(node) => {
                        self.tree[*node].display(&self.tree, "    ".to_owned())
                    }
                    Pointer::Scope(scope) => {
                        self.tree[*scope].display(&self.tree, "    ".to_owned())
                    }
                }
            )
        }
        write!(f, "{}", out)
    }
}
