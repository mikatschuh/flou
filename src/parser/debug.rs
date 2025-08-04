use crate::{parser::PrattParser, tree::NodeWrapping};
use std::fmt::Display;

impl<Wrapper: NodeWrapping> Display for PrattParser<Wrapper> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = "".to_owned();
        for layer in self.parse_stack.layers.iter() {
            println!(
                "\n--> {}",
                self.tree[*layer].display(&self.tree, "    ".to_owned())
            )
        }
        write!(f, "{}", out)
    }
}
