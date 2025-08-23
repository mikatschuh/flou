use crate::{
    parser::{intern::Symbol, Location, Parser},
    tree::{NodeId, NodeWrapping, Path},
};

pub struct Function<'src> {
    pub is_indexing: bool,
    pub generics: Generics<'src>,
    pub args: Vec<(NodeId<'src>, NodeId<'src>)>,
    pub return_type: NodeId<'src>,
    pub body: Option<Path<NodeId<'src>>>,
}

pub struct Generics<'src> {
    parameter: Vec<Symbol<'src>>,
    constrains: Option<NodeId<'src>>,
}

impl Generics<'_> {
    pub fn new() -> Self {
        Self {
            parameter: vec![],
            constrains: None,
        }
    }
}

impl<'src, W: NodeWrapping<'src>> Parser<'src, W> {
    fn parse_args<'caller>(&mut self, loc: Location<'src, 'caller>) {}
}
