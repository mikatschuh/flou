use crate::{
    parser::intern::Internalizer,
    tree::{NodeWrapping, Tree},
};

pub enum Item<'src, Wrapper: NodeWrapping<'src>> {
    Function {
        generics: Generics<'src, Wrapper>,
        arguments: Vec<(String, Tree<'src, Wrapper>)>,
        return_type: Tree<'src, Wrapper>,
        body: Option<Tree<'src, Wrapper>>,
    },
    Distinct {
        generics: Generics<'src, Wrapper>,
        val: Tree<'src, Wrapper>,
    },
    Constant {
        generics: Generics<'src, Wrapper>,
        val: Tree<'src, Wrapper>,
    },
}
pub struct Generics<'src, Wrapper: NodeWrapping<'src>> {
    parameter: Internalizer<'src>,
    constrains: Tree<'src, Wrapper>,
}
