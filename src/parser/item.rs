use crate::{
    parser::str_ref::Internalizer,
    tree::{NodeWrapping, Tree},
};

pub enum Item<'src, Wrapper: NodeWrapping> {
    Function {
        generics: Generics<'src, Wrapper>,
        arguments: Vec<(String, Tree<Wrapper>)>,
        return_type: Tree<Wrapper>,
        body: Option<Tree<Wrapper>>,
    },
    Distinct {
        generics: Generics<'src, Wrapper>,
        val: Tree<Wrapper>,
    },
    Constant {
        generics: Generics<'src, Wrapper>,
        val: Tree<Wrapper>,
    },
}
pub struct Generics<'src, Wrapper: NodeWrapping> {
    parameter: Internalizer<'src>,
    constrains: Tree<Wrapper>,
}
