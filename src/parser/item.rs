use crate::{
    parser::str_ref::NameSpace,
    tree::{NodeWrapping, Tree},
};

pub enum Item<Wrapper: NodeWrapping> {
    Function {
        generics: Generics<Wrapper>,
        arguments: Vec<(String, Tree<Wrapper>)>,
        return_type: Tree<Wrapper>,
        body: Option<Tree<Wrapper>>,
    },
    Distinct {
        generics: Generics<Wrapper>,
        val: Tree<Wrapper>,
    },
    Constant {
        generics: Generics<Wrapper>,
        val: Tree<Wrapper>,
    },
}
pub struct Generics<Wrapper: NodeWrapping> {
    parameter: NameSpace,
    constrains: Tree<Wrapper>,
}
