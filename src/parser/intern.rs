use std::{collections::HashMap, marker::PhantomData};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol<'internalizer> {
    _marker: PhantomData<&'internalizer ()>,
    idx: usize,
}

pub struct Internalizer<'src> {
    map: HashMap<&'src str, Symbol<'src>>,
    vec: Vec<&'src str>, // Optional: zum Zurückübersetzen
}

impl<'src> Internalizer<'src> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    pub fn get(&mut self, name: &'src str) -> Symbol<'src> {
        if let Some(&sym) = self.map.get(name) {
            return sym;
        }
        let id = Symbol {
            _marker: PhantomData,
            idx: self.vec.len(),
        };
        self.vec.push(name);
        self.map.insert(name, id);
        id
    }

    pub fn resolve(&self, sym: Symbol) -> &str {
        self.vec[sym.idx]
    }
}
