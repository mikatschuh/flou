use std::{ collections::HashMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

pub struct NameSpace {
    map: HashMap<String, Symbol>,
    vec: Vec<String>, // Optional: zum Zurückübersetzen
}

impl NameSpace {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&sym) = self.map.get(name) {
            return sym;
        }
        let id = Symbol(self.vec.len());
        self.vec.push(name.to_owned());
        self.map.insert(name.to_owned(), id);
        id
    }

    pub fn get(&self, name: &String) -> Option<Symbol> {
        self.map.get(name).copied()
    }
    pub fn contains(&self, name: &String) -> bool {
        self.map.contains_key(name)
    }

    pub fn resolve(&self, sym: Symbol) -> &str {
        &self.vec[sym.0]
    }
}
