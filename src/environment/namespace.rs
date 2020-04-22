use crate::environment::class::{Class, Method, Property};
use std::collections::HashMap;

use tower_lsp::lsp_types::{DocumentSymbol, Range, SymbolKind};

#[derive(Debug, Default)]
pub struct Namespace {
    /// Definition of the symbol, or first usage in case of a variable
    pub name: String,

    /// Namespaces below this namespace
    pub namespaces: HashMap<String, Namespace>,

    /// All usages of a symbol
    pub classes: HashMap<String, Class>,

    pub range: Range,
}

impl Namespace {
    pub fn new(name: &str, range: Range) -> Self {
        Namespace {
            name: name.to_owned(),
            range,
            ..Namespace::default()
        }
    }

    /// Recursivly register the namespace. We assume that the namespace is already in the reverse order
    pub fn register_namespace(&mut self, mut path: Vec<String>, range: Range) {
        if path.is_empty() {
            return;
        }

        let next = path.pop().unwrap();
        let new_ns = Namespace::new(&next, range);

        self.namespaces
            .entry(next)
            .or_insert_with(|| new_ns)
            .register_namespace(path, range);
    }

    /// Recursivly register a class in the namespace
    pub fn register_class(&mut self, mut path: Vec<String>, range: Range) {
        let part = path.pop().unwrap();

        if path.is_empty() {
            let new_class = Class::new(&part, range);
            self.classes.insert(part, new_class);

            return;
        }

        self.namespaces
            .get_mut(&part)
            .unwrap()
            .register_class(path, range);
    }

    /// Recursivly register a method in a class
    pub fn register_method(&mut self, mut path: Vec<String>, range: Range) {
        let part = path.pop().unwrap();

        // If only one is left then part is the class and the last one is the method
        if path.len() == 1 {
            self.classes
                .get_mut(&part)
                .unwrap()
                .methods
                .push(Method::new(&path.pop().unwrap(), range));

            return;
        }

        self.namespaces
            .get_mut(&part)
            .unwrap()
            .register_method(path, range);
    }

    /// Recursivly register a method in a class
    pub fn register_constant(&mut self, mut path: Vec<String>, range: Range) {
        let part = path.pop().unwrap();

        // If only one is left then part is the class and the last one is the method
        if path.len() == 1 {
            self.classes
                .get_mut(&part)
                .unwrap()
                .constants
                .push(path.pop().unwrap());

            return;
        }

        self.namespaces
            .get_mut(&part)
            .unwrap()
            .register_constant(path, range);
    }

    /// Recursivly register a method in a class
    pub fn register_property(&mut self, mut path: Vec<String>, range: Range) {
        let part = path.pop().unwrap();

        // If only one is left then part is the class and the last one is the property
        if path.len() == 1 {
            self.classes
                .get_mut(&part)
                .unwrap()
                .properties
                .push(Property::new(&path.pop().unwrap(), range));

            return;
        }

        self.namespaces
            .get_mut(&part)
            .unwrap()
            .register_property(path, range);
    }
}

impl From<&Namespace> for DocumentSymbol {
    fn from(ns: &Namespace) -> DocumentSymbol {
        DocumentSymbol {
            name: ns.name.clone(),
            detail: None,
            kind: SymbolKind::Namespace,
            deprecated: None,
            range: ns.range,
            selection_range: ns.range,
            children: Some(
                ns.namespaces
                    .iter()
                    .map(|(_, ns)| DocumentSymbol::from(ns))
                    .chain(
                        ns.classes
                            .iter()
                            .map(|(_, class)| DocumentSymbol::from(class)),
                    )
                    .collect(),
            ),
        }
    }
}
