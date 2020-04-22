use crate::environment::namespace::Namespace;
use crate::token::Token;
use tower_lsp::lsp_types::{DocumentSymbol, Range};

pub mod class;
pub mod index;
pub mod namespace;

#[derive(Debug, Default)]
pub struct Environment {
    /// Path to the current stack
    /// Example: ["/root/of/my/project/stuff.php", "MyClass" "my_function"]
    /// This will be the same as the document uri used by the language server protocol
    current_key: Vec<String>,

    global_namespace: Namespace,

    current_namespace: Vec<String>,
}

impl Environment {
    /// Recursivly walk the ast and create the environment. The environment will be used to provide the
    /// language server capabilities
    pub fn new(file: &str) -> Environment {
        let current_key = vec![file.to_owned()];

        Environment {
            current_key: current_key,
            global_namespace: Namespace::default(),
            current_namespace: Vec::new(),
        }
    }

    /// Recursivly creates the namespace in a nested HashMap
    pub fn start_namespace(&mut self, path: Vec<Token>, range: Range) {
        let name = path
            .iter()
            .map(|t| {
                if let Some(label) = t.clone().label {
                    return label;
                } else {
                    return "\\".to_owned();
                }
            })
            .collect::<Vec<String>>()
            .join("");

        self.current_namespace.push(name);

        let mut path = self.current_namespace.clone();
        path.reverse();

        self.global_namespace.register_namespace(path, range);
    }

    pub fn finish_namespace(&mut self) {
        self.current_namespace.pop();
    }

    /// Start a new class by adding its name to the current namespace path and
    /// registering it into the current namespace
    pub fn start_class(&mut self, name: &str, range: Range) {
        self.current_namespace.push(name.to_owned());

        let mut path = self.current_namespace.clone();
        path.reverse();

        self.global_namespace.register_class(path, range);
    }

    /// Pop the class name right off of the stack
    pub fn finish_class(&mut self) {
        self.current_namespace.pop();
    }

    /// Register a new method in the active class
    pub fn register_method(&mut self, name: &str, range: Range) {
        let mut path = self.current_namespace.clone();
        path.push(name.to_owned());
        path.reverse();

        self.global_namespace.register_method(path, range);
    }

    /// Register a new property in the active class
    pub fn register_property(&mut self, name: &str, range: Range) {
        let mut path = self.current_namespace.clone();
        path.push(name.to_owned());
        path.reverse();

        self.global_namespace.register_method(path, range);
    }

    /// Register a new property in the active class
    pub fn register_constant(&mut self, name: &str, range: Range) {
        let mut path = self.current_namespace.clone();
        path.push(name.to_owned());
        path.reverse();

        self.global_namespace.register_constant(path, range);
    }

    pub fn register_variable(&mut self, name: &str, range: Range) {
        let mut path = self.current_namespace.clone();
        path.push(name.to_owned());
        path.reverse();

        //self.global_namespace.register_variable(path, range);
    }

    /// Returns a vector of all document symbols in the current document
    pub fn document_symbols(&self) -> Vec<DocumentSymbol> {
        self.global_namespace
            .namespaces
            .iter()
            .map(|(_, ns)| DocumentSymbol::from(ns))
            .collect()
    }
}
