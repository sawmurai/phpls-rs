use crate::environment::scope::Scope;
use crate::token::Token;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub mod index;
pub mod scope;

#[derive(Debug, Default)]
pub struct Environment {
    /// List of scopes available
    scopes: HashMap<Vec<String>, Arc<Mutex<Scope>>>,

    /// Path to the current stack
    /// Example: ["/root/of/my/project/stuff.php", "MyClass" "my_function"]
    /// This will be the same as the document uri used by the language server protocol
    current_key: Vec<String>,

    current_scope: Arc<Mutex<Scope>>,
}

/// Recursivly walk the ast and create the environment. The environment will be used to provide the
/// language server capabilities
pub fn new(file: String) -> Environment {
    //let symbol_index = build_symbol_index(ast);

    Environment {
        current_key: vec![file],
        scopes: HashMap::new(),
        current_scope: Arc::new(Mutex::new(Scope::default())),
    }
}

impl Environment {
    /// Enter the next scope by referencing it to its parent and pushing it onto the stack
    pub fn enter_scope(&mut self, name: &str) {
        self.current_key.push(name.to_owned());

        //println!("{}", self.current_stack.join("/"));

        let scope = Arc::new(Mutex::new(Scope::new(self.current_scope.clone())));
        self.scopes.insert(self.current_key.clone(), scope.clone());

        self.current_scope = scope;
    }

    /// Finish and leave the current scope.
    pub fn finish_scope(&mut self) {
        self.current_key.pop();
    }

    /// Register a new symbol usage in the scope that defines it or in the current scope, if no scope previously
    /// defined it
    pub fn usage(&mut self, token: Token) {
        self.current_scope.lock().unwrap().push_symbol(token);
    }

    // Register a new symbol definition
    pub fn definition(&mut self, token: &Token) {
        self.current_scope
            .lock()
            .unwrap()
            .push_definition(token.clone());
    }
}
