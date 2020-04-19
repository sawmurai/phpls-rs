use crate::environment::scope::Scope;
use crate::token::Token;
use std::collections::HashMap;

pub mod index;
pub mod scope;

#[derive(Debug, Default)]
pub struct Environment {
    /// List of scopes available
    scopes: HashMap<String, Scope>,

    /// Path to the current stack
    /// Example: ["/root/of/my/project/stuff.php", "MyClass" "my_function"]
    /// This will be the same as the document uri used by the language server protocol
    current_stack: Vec<String>,

    /// Cached version of the current path used as key in the scope hashmap
    current_key: String,
}

/// Recursivly walk the ast and create the environment. The environment will be used to provide the
/// language server capabilities
pub fn new(file: String) -> Environment {
    //let symbol_index = build_symbol_index(ast);

    Environment {
        current_key: file.clone(),
        current_stack: vec![file],
        scopes: HashMap::new(),
    }
}

impl Environment {
    /// Enter the next scope by referencing it to its parent and pushing it onto the stack
    pub fn enter_scope(&mut self, name: &str) {
        self.current_stack.push(name.to_owned());

        self.current_key = get_key(&self.current_stack);

        //println!("{}", self.current_stack.join("/"));

        self.scopes
            .insert(self.current_key.clone(), Scope::default());
    }

    /// Finish and leave the current scope.
    pub fn finish_scope(&mut self) {
        self.current_stack.pop();
        self.current_key = self.current_stack.iter().cloned().collect::<String>();
    }

    /// Register a new symbol usage in the scope that defines it or in the current scope, if no scope previously
    /// defined it
    pub fn usage(&mut self, token: Token) {
        let path = if let Some(path) = self.defining_scope_path(&token) {
            get_key(&path)
        } else {
            get_key(&self.current_stack)
        };

        self.scopes.get_mut(&path).unwrap().push_symbol(token);
    }

    /// Resolve the path of the scope defining a symbol (or its first usage in case of variables)
    pub fn defining_scope_path(&mut self, token: &Token) -> Option<Vec<String>> {
        let name = token.label.as_ref().unwrap();
        let mut path = self.current_stack.clone();

        loop {
            if path.len() == 1 {
                return None;
            }

            let parent_scope = self.scopes.get(&get_parent_key(&path)).unwrap();
            let scope = self.scopes.get(&get_key(&path)).unwrap();

            if scope.has_symbol(&name) && !parent_scope.has_symbol(&name) {
                return Some(path);
            }

            path.pop();
        }
    }

    // Register a new symbol definition
    pub fn definition(&mut self, token: &Token) {
        self.scopes
            .get_mut(&self.current_key)
            .unwrap()
            .push_definition(token.clone());
    }

    /// Return the current scope
    pub fn current_scope(&mut self) -> Option<&Scope> {
        self.scopes.get(&self.current_key)
    }
}

fn get_key(path: &Vec<String>) -> String {
    path.iter().cloned().collect::<String>()
}

fn get_parent_key(path: &Vec<String>) -> String {
    path.iter().rev().skip(1).rev().cloned().collect::<String>()
}
