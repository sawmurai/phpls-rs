use crate::environment::import::SymbolImport;
use crate::environment::symbol::{document_symbol, Symbol};
use crate::node::Node;
use async_recursion::async_recursion;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

pub enum ScopeType {
    Function,
    Class,
    Method,
    Global,
    Namespace,
    File,
}

impl Default for ScopeType {
    fn default() -> ScopeType {
        ScopeType::Global
    }
}

#[derive(Default)]
pub struct Scope {
    /// Type of this scope
    pub scope_type: ScopeType,

    /// Symbols defined in this scope
    pub symbols: HashMap<String, Symbol>,

    /// Symbols imported into this scope via use-statements
    pub imports: Vec<SymbolImport>,

    /// Access to this scope's parent
    pub parent: Option<Arc<Mutex<Scope>>>,

    /// All children. The key is the function-, class- or namespace-name
    pub children: HashMap<String, Arc<Mutex<Scope>>>,
}

impl Scope {
    pub async fn within(
        name: &str,
        parent: Arc<Mutex<Self>>,
        scope_type: ScopeType,
    ) -> Arc<Mutex<Self>> {
        let new = Self {
            parent: Some(parent.clone()),
            scope_type,
            ..Default::default()
        };

        let new = Arc::new(Mutex::new(new));

        parent
            .lock()
            .await
            .children
            .insert(name.to_owned(), Arc::clone(&new));

        new
    }

    pub fn definition(&mut self, symbol: Symbol) {
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    pub fn get_definitions(&self) -> Vec<Symbol> {
        self.symbols
            .values()
            .map(|s| s.clone())
            .collect::<Vec<Symbol>>()
    }

    #[async_recursion]
    pub async fn all_definitions(&self) -> Vec<Symbol> {
        let mut symbols: Vec<Symbol> = self.get_definitions();

        for scope in self.children.values() {
            let child = scope.lock().await;

            symbols.extend(child.all_definitions().await);
        }

        symbols
    }
}

#[async_recursion]
pub async fn collect_symbols(node: &Node, scope: Arc<Mutex<Scope>>) -> Result<(), String> {
    match node {
        Node::NamespaceStatement { .. }
        | Node::LexicalVariable { .. }
        | Node::Variable { .. }
        | Node::StaticVariable { .. }
        | Node::Function { .. }
        | Node::FunctionArgument { .. }
        | Node::Class { .. }
        | Node::NamespaceBlock { .. }
        | Node::ClassStatement { .. }
        | Node::TraitStatement { .. }
        | Node::ClassConstantDefinitionStatement { .. }
        | Node::PropertyDefinitionStatement { .. }
        | Node::MethodDefinitionStatement { .. }
        | Node::FunctionDefinitionStatement { .. }
        | Node::NamedFunctionDefinitionStatement { .. }
        | Node::Const { .. }
        | Node::Interface { .. } => {
            let def = document_symbol(node, scope.clone()).await?;

            scope.lock().await.definition(def);
        }
        Node::Identifier(token) => scope.lock().await.definition(Symbol::from(token)),
        Node::Literal(_token) => {
            //if token.is_identifier() {
            //    scope.lock().unwrap().usage(DocumentSymbol::from(token))?;
            //}
        }
        _ => {
            for child in node.children() {
                collect_symbols(child, scope.clone()).await?;
            }
        }
    };

    Ok(())
}
