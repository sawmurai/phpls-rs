use crate::environment::scope::{collect_symbols, Scope, ScopeType};
use crate::node::{get_range, Node};
use async_recursion::async_recursion;
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::lsp_types::{DocumentSymbol, Range, SymbolKind};

/// Contains information about a symbol in a scope. This can be a function, a class, a variable etc.
/// It is bacially an extended `lsp_types::DocumentSymbol` that also contains a data type (for vars and properties)
#[derive(Clone)]
pub struct Symbol {
    /// The data type of this symbol. Of course not all symbols have data types (Namespaces for example do not), so
    /// this remains an `Option`
    pub data_type: Option<Node>,
    pub kind: SymbolKind,
    pub name: String,
    pub range: Range,
    pub selection_range: Range,
    pub detail: Option<String>,
    pub deprecated: Option<bool>,
    pub children: Option<Vec<Symbol>>,
}

/// Basically a 1:1 mapping that omits the data type
impl From<&Symbol> for DocumentSymbol {
    fn from(symbol: &Symbol) -> DocumentSymbol {
        let children = if let Some(children) = symbol.children.as_ref() {
            Some(children.iter().map(|s| DocumentSymbol::from(s)).collect())
        } else {
            None
        };

        DocumentSymbol {
            kind: symbol.kind,
            name: symbol.name.clone(),
            range: symbol.range,
            selection_range: symbol.selection_range,
            detail: symbol.detail.clone(),
            deprecated: symbol.deprecated,
            children,
        }
    }
}

#[async_recursion]
pub async fn document_symbol(node: &Node, scope: Arc<Mutex<Scope>>) -> Result<Symbol, String> {
    match node {
        Node::Const { name, .. } => Ok(Symbol {
            data_type: None,
            name: name.clone().label.unwrap(),
            kind: SymbolKind::Constant,
            range: get_range(node.range()),
            selection_range: get_range(node.range()),
            detail: None,
            children: None,
            deprecated: None,
        }),
        Node::LexicalVariable { variable, .. } => Ok(Symbol::from(variable)),
        Node::StaticVariable { variable, .. } => Ok(Symbol::from(variable)),
        Node::Function {
            token,
            body,
            arguments,
            return_type,
            ..
        } => {
            let range = get_range(node.range());
            let name = String::from("Anonymous function");
            let scope = Scope::within(&name, scope, ScopeType::Function).await;

            collect_symbols(body, scope.clone()).await?;

            if let Some(arguments) = arguments {
                for n in arguments {
                    collect_symbols(n, scope.clone()).await?;
                }
            }

            let children = scope.lock().await.get_definitions();

            let data_type = if let Some(data_type) = return_type {
                Some(*data_type.clone())
            } else {
                None
            };

            Ok(Symbol {
                data_type,
                name,
                kind: SymbolKind::Function,
                range,
                selection_range: get_range(token.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::FunctionArgument { name, .. } => Ok(Symbol::from(name)),
        Node::Class {
            token,
            body,
            arguments,
            ..
        } => {
            let name = String::from("Anonymous class");
            let range = get_range(node.range());
            let scope = Scope::within(&name, scope, ScopeType::Class).await;
            collect_symbols(body, scope.clone()).await?;

            if let Some(arguments) = arguments {
                for n in arguments {
                    collect_symbols(n, scope.clone()).await?;
                }
            }

            let children = scope.lock().await.get_definitions();

            Ok(Symbol {
                data_type: None,
                name,
                kind: SymbolKind::Class,
                range,
                selection_range: get_range(token.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::NamespaceBlock {
            token,
            type_ref,
            block,
            ..
        } => {
            let range = get_range(node.range());
            let name = if let Some(name) = type_ref {
                match name.as_ref() {
                    Node::TypeRef(tokens) => tokens
                        .iter()
                        .map(|n| n.clone().label.unwrap_or_else(|| "n to string".to_owned()))
                        .collect::<Vec<String>>()
                        .join(""),
                    _ => panic!("This should not happen"),
                }
            } else {
                "Anonymous namespace".to_string()
            };

            let scope = Scope::within(&name, scope, ScopeType::Namespace).await;
            collect_symbols(block, scope.clone()).await?;

            let children = scope.lock().await.get_definitions();
            Ok(Symbol {
                data_type: None,
                name,
                kind: SymbolKind::Class,
                range,
                selection_range: get_range(token.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::ClassStatement { name, body, .. } => {
            let selection_range = get_range(name.range());
            let name = name.clone().label.unwrap();
            let range = get_range(node.range());
            let scope = Scope::within(&name, scope, ScopeType::Class).await;
            collect_symbols(body, scope.clone()).await?;

            let children = scope.lock().await.get_definitions();

            Ok(Symbol {
                data_type: None,
                name,
                kind: SymbolKind::Class,
                range,
                selection_range,
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::TraitStatement { name, body, .. } => {
            let selection_range = get_range(name.range());
            let name = name.clone().label.unwrap();
            let range = get_range(node.range());
            let scope = Scope::within(&name, scope, ScopeType::Class).await;
            collect_symbols(body, scope.clone()).await?;

            let children = scope.lock().await.get_definitions();
            Ok(Symbol {
                data_type: None,
                name,
                kind: SymbolKind::Class,
                range,
                selection_range,
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::Interface { name, body, .. } => {
            let selection_range = get_range(name.range());
            let name = name.clone().label.unwrap();
            let range = get_range(node.range());
            let scope = Scope::within(&name, scope, ScopeType::Class).await;
            collect_symbols(body, scope.clone()).await?;

            let children = scope.lock().await.get_definitions();

            Ok(Symbol {
                data_type: None,
                name,
                kind: SymbolKind::Interface,
                range,
                selection_range,
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::ClassConstantDefinitionStatement { name, .. } => {
            let range = get_range(node.range());
            Ok(Symbol {
                data_type: None,
                name: name.clone().label.unwrap(),
                kind: SymbolKind::Constant,
                range,
                selection_range: range,
                detail: None,
                children: None,
                deprecated: None,
            })
        }
        Node::PropertyDefinitionStatement {
            name, data_type, ..
        } => {
            let range = get_range(node.range());

            let data_type = if let Some(data_type) = data_type {
                Some(*data_type.clone())
            } else {
                None
            };

            Ok(Symbol {
                data_type,
                name: name.clone().label.unwrap(),
                kind: SymbolKind::Property,
                range,
                selection_range: range,
                detail: None,
                children: None,
                deprecated: None,
            })
        }
        Node::MethodDefinitionStatement { name, function, .. } => {
            let range = get_range(node.range());
            // From the start of the declaration to the end of the method
            let function = document_symbol(function.as_ref(), scope).await;

            Ok(Symbol {
                name: name.clone().label.unwrap(),
                range,
                selection_range: get_range(name.range()),
                kind: SymbolKind::Method,
                ..function?
            })
        }
        Node::FunctionDefinitionStatement {
            return_type,
            arguments,
            body,
            ..
        } => {
            let range = get_range(node.range());
            let name = "Anonymous function".to_owned();
            let scope = Scope::within(&name, scope, ScopeType::Function).await;

            if let Some(arguments) = arguments {
                for n in arguments {
                    collect_symbols(n, scope.clone()).await?;
                }
            }

            if let Some(body) = body {
                collect_symbols(body, scope.clone()).await?;
            }

            let children = scope.lock().await.get_definitions();

            let data_type = if let Some(data_type) = return_type {
                Some(*data_type.clone())
            } else {
                None
            };

            Ok(Symbol {
                data_type,
                name,
                kind: SymbolKind::Function,
                range,
                selection_range: range,
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::NamedFunctionDefinitionStatement { name, function, .. } => {
            let range = get_range(node.range());

            // From the start of the declaration to the end of the method
            let function = document_symbol(function.as_ref(), scope).await;

            Ok(Symbol {
                name: name.clone().label.unwrap(),
                range,
                selection_range: get_range(name.range()),
                ..function?
            })
        }
        Node::Variable(token) => Ok(Symbol::from(token)),
        Node::NamespaceStatement { type_ref, .. } => {
            let range = get_range(node.range());
            let name = match &**type_ref {
                Node::TypeRef(tokens) => tokens
                    .iter()
                    .map(|n| n.clone().label.unwrap_or_else(|| "\\".to_owned()))
                    .collect::<Vec<String>>()
                    .join(""),
                _ => panic!("This should not happen"),
            };

            Ok(Symbol {
                data_type: None,
                name,
                kind: SymbolKind::Namespace,
                range,
                selection_range: range,
                detail: None,
                children: None,
                deprecated: None,
            })
        }
        _ => unimplemented!("Unexpected {:?}", node),
    }
}
