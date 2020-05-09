use crate::environment::scope::{collect_symbols, Reference, Scope, ScopeType};
use crate::node::{get_range, Node};
use crate::token::Token;
use indextree::{Arena, NodeId};
use tower_lsp::lsp_types::{DocumentSymbol, Range, SymbolKind};

/// Contains information about a symbol in a scope. This can be a function, a class, a variable etc.
/// It is bacially an extended `lsp_types::DocumentSymbol` that also contains a data type (for vars and properties)
#[derive(Clone, Debug)]
pub struct Symbol {
    /// The data type of this symbol. Of course not all symbols have data types (Namespaces for example do not), so
    /// this remains an `Option`
    pub data_type: Option<Box<Reference>>,
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

fn get_type_ref(node: &Node) -> Option<Vec<Token>> {
    match node {
        Node::ArgumentType { type_ref, .. } => match &**type_ref {
            Node::TypeRef(items) => Some(items.clone()),
            _ => None,
        },
        _ => None,
    }
}

pub fn document_symbol(
    arena: &mut Arena<Scope>,
    scope: &NodeId,
    node: &Node,
) -> Result<Symbol, String> {
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
            token, return_type, ..
        } => {
            let child = arena.new_node(Scope::new(ScopeType::Function, node.range()));
            scope.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            let children = Some(arena[child].get().get_definitions());

            let data_type = if let Some(data_type) = return_type {
                if let Some(type_ref) = get_type_ref(data_type) {
                    Some(Box::new(Reference::type_ref(type_ref)))
                } else {
                    None
                }
            } else {
                None
            };

            Ok(Symbol {
                data_type,
                name: String::from("Anonymous function"),
                kind: SymbolKind::Function,
                range: get_range(node.range()),
                selection_range: get_range(token.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::FunctionArgument {
            name,
            argument_type,
            ..
        } => {
            if let Some(argument_type) = argument_type {
                if let Some(type_ref) = get_type_ref(argument_type) {
                    Ok(Symbol {
                        data_type: Some(Box::new(Reference::type_ref(type_ref))),
                        ..Symbol::from(name)
                    })
                } else {
                    Ok(Symbol::from(name))
                }
            } else {
                Ok(Symbol::from(name))
            }
        }
        Node::Class { token, .. } => {
            let name = String::from("Anonymous class");
            let range = get_range(node.range());
            let child = arena.new_node(Scope::new(ScopeType::Class, node.range()));
            scope.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            let children = Some(arena[child].get().get_definitions());

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
            token, type_ref, ..
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

            let child = arena.new_node(Scope::new(ScopeType::Namespace, node.range()));
            scope.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            let children = Some(arena[child].get().get_definitions());
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
        Node::ClassStatement { name, .. } => {
            let selection_range = get_range(name.range());
            let name = name.clone().label.unwrap();
            let range = get_range(node.range());

            let child = arena.new_node(Scope::new(ScopeType::Class, node.range()));
            scope.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            let children = Some(arena[child].get().get_definitions());

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
        Node::TraitStatement { name, .. } => {
            let selection_range = get_range(name.range());
            let name = name.clone().label.unwrap();
            let range = get_range(node.range());
            let child = arena.new_node(Scope::new(ScopeType::Class, node.range()));
            scope.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            let children = Some(arena[child].get().get_definitions());
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
        Node::Interface { name, .. } => {
            let selection_range = get_range(name.range());
            let name = name.clone().label.unwrap();
            let range = get_range(node.range());
            let child = arena.new_node(Scope::new(ScopeType::Class, node.range()));
            scope.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            let children = Some(arena[child].get().get_definitions());

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
                if let Some(type_ref) = get_type_ref(data_type) {
                    Some(Box::new(Reference::type_ref(type_ref)))
                } else {
                    None
                }
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
            let function = document_symbol(arena, scope, function.as_ref());

            Ok(Symbol {
                name: name.clone().label.unwrap(),
                range,
                selection_range: get_range(name.range()),
                kind: SymbolKind::Method,
                ..function?
            })
        }
        Node::FunctionDefinitionStatement { return_type, .. } => {
            let range = get_range(node.range());
            let name = "Anonymous function".to_owned();
            let child = arena.new_node(Scope::new(ScopeType::Function, node.range()));
            scope.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            let children = Some(arena[child].get().get_definitions());

            let data_type = if let Some(data_type) = return_type {
                if let Some(type_ref) = get_type_ref(data_type) {
                    Some(Box::new(Reference::type_ref(type_ref)))
                } else {
                    None
                }
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
            let function = document_symbol(arena, scope, function.as_ref());

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
