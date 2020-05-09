use crate::environment::import::{collect_uses, SymbolImport};
use crate::environment::symbol::{document_symbol, Symbol};
use crate::node::{get_range, Node};
use crate::token::Token;
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use tower_lsp::lsp_types::{Diagnostic, Range, SymbolKind, Url};
#[derive(Debug)]
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

#[derive(Clone, Debug)]
pub struct Reference {
    /// Used for variables
    pub token: Option<Token>,

    /// The type_ref if applicable
    pub type_ref: Option<Vec<Token>>,

    /// The definition of the symbol that is used
    pub defining_symbol: Option<Symbol>,

    /// Selection range of the usage
    pub range: Range,
}

impl Reference {
    /// Reference to a variable
    pub fn variable(token: &Token) -> Self {
        Self {
            token: Some(token.clone()),
            defining_symbol: None,
            type_ref: None,
            range: get_range(token.range()),
        }
    }

    /// Reference to an identifier, for example a function or a member
    pub fn identifier(token: &Token) -> Self {
        Self {
            token: Some(token.clone()),
            defining_symbol: None,
            type_ref: None,
            range: get_range(token.range()),
        }
    }

    /// Reference to a type
    pub fn type_ref(type_ref: Vec<Token>) -> Self {
        let range = get_range((
            type_ref.first().unwrap().range().0,
            type_ref.last().unwrap().range().1,
        ));

        Self {
            token: None,
            defining_symbol: None,
            type_ref: Some(type_ref),
            range,
        }
    }
}

impl From<Reference> for Diagnostic {
    fn from(reference: Reference) -> Diagnostic {
        Diagnostic {
            range: reference.range,
            message: format!("{:#?}", reference),
            ..Diagnostic::default()
        }
    }
}

#[derive(Default, Debug)]
pub struct Scope {
    /// Type of this scope
    pub scope_type: ScopeType,

    /// Symbols defined in this scope
    pub symbols: HashMap<String, Symbol>,

    /// Symbols imported into this scope via use-statements
    pub aliases: Vec<SymbolImport>,

    /// All unresolved references
    pub references: Vec<Reference>,
}

impl Scope {
    pub fn new(scope_type: ScopeType) -> Self {
        Self {
            scope_type,
            ..Default::default()
        }
    }

    pub fn definition(&mut self, symbol: Symbol) {
        let name = symbol.name.clone();

        if self.symbols.get(&name).is_none() {
            self.symbols.insert(name, symbol);
        } else if symbol.kind != SymbolKind::Variable {
            self.symbols.insert(
                format!(
                    "{}-{}:{}",
                    name, symbol.range.start.line, symbol.range.start.character
                ),
                symbol,
            );
        }
    }

    pub fn get_definitions(&self) -> Vec<Symbol> {
        self.symbols
            .values()
            .map(|r| r.clone())
            .collect::<Vec<Symbol>>()
    }

    pub fn get_unresolvable(&self) -> Vec<Reference> {
        self.references
            .iter()
            .filter(|r| r.defining_symbol.is_none())
            .map(|s| s.clone())
            .collect::<Vec<Reference>>()
    }

    pub fn prepare_reference(&mut self, reference: Reference) {
        self.references.push(reference);
    }

    /// Resolve all dangling references using the provided symbol table.
    /// The symbol table gets enriched with the symbols registered in each new scope
    /// Due to all the cloning of the HashMaps this might be slow  
    /// TODO: Treat scope boundaries correctly, so that functions hide some stuff of the parent scope
    pub fn resolve_references(
        &mut self,
        _uri: &Url,
        _arena: &mut Arena<Scope>,
        _scope: NodeId,
    ) -> Result<(), String> {
        /*
        let mut local_table = symbol_table.clone();

        for (name, symbol) in self.symbols.iter() {
            local_table.insert(name.clone(), symbol.clone());
        }

        for import in self.aliases.iter() {
            let alias = if let Some(alias) = import.alias.as_ref() {
                alias
            } else {
                import.path.last().unwrap()
            };

            local_table.insert(alias.label.clone().unwrap(), Symbol::from(alias));
        }

        for reference in self.references.iter_mut() {
            let ref_name = if let Some(token) = reference.token.as_ref() {
                if let Some(label) = token.label.as_ref() {
                    label
                } else {
                    continue;
                }
            // Add support for type refs etc
            } else if let Some(type_ref) = reference.type_ref.as_ref() {
                let last = type_ref.last().unwrap();

                if let Some(label) = last.label.as_ref() {
                    label
                } else {
                    continue;
                }
            } else {
                continue;
            };

            if let Some(symbol) = local_table.get_mut(ref_name) {
                // This works but is kind of useless, as the local_table gets thrown away anyway
                // TODO: Find a way to keep this. Maybe aggregate and return an entirely new scope?
                // Maybe a reduced scope that only contains symbols and references?
                // Maybe it could even just be a stupid list because there won't be so many definitions?
                // Or we just say fuck it and do not cache it at all .. safe some memory
                symbol.reference(Location {
                    uri: uri.clone(),
                    range: reference.range,
                });

                reference.defining_symbol = Some(symbol.clone());
            }
        }

        for child in self.children.values() {
            child
                .lock()
                .unwrap()
                .resolve_references(uri, local_table.clone())?;
        }*/

        Ok(())
    }
}

pub fn collect_symbols(arena: &mut Arena<Scope>, scope: NodeId, node: &Node) -> Result<(), String> {
    match node {
        Node::NamespaceStatement { .. }
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
            let def = document_symbol(arena, scope, node)?;

            arena[scope].get_mut().definition(def);
        }

        Node::LexicalVariable { variable, .. }
        | Node::Variable(variable)
        | Node::StaticVariable { variable, .. } => {
            let def = document_symbol(arena, scope, node)?;

            arena[scope].get_mut().definition(def);
            arena[scope]
                .get_mut()
                .prepare_reference(Reference::variable(variable));
        }
        Node::Identifier(token) => arena[scope].get_mut().definition(Symbol::from(token)),
        Node::Literal(token) => {
            if token.is_identifier() {
                arena[scope]
                    .get_mut()
                    .prepare_reference(Reference::identifier(token));
            }
        }
        Node::TypeRef(parts) => {
            arena[scope]
                .get_mut()
                .prepare_reference(Reference::type_ref(
                    parts
                        .iter()
                        .filter(|t| t.label.is_some())
                        .map(|t| t.clone())
                        .collect(),
                ));
        }
        Node::GroupedUse { .. }
        | Node::UseDeclaration { .. }
        | Node::UseFunction { .. }
        | Node::UseConst { .. } => {
            arena[scope]
                .get_mut()
                .aliases
                .extend(collect_uses(node, &Vec::new()));
        }
        _ => {
            for child in node.children() {
                collect_symbols(arena, scope, child)?;
            }
        }
    };

    Ok(())
}
