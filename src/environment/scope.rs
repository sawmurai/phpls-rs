use crate::environment::import::{collect_uses, SymbolImport};
use crate::environment::in_range;
use crate::environment::symbol::{document_symbol, Symbol};
use crate::node::{get_range, Node, NodeRange};
use crate::token::Token;
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use tower_lsp::lsp_types::{Diagnostic, Location, Position, Range, SymbolKind, Url};
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

impl From<&Reference> for Diagnostic {
    fn from(reference: &Reference) -> Diagnostic {
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

    /// A range to help find the correct scope from a cursor position
    pub range: Range,
}

impl Scope {
    pub fn new(scope_type: ScopeType, range: NodeRange) -> Self {
        Self {
            scope_type,
            range: get_range(range),
            ..Default::default()
        }
    }

    pub fn symbol_at_position(&self, position: &Position) -> Option<&Symbol> {
        for (_, symbol) in self.symbols.iter() {
            if in_range(position, &symbol.range) {
                return Some(symbol);
            }
        }

        None
    }

    pub fn reference_at_position(&self, position: &Position) -> Option<&Reference> {
        for reference in self.references.iter() {
            if in_range(position, &reference.range) {
                return Some(reference);
            }
        }

        None
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

    pub fn get_unresolvable(
        &self,
        uri: &Url,
        arena: &Arena<Scope>,
        scope: NodeId,
        global_symbols: &HashMap<String, Location>,
    ) -> Vec<&Reference> {
        let mut unresolvable = Vec::new();

        for reference in self.references.iter() {
            if self
                .resolve_reference(uri, reference, arena, scope, global_symbols)
                .is_none()
            {
                unresolvable.push(reference);
            }
        }

        unresolvable
    }

    pub fn prepare_reference(&mut self, reference: Reference) {
        self.references.push(reference);
    }

    /// Resolve all dangling references using the provided symbol table.
    /// The symbol table gets enriched with the symbols registered in each new scope
    /// Due to all the cloning of the HashMaps this might be slow  
    /// TODO: Treat scope boundaries correctly, so that functions hide some stuff of the parent scope
    pub fn resolve_reference<'a>(
        &'a self,
        uri: &Url,
        reference: &Reference,
        arena: &'a Arena<Scope>,
        scope: NodeId,
        global_symbols: &HashMap<String, Location>,
    ) -> Option<Location> {
        let ref_name = if let Some(token) = reference.token.as_ref() {
            if let Some(label) = token.label.as_ref() {
                label
            } else {
                return None;
            }
        } else if let Some(type_ref) = reference.type_ref.as_ref() {
            let full_name: String = type_ref
                .iter()
                .map(|token| {
                    if let Some(label) = token.label.as_ref() {
                        label
                    } else {
                        "\\"
                    }
                })
                .collect();

            if let Some(location) = global_symbols.get(&full_name) {
                return Some(location.clone());
            }

            let last = type_ref.last().unwrap();

            if let Some(label) = last.label.as_ref() {
                label
            } else {
                return None;
            }
        } else {
            return None;
        };

        for import in self.aliases.iter() {
            let alias = if let Some(alias) = import.alias.as_ref() {
                alias
            } else {
                import.path.last().unwrap()
            };

            if let Some(type_ref) = reference.type_ref.as_ref() {
                let first = type_ref.first().unwrap();

                // If the first token of the type ref is equal to the name of the import,
                // we continue resolving the alias using the expanded name
                if first.label.is_some() && first.label == alias.label {
                    if let Some(parent) = arena[scope].parent() {
                        return arena[parent].get().resolve_reference(
                            uri,
                            &Reference::type_ref(import.path.clone()),
                            arena,
                            parent,
                            global_symbols,
                        );
                    } else {
                        return None;
                    }
                }
            }
        }

        if let Some(symbol) = self.symbols.get(ref_name) {
            return Some(Location {
                uri: uri.clone(),
                range: symbol.range,
            });
        } else if let Some(parent) = arena[scope].parent() {
            return arena[parent].get().resolve_reference(
                uri,
                reference,
                arena,
                parent,
                global_symbols,
            );
        }

        None
    }
}

pub fn collect_symbols(
    arena: &mut Arena<Scope>,
    scope: &NodeId,
    node: &Node,
) -> Result<(), String> {
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

            arena[*scope].get_mut().definition(def);
        }

        Node::LexicalVariable { variable, .. }
        | Node::Variable(variable)
        | Node::StaticVariable { variable, .. } => {
            let def = document_symbol(arena, scope, node)?;

            arena[*scope].get_mut().definition(def);
            arena[*scope]
                .get_mut()
                .prepare_reference(Reference::variable(variable));
        }
        Node::Identifier(token) => arena[*scope].get_mut().definition(Symbol::from(token)),
        Node::Literal(token) => {
            if token.is_identifier() {
                arena[*scope]
                    .get_mut()
                    .prepare_reference(Reference::identifier(token));
            }
        }
        Node::TypeRef(parts) => {
            arena[*scope]
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
            arena[*scope]
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
