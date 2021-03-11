use super::{get_range, in_range, visitor::name_resolver::NameResolver};
use crate::environment::import::SymbolImport;
use crate::environment::scope::Reference;
use crate::parser::token::{Token, TokenType};
use indextree::{Arena, NodeId};
use std::{cmp::PartialOrd, fmt::Display};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemTag, Diagnostic, DocumentSymbol, Position,
    Range, SymbolKind,
};

/// An extension if the LSP-type "SymbolKind"
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
#[repr(u8)]
pub enum PhpSymbolKind {
    File = 1,
    Namespace = 3,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    Object = 19,
    Key = 20,
    Null = 21,
    Operator = 25,
    Import = 26,

    // Custom types
    BuiltInType = 100,
    MagicConst = 101,

    // Capturing all unknown enums by this lib.
    Unknown = 255,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Visibility {
    None,
    Public,
    Protected,
    Private,
}

impl Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Visibility::None => "",
                Visibility::Public => "public",
                Visibility::Protected => "protected",
                Visibility::Private => "private",
            },
        )
    }
}

impl PartialOrd for Visibility {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self == other {
            return Some(std::cmp::Ordering::Equal);
        }

        if *self == Visibility::None || *self == Visibility::Public {
            return Some(std::cmp::Ordering::Greater);
        }

        if *self == Visibility::Private {
            return Some(std::cmp::Ordering::Less);
        }

        if *other == Visibility::Private {
            return Some(std::cmp::Ordering::Greater);
        }

        Some(std::cmp::Ordering::Less)
    }
}

impl From<&Option<Token>> for Visibility {
    fn from(token: &Option<Token>) -> Self {
        if let Some(token) = token {
            match token.t {
                TokenType::Private => Visibility::Private,
                TokenType::Protected => Visibility::Protected,
                TokenType::Public => Visibility::Public,
                _ => Visibility::None,
            }
        } else {
            Visibility::None
        }
    }
}

/// Contains information about a symbol in a scope. This can be a function, a class, a variable etc.
/// It is bacially an extended `lsp_types::DocumentSymbol` that also contains a data type (for vars and properties)
#[derive(Clone, Debug)]
pub struct Symbol {
    /// The data type of this symbol. Of course not all symbols have data types (Namespaces for example do not), so
    /// this remains an `Option`
    pub kind: PhpSymbolKind,
    pub name: String,
    pub range: Range,
    pub selection_range: Range,
    pub deprecated: Option<bool>,

    pub imports: Option<Vec<SymbolImport>>,

    /// Parent symbol(s) (used for inheritance)
    /// While multi inheritance is not supported in PHP class, we still collect all parents
    /// as its supported in interfaces
    pub inherits_from: Option<Vec<Reference>>,

    /// Ids of the symbols defining the possible types this symbol can have
    pub data_types: Vec<Reference>,

    /// True if this value was declared static
    pub is_static: bool,

    /// The visibility of the symbol
    pub visibility: Visibility,
}

impl Default for Symbol {
    fn default() -> Self {
        Symbol {
            name: String::new(),
            kind: PhpSymbolKind::File,
            range: get_range(((0, 0), (0, 0))),
            selection_range: get_range(((0, 0), (0, 0))),
            deprecated: None,
            inherits_from: None,
            data_types: Vec::new(),
            is_static: false,
            imports: None,
            visibility: Visibility::None,
        }
    }
}

impl PhpSymbolKind {
    pub fn to_symbol_kind(&self) -> Option<SymbolKind> {
        let kind = match self {
            PhpSymbolKind::File => SymbolKind::File,
            PhpSymbolKind::Namespace => SymbolKind::Namespace,
            PhpSymbolKind::Class => SymbolKind::Class,
            PhpSymbolKind::Method => SymbolKind::Method,
            PhpSymbolKind::Property => SymbolKind::Property,
            PhpSymbolKind::Field => SymbolKind::Field,
            PhpSymbolKind::Constructor => SymbolKind::Constructor,
            PhpSymbolKind::Interface => SymbolKind::Interface,
            PhpSymbolKind::Function => SymbolKind::Function,
            PhpSymbolKind::Variable => SymbolKind::Variable,
            PhpSymbolKind::Constant => SymbolKind::Constant,
            PhpSymbolKind::String => SymbolKind::String,
            PhpSymbolKind::Number => SymbolKind::Number,
            PhpSymbolKind::Boolean => SymbolKind::Boolean,
            PhpSymbolKind::Array => SymbolKind::Array,
            PhpSymbolKind::Object => SymbolKind::Object,
            PhpSymbolKind::Key => SymbolKind::Key,
            PhpSymbolKind::Null => SymbolKind::Null,
            PhpSymbolKind::Operator => SymbolKind::Operator,
            PhpSymbolKind::BuiltInType => return None,
            _ => SymbolKind::Unknown,
        };

        Some(kind)
    }

    pub fn is_internal(&self) -> bool {
        matches!(self, PhpSymbolKind::Unknown | PhpSymbolKind::Import)
    }

    pub fn register_global(&self) -> bool {
        matches!(
            self,
            PhpSymbolKind::Class
                | PhpSymbolKind::Interface
                | PhpSymbolKind::Function
                | PhpSymbolKind::Constant
        )
    }
}

impl Into<CompletionItem> for &Symbol {
    fn into(self) -> CompletionItem {
        let tags = if let Some(true) = self.deprecated {
            Some(vec![CompletionItemTag::Deprecated])
        } else {
            None
        };

        match self.kind {
            PhpSymbolKind::Method => CompletionItem {
                label: self.name.clone(),
                detail: Some(format!("{} {}()", self.visibility, self.name)),
                kind: Some(CompletionItemKind::Method),
                insert_text: Some(format!("{}()", self.name)),
                tags,
                ..CompletionItem::default()
            },
            _ => CompletionItem {
                label: self.name.clone(),
                tags,
                ..CompletionItem::default()
            },
        }
    }
}

/// Basically a 1:1 mapping that omits the data type
impl Symbol {
    pub fn symbol_at(
        &self,
        position: &Position,
        my_node_id: NodeId,
        arena: &Arena<Self>,
    ) -> NodeId {
        for c in my_node_id.children(arena) {
            let s = arena[c].get();

            // Ignore namespaces as they span the entire document but do not contain symbols
            if s.kind == PhpSymbolKind::Namespace {
                continue;
            }

            if in_range(position, &s.range) {
                return s.symbol_at(position, c, arena);
            }
        }

        return my_node_id;
    }

    /// Collect all inherited symbols of all parents (interfaces have more than one parent)
    pub fn get_inherited_symbols(
        &self,
        my_node_id: &NodeId,
        resolver: &mut NameResolver,
        arena: &Arena<Self>,
    ) -> Vec<NodeId> {
        self.get_parent_nodes(my_node_id, resolver, arena)
            .iter()
            .map(|parent| {
                let mut children = parent.children(arena).collect::<Vec<NodeId>>();

                children.extend(
                    arena[*parent]
                        .get()
                        .get_inherited_symbols(parent, resolver, arena),
                );

                children
            })
            .fold(Vec::new(), |cur, mut tot| {
                tot.extend(cur);
                tot
            })
    }

    /// Collect all available symbols, including used through traits and symbols of parents
    pub fn get_all_symbols(
        &self,
        my_node_id: &NodeId,
        resolver: &mut NameResolver,
        arena: &Arena<Self>,
    ) -> Vec<NodeId> {
        let mut children = my_node_id.children(arena).collect::<Vec<NodeId>>();

        // Go through all used traits
        if let Some(imports) = self.imports.as_ref() {
            for import in imports.iter() {
                if let Some(used_trait) =
                    resolver.resolve_type_ref(&import.path, arena, &my_node_id, true)
                {
                    children.extend(used_trait.children(arena));
                }
            }
        }

        children.extend(self.get_inherited_symbols(my_node_id, resolver, arena));

        children
    }

    pub fn get_unique_parent(
        &self,
        my_node_id: &NodeId,
        resolver: &mut NameResolver,
        arena: &Arena<Self>,
    ) -> Option<NodeId> {
        let parents = self.get_parent_nodes(my_node_id, resolver, arena);

        if parents.len() != 1 {
            None
        } else {
            Some(parents.first().unwrap().clone())
        }
    }

    pub fn get_parent_nodes(
        &self,
        my_node_id: &NodeId,
        resolver: &mut NameResolver,
        arena: &Arena<Self>,
    ) -> Vec<NodeId> {
        if let Some(inherits_from) = self.inherits_from.as_ref() {
            inherits_from
                .iter()
                .filter_map(|r| {
                    resolver.resolve_type_ref(
                        &r.type_ref.as_ref().unwrap(),
                        &arena,
                        &my_node_id,
                        false,
                    )
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    pub fn get_parent_symbols<'a>(
        &'a self,
        my_node_id: &NodeId,
        resolver: &mut NameResolver,
        arena: &'a Arena<Self>,
    ) -> Vec<&'a Self> {
        self.get_parent_nodes(my_node_id, resolver, &arena)
            .iter()
            .map(|parent| arena[*parent].get())
            .collect()
    }

    pub fn detail(&self) -> Option<String> {
        match self.kind {
            PhpSymbolKind::Method => {
                let types = self
                    .data_types
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(" | ");

                Some(format!(
                    "{} function {}(): {}",
                    self.visibility, self.name, types
                ))
            }
            _ => None,
        }
    }

    pub fn to_doc_sym(&self, arena: &Arena<Symbol>, node: &NodeId) -> Option<DocumentSymbol> {
        let children = node
            .children(arena)
            .filter(|s| !arena[*s].get().kind.is_internal())
            .map(|s| arena[s].get().to_doc_sym(arena, &s))
            .filter(std::option::Option::is_some)
            .map(std::option::Option::unwrap)
            .collect::<Vec<DocumentSymbol>>();

        let children = if children.is_empty() {
            None
        } else {
            Some(children)
        };

        let kind = self.kind.to_symbol_kind()?;

        Some(DocumentSymbol {
            kind,
            name: self.name.clone(),
            range: self.range,
            selection_range: self.selection_range,
            detail: self.detail(),
            deprecated: self.deprecated,

            // Needs to be added from outside of this
            children,
        })
    }

    pub fn hover_text(&self) -> String {
        format!("{} ({:?})", self.name, self.kind)
    }
}

impl From<&Symbol> for Diagnostic {
    fn from(reference: &Symbol) -> Diagnostic {
        Diagnostic {
            range: reference.range,
            message: format!("{:#?}", reference),
            ..Diagnostic::default()
        }
    }
}
