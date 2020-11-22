use crate::environment::import::SymbolImport;
use crate::environment::scope::Reference;
use crate::node::get_range;
use crate::token::{Token, TokenType};
use indextree::{Arena, NodeId};
use std::cmp::PartialOrd;
use tower_lsp::lsp_types::{Diagnostic, DocumentSymbol, Range, SymbolKind};

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
    /// While multi inheritance is not supported in PHP, we still collect all parents
    /// to display meaningful error messages
    pub inherits_from: Vec<Reference>,

    /// Id of the node this node references (if it is not a definition)
    pub references: Option<Reference>,

    /// Ids referencing this node
    pub references_by: Vec<NodeId>,

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
            inherits_from: Vec::new(),
            references: None,
            references_by: Vec::new(),
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
        matches!(self, PhpSymbolKind::Class
            | PhpSymbolKind::Interface
            | PhpSymbolKind::Function
            | PhpSymbolKind::Constant)
    }
}

/// Basically a 1:1 mapping that omits the data type
impl Symbol {
    pub fn detail(&self) -> Option<String> {
        match self.kind {
            PhpSymbolKind::Method => Some(
                self.data_types
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(" | "),
            ),
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

    pub fn hover_text(&self, arena: &Arena<Symbol>, me: &NodeId) -> String {
        format!(
            "{} ({:?}) < {:?}",
            self.name,
            self.kind,
            arena[arena[*me].parent().unwrap()].get().kind
        )
    }

    /// Find a direct descendant of node by its name
    pub fn find_child_by_name(
        &self,
        arena: &Arena<Symbol>,
        node: &NodeId,
        name: &str,
    ) -> Option<NodeId> {
        for child in node.children(arena) {
            if arena[child].get().name == name {
                return Some(child);
            } else {
                eprintln!("{} != {}", arena[child].get().name, name);
            }
        }

        None
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
