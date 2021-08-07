use super::{
    get_range,
    import::{SymbolImportBlock, TraitUseAlteration},
    in_range,
    visitor::name_resolver::NameResolver,
};
use crate::environment::fs::file_read_range;
use crate::environment::scope::Reference;
use crate::parser::node::Node as AstNode;
use crate::parser::token::{Token, TokenType};
use indextree::{Arena, NodeId};
use lsp_types::SymbolTag;
use std::{cmp::PartialOrd, collections::HashMap, fmt::Display};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionItemTag, DocumentSymbol, InsertTextFormat,
    Position, Range, SymbolKind,
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
    FunctionParameter = 102,
    Trait = 103,

    // Capturing all unknown enums by this lib.
    Unknown = 255,
}
#[derive(Clone, Debug, PartialEq, Copy)]
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

/// Struct representing a function parameter. Its a wrapper around the Symbols representing
/// the parameters and adds some additional information
#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub symbol: NodeId,
    pub ast: AstNode,
}

impl FunctionParameter {
    pub fn new(symbol: &NodeId, ast: &AstNode) -> Self {
        Self {
            symbol: symbol.to_owned(),
            ast: ast.clone(),
        }
    }
}

/// Struct to store the alias to a symbol, for instance the alias to a
/// method name. This should also the only use case for now, methods that
/// are equipped with an alias during inclusion via traits. The alias could
/// also be the original name.
#[derive(Copy, Clone)]
pub struct SymbolAlias<'a> {
    pub symbol: NodeId,
    pub alias: &'a str,
    pub visibility: Visibility,
    pub origin: NodeId,
}

impl<'a> SymbolAlias<'a> {
    pub fn new(symbol: NodeId, alias: &'a str, visibility: Visibility, origin: NodeId) -> Self {
        SymbolAlias {
            symbol,
            alias,
            visibility,
            origin,
        }
    }

    /// Create an alias for a previous import which is now available under a new name
    pub fn aliased(&self, alias: &'a str) -> Self {
        Self { alias, ..*self }
    }

    /// Pass this alias along, which means updating its origin. For example, if trait a inherits symbol x from trait b
    /// and trait a ends up being used in class C, class C does not need to know that symbol x originates from trait b
    /// but that it comes via trait a, because trait a is what will be referenced for the alterations
    pub fn pass_along(self, new_origin: NodeId) -> Self {
        SymbolAlias {
            origin: new_origin,
            ..self
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

    /// Optional namespace this symbol is defined in
    pub namespace: Option<String>,

    /// If symbol is a file, this contains the namespace imports. If its a class
    /// this contains the used traits
    pub imports: Option<SymbolImportBlock>,

    /// Contains all the use x insteadof y or as a public b trait import rules
    pub import_resolutions: Option<Vec<TraitUseAlteration>>,

    /// Parent symbol(s) (used for inheritance)
    /// While multi inheritance is not supported in PHP class, we still collect all parents
    /// as its supported in interfaces
    pub inherits_from: Option<Vec<Reference>>,

    /// Ids of the symbols defining the possible types this symbol can have. This includes
    /// return types or implemented interfaces
    pub data_types: Vec<Reference>,

    /// An optional set of parameters. This is mainly for methods and functions.
    pub parameters: Vec<FunctionParameter>,

    /// True if this value was declared static
    pub is_static: bool,

    /// The visibility of the symbol
    pub visibility: Visibility,

    /// Can this symbol be used as an attribute?
    pub is_attribute: bool,
}

impl Default for Symbol {
    fn default() -> Self {
        Symbol {
            namespace: None,
            name: String::new(),
            kind: PhpSymbolKind::File,
            range: get_range(((0, 0), (0, 0))),
            selection_range: get_range(((0, 0), (0, 0))),
            deprecated: None,
            inherits_from: None,
            data_types: Vec::new(),
            is_static: false,
            imports: None,
            import_resolutions: None,
            parameters: Vec::new(),
            visibility: Visibility::None,
            is_attribute: false,
        }
    }
}

impl PhpSymbolKind {
    pub fn get_symbol_kind(&self) -> Option<SymbolKind> {
        let kind = match self {
            PhpSymbolKind::File => SymbolKind::File,
            PhpSymbolKind::Namespace => SymbolKind::Namespace,
            PhpSymbolKind::Class | PhpSymbolKind::Trait => SymbolKind::Class,
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
        matches!(
            self,
            PhpSymbolKind::Unknown | PhpSymbolKind::Import | PhpSymbolKind::MagicConst
        )
    }

    pub fn register_global(&self) -> bool {
        matches!(
            self,
            PhpSymbolKind::Class
                | PhpSymbolKind::Trait
                | PhpSymbolKind::Interface
                | PhpSymbolKind::Function
                | PhpSymbolKind::Constant
        )
    }
}

/// Basically a 1:1 mapping that omits the data type
impl Symbol {
    pub fn completion_item(&self, node: NodeId, arena: &Arena<Symbol>) -> CompletionItem {
        let tags = if let Some(true) = self.deprecated {
            Some(vec![CompletionItemTag::Deprecated])
        } else {
            None
        };

        let ns = if let Some(ns) = self.namespace.as_ref() {
            ns
        } else {
            ""
        };

        let label = self.name().to_string();

        match self.kind {
            PhpSymbolKind::Interface => CompletionItem {
                label,
                detail: Some(format!("interface {}\\{}", ns, self.name)),
                kind: Some(CompletionItemKind::Interface),
                tags,
                ..CompletionItem::default()
            },
            PhpSymbolKind::Class => CompletionItem {
                label,
                detail: Some(format!("class {}\\{}", ns, self.name)),
                kind: Some(CompletionItemKind::Class),
                tags,
                ..CompletionItem::default()
            },
            PhpSymbolKind::Trait => CompletionItem {
                label,
                detail: Some(format!("trait {}\\{}", ns, self.name)),
                kind: Some(CompletionItemKind::Class),
                tags,
                ..CompletionItem::default()
            },
            PhpSymbolKind::Constant => CompletionItem {
                label,
                detail: Some(format!("{} {}()", self.visibility, self.name)),
                kind: Some(CompletionItemKind::Constant),
                tags,
                ..CompletionItem::default()
            },
            PhpSymbolKind::MagicConst => CompletionItem {
                label,
                detail: None,
                kind: Some(CompletionItemKind::Constant),
                tags,
                ..CompletionItem::default()
            },
            PhpSymbolKind::Method => CompletionItem {
                label,
                detail: Some(format!("{} {}()", self.visibility, self.name)),
                kind: Some(CompletionItemKind::Method),
                insert_text: Some(format!(
                    "{}({})",
                    self.name,
                    self.parameters_as_snippets(node, arena)
                )),
                insert_text_format: Some(InsertTextFormat::Snippet),
                tags,
                ..CompletionItem::default()
            },

            PhpSymbolKind::Function => CompletionItem {
                label,
                detail: Some(format!("{}()", self.name)),
                kind: Some(CompletionItemKind::Function),
                insert_text: Some(format!(
                    "{}({})",
                    self.name,
                    self.parameters_as_snippets(node, arena)
                )),
                insert_text_format: Some(InsertTextFormat::Snippet),
                tags,
                ..CompletionItem::default()
            },
            _ => CompletionItem {
                label,
                tags,
                ..CompletionItem::default()
            },
        }
    }

    fn parameters<'a>(&self, node: NodeId, arena: &'a Arena<Symbol>) -> Vec<&'a str> {
        node.children(arena)
            .map(|c| arena[c].get().name())
            .collect::<Vec<&str>>()
    }

    fn parameters_as_snippets(&self, node: NodeId, arena: &Arena<Symbol>) -> String {
        self.parameters(node, arena)
            .iter()
            .enumerate()
            .map(|(i, c)| format!("${{{}:\\${}}}", i + 1, c))
            .collect::<Vec<String>>()
            .join(", ")
    }

    pub fn name(&self) -> &str {
        if self.name.starts_with('$') {
            &self.name[1..]
        } else {
            &self.name
        }
    }

    pub fn normalized_name(&self) -> String {
        self.name().to_lowercase()
    }

    pub fn fqdn(&self) -> String {
        if let Some(namespace) = self.namespace.as_ref() {
            format!("{}\\{}", namespace, self.name())
        } else {
            self.name().to_string()
        }
    }

    pub fn fqdn_matches(&self, pattern: &str) -> bool {
        let fqdn = self.fqdn();

        if fqdn.eq(pattern) {
            return true;
        }

        let pattern = &format!("\\{}", pattern);
        if fqdn.eq(pattern) {
            return true;
        }

        let fqdn = format!("\\{}", fqdn);

        fqdn.eq(pattern)
    }

    pub fn symbol_at(
        &self,
        position: &Position,
        my_node_id: NodeId,
        arena: &Arena<Self>,
    ) -> NodeId {
        for c in my_node_id.children(arena) {
            let s = arena[c].get();

            // Ignore namespaces as they span the entire document but do not contain symbols
            if s.kind == PhpSymbolKind::Namespace || s.kind == PhpSymbolKind::MagicConst {
                continue;
            }

            if in_range(position, &s.range) {
                return s.symbol_at(position, c, arena);
            }
        }

        my_node_id
    }

    /// Collect all inherited symbols of all parents (interfaces have more than one parent)
    pub fn get_inherited_symbols<'a, 'b>(
        &'a self,
        ctx: NodeId,
        resolver: &'b mut NameResolver,
        arena: &'a Arena<Self>,
    ) -> HashMap<String, SymbolAlias> {
        let mut inherited_symbols = self
            .get_parent_nodes(ctx, resolver, arena)
            .iter()
            .map(|(_, parent)| {
                arena[*parent]
                    .get()
                    .get_all_symbols(*parent, resolver, arena)
            })
            .fold(HashMap::new(), |cur, mut tot| {
                tot.extend(cur);
                tot
            });

        inherited_symbols.iter_mut().for_each(|(_, alias)| {
            *alias = alias.pass_along(ctx);
        });

        inherited_symbols
    }

    // Resolve children of all imports
    pub fn get_imports<'a, 'b>(
        &'a self,
        ctx: NodeId,
        resolver: &'b mut NameResolver,
        arena: &'a Arena<Self>,
    ) -> HashMap<String, SymbolAlias> {
        // Step 1: Go through all used traits and get the children of all traits
        let mut all_children = HashMap::new();
        if let Some(imports) = self.imports.as_ref() {
            for import in imports.all() {
                if let Some(used_trait) =
                    resolver.resolve_type_ref(&import.path, arena, &ctx, false)
                {
                    let symbol = arena[used_trait].get();

                    // Prevent inclusion of false positives, mainly in the php storm stubs
                    if symbol.kind != PhpSymbolKind::Trait {
                        continue;
                    }

                    // Get HashMap<String, Vec<NodeId>> where nodeids are all symbols with the same name (i.e. conflicting)
                    symbol
                        .get_all_symbols(used_trait, resolver, arena)
                        .drain()
                        .for_each(|(name, node)| {
                            all_children.entry(name).or_insert_with(Vec::new).push(node);
                        });
                }
            }
        }

        // Keep a copy as some symbols may be added later again via as
        let orig_all_children = all_children.clone();

        // Step 2: Do all insteadofs
        if let Some(resolutions) = self.import_resolutions.as_ref() {
            for resolution in resolutions {
                if let TraitUseAlteration::InsteadOf {
                    class,
                    instead_ofs,
                    member,
                    ..
                } = resolution
                {
                    let loser_traits = instead_ofs
                        .iter()
                        .filter_map(|tr| resolver.resolve_type_ref(tr, arena, &ctx, false))
                        .collect::<Vec<NodeId>>();

                    if let Some(winner_trait) = resolver.resolve_type_ref(class, arena, &ctx, false)
                    {
                        let normalized_member = member.to_lowercase();

                        if let Some(aliases) = all_children.get_mut(&normalized_member) {
                            // Only keep the aliases that are either the winner or not in the list of losers
                            aliases.retain(|alias| {
                                alias.origin == winner_trait
                                    || !loser_traits.contains(&alias.origin)
                            });
                        }
                    }
                }
            }
        }

        // Drain into result map
        let mut children = HashMap::new();
        all_children.drain().for_each(|(key, val)| {
            children.insert(key, *val.first().unwrap());
        });

        if let Some(resolutions) = self.import_resolutions.as_ref() {
            // Step 3: Do all aliases and visibility modifiers
            for resolution in resolutions {
                if let TraitUseAlteration::As {
                    visibility,
                    class,
                    member,
                    alias,
                } = resolution
                {
                    // class::member as private
                    // class::member as otherMember
                    // member as private
                    // member as OtherMember

                    let normalized_member = &member.to_lowercase();

                    let visibility = Visibility::from(visibility);

                    // Do we need to add a new alias for the remaining symbol named "member"?
                    if let Some(alias) = alias {
                        if let Some(label) = alias.label.as_ref() {
                            let mut new_alias = if let Some(class) = class {
                                if let Some(resolved_class) =
                                    resolver.resolve_type_ref(class, arena, &ctx, false)
                                {
                                    if let Some(origs) = orig_all_children.get(normalized_member) {
                                        if let Some(orig) =
                                            origs.iter().find(|orig| orig.origin == resolved_class)
                                        {
                                            orig.aliased(label)
                                        } else {
                                            // Original name exists but not from the trait "class"
                                            continue;
                                        }
                                    } else {
                                        // Original name never existed
                                        continue;
                                    }
                                } else {
                                    // The class can not be resolved, so the user is using a trait name that is
                                    // not available
                                    continue;
                                }
                            } else if let Some(to_be_aliased) = children.get(normalized_member) {
                                to_be_aliased.aliased(label)
                            } else {
                                // member does not exist
                                continue;
                            };

                            if visibility != Visibility::None {
                                new_alias.visibility = visibility;
                            }

                            children.insert(label.to_lowercase(), new_alias);
                        }
                    // No additional alias, just a change in visibility
                    } else if visibility != Visibility::None {
                        if let Some(symbol_alias) = children.get_mut(normalized_member) {
                            symbol_alias.visibility = visibility;
                        }
                    }
                }
            }
        }

        children
    }

    /// Collect all available symbols, including used through traits and symbols of parents
    pub fn get_all_symbols<'a, 'b>(
        &'a self,
        ctx: NodeId,
        resolver: &'b mut NameResolver,
        arena: &'a Arena<Self>,
    ) -> HashMap<String, SymbolAlias> {
        let mut children = HashMap::new();

        // Get this symbols children
        ctx.children(arena).for_each(|child| {
            let child_symbol = arena[child].get();

            children.insert(
                child_symbol.normalized_name(),
                SymbolAlias::new(child, child_symbol.name(), child_symbol.visibility, ctx),
            );
        });

        // Go through all used traits and get the children of all traits
        children.extend(self.get_imports(ctx, resolver, arena));

        // Get the children of all data_types, which are also all implemented interfaces
        let resolved_datatypes = self
            .data_types
            .iter()
            .filter_map(|dt| resolver.resolve_type_ref(&dt.type_ref, arena, &ctx, false))
            .collect::<Vec<NodeId>>();

        resolved_datatypes.iter().for_each(|dt| {
            // Skip self-reference
            if ctx != *dt {
                children.extend(arena[*dt].get().get_all_symbols(*dt, resolver, arena));
            }
        });

        children.extend(self.get_inherited_symbols(ctx, resolver, arena));
        children
    }

    pub fn get_unique_parent(
        &self,
        ctx: NodeId,
        resolver: &mut NameResolver,
        arena: &Arena<Self>,
    ) -> Option<NodeId> {
        let parents = self.get_parent_nodes(ctx, resolver, arena);

        if parents.len() != 1 {
            None
        } else {
            Some(*parents.values().next().unwrap())
        }
    }

    pub fn get_parent_nodes(
        &self,
        ctx: NodeId,
        resolver: &mut NameResolver,
        arena: &Arena<Self>,
    ) -> HashMap<String, NodeId> {
        let mut parents = HashMap::new();

        if let Some(inherits_from) = self.inherits_from.as_ref() {
            inherits_from
                .iter()
                .filter_map(|r| resolver.resolve_type_ref(&r.type_ref, &arena, &ctx, false))
                .for_each(|node| {
                    parents.insert(arena[node].get().normalized_name(), node);
                })
        }

        parents
    }

    pub fn get_parent_symbols<'a>(
        &'a self,
        ctx: NodeId,
        resolver: &mut NameResolver,
        arena: &'a Arena<Self>,
    ) -> Vec<&'a Self> {
        self.get_parent_nodes(ctx, resolver, &arena)
            .iter()
            .map(|(_, parent)| arena[*parent].get())
            .collect()
    }

    pub fn detail(&self, arena: &Arena<Symbol>) -> Option<String> {
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
            PhpSymbolKind::Variable => {
                let types = self
                    .data_types
                    .iter()
                    .filter_map(|r| r.node)
                    .map(|n| arena[n].get().name.to_string())
                    .collect::<Vec<String>>()
                    .join(" | ");

                Some(format!("{}", types))
            }
            _ => None,
        }
    }

    pub fn to_doc_sym(&self, arena: &Arena<Symbol>, node: &NodeId) -> Option<DocumentSymbol> {
        let children = node
            .children(arena)
            .filter(|s| !arena[*s].get().kind.is_internal())
            .filter_map(|s| arena[s].get().to_doc_sym(arena, &s))
            .collect::<Vec<DocumentSymbol>>();

        let children = if children.is_empty() {
            None
        } else {
            Some(children)
        };

        let kind = self.kind.get_symbol_kind()?;

        Some(DocumentSymbol {
            kind,
            name: self.name.clone(),
            range: self.range,
            selection_range: self.selection_range,
            detail: self.detail(arena),
            deprecated: None,
            tags: if self.deprecated.is_some() {
                Some(vec![SymbolTag::Deprecated])
            } else {
                None
            },
            // Needs to be added from outside of this
            children,
        })
    }

    pub fn hover_text(&self, node: NodeId, arena: &Arena<Self>) -> String {
        if let Some(file) = node.ancestors(arena).find_map(|a| {
            let ancestor = arena[a].get();

            if ancestor.kind == PhpSymbolKind::File {
                Some(ancestor)
            } else {
                None
            }
        }) {
            let doc = file_read_range(&file.name, self.range.start.line, self.range.end.line);

            format!(
                "
# Documentation
```php
<?php
{}
```
",
                doc
            )
        } else {
            self.name.clone()
        }
    }
}
