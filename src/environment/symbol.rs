use crate::environment::import::SymbolImport;
use crate::environment::scope::{collect_symbols, Reference};
use crate::node::{get_range, Node};
use crate::token::{Token, TokenType};
use indextree::{Arena, NodeId};
use std::collections::HashMap;
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

    // Custom types
    BuiltInType = 100,

    // Capturing all unknown enums by this lib.
    Unknown = 255,
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
    pub detail: Option<String>,
    pub deprecated: Option<bool>,

    pub imports: Option<Vec<SymbolImport>>,

    /// Parent symbol(s) (used for inheritance)
    /// While multi inheritance is not supported in PHP, we still collect all parents
    /// to display meaningful error messages
    pub inherits_from: Vec<Reference>,

    /// Parent within a call, for example: $someObject->this;
    pub parent: Option<NodeId>,

    /// Id of the node this node references (if it is not a definition)
    pub references: Option<Reference>,

    /// Ids referencing this node
    pub references_by: Vec<NodeId>,

    /// Ids of the symbols defining the possible types this symbol can have
    pub data_types: Vec<Reference>,

    /// True if this value was declared static
    pub is_static: bool,
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
            PhpSymbolKind::Unknown => SymbolKind::Unknown,
        };

        Some(kind)
    }
}

/// Basically a 1:1 mapping that omits the data type
impl Symbol {
    pub fn to_doc_sym(&self, arena: &Arena<Symbol>, node: &NodeId) -> Option<DocumentSymbol> {
        let children = node
            .children(arena)
            .filter(|s| arena[*s].get().kind != PhpSymbolKind::Unknown)
            .map(|s| arena[s].get().to_doc_sym(arena, &s))
            .filter(|s| s.is_some())
            .map(|s| s.unwrap())
            .collect::<Vec<DocumentSymbol>>();

        let children = if children.is_empty() {
            None
        } else {
            Some(children)
        };

        let kind = if let Some(kind) = self.kind.to_symbol_kind() {
            kind
        } else {
            return None;
        };

        Some(DocumentSymbol {
            kind,
            name: self.name.clone(),
            range: self.range,
            selection_range: self.selection_range,
            detail: self.detail.clone(),
            deprecated: self.deprecated,

            // Needs to be added from outside of this
            children,
        })
    }

    pub fn hover_text(&self, arena: &Arena<Symbol>, me: &NodeId) -> String {
        if let Some(parent) = self.parent {
            format!(
                "{}, child of {}",
                self.name,
                arena[parent].get().hover_text(&arena, me)
            )
        } else {
            format!(
                "{} ({:?}) < {:?}",
                self.name,
                self.kind,
                arena[arena[*me].parent().unwrap()].get().kind
            )
        }
    }

    /// Resolve this symbol to the defining symbol (can also be itself)
    /// Bugs
    /// - absolute pathes like \Rofl\Copter do not work
    pub fn resolve(
        &self,
        arena: &Arena<Symbol>,
        node: &NodeId,
        global_symbols: &HashMap<String, NodeId>,
    ) -> Option<NodeId> {
        match self.kind {
            PhpSymbolKind::Variable => {
                // Find first instance this one was named and return it
                // If nothing can be found, this is the definition
                let parent_node = arena[*node].parent().unwrap();

                // $this resolves to the parent class, which is the grant parent if $this (as there
                // is a method in between)
                // TODO Make sure only classes support $this
                if self.name == "this" {
                    return Some(arena[parent_node].parent().unwrap());
                }

                for symbol in parent_node.children(arena) {
                    // Do not compare to itself
                    if symbol == *node {
                        continue;
                    }

                    let found_symbol = arena[symbol].get();

                    if found_symbol.kind == PhpSymbolKind::Variable
                        && found_symbol.name == self.name
                    {
                        return Some(symbol);
                    }
                }

                Some(*node)
            }
            PhpSymbolKind::Unknown => {
                // Resolve reference

                // This is the case if we are dealing with a method / property of another class
                if let Some(&parent_node) = self.parent.as_ref() {
                    if let Some(parent_definition) =
                        arena[parent_node]
                            .get()
                            .resolve(arena, &parent_node, global_symbols)
                    {
                        // For example the $object in $object->method() (when trying to resolve
                        // method)
                        let parent_symbol = arena[parent_definition].get();

                        if parent_symbol.kind == PhpSymbolKind::Class {
                            // TODO: Also check parents of this class if necessary
                            for child in parent_definition.children(arena) {
                                if arena[child].get().name == self.name {
                                    return Some(child);
                                }
                            }
                        }

                        // Get all possible data types. Thanks to union types and php docs with
                        // mulitple annotated types we need to check multiple data types.
                        for data_type in parent_symbol.data_types.iter() {
                            // Try to resolve the data type, revealing the defining class for example
                            if let Some(mut data_type_symbol_node) =
                                resolve_reference(data_type, arena, node, global_symbols)
                            {
                                // Lookup loop that goes through the data_type class and its parents
                                'check_next_parent: loop {
                                    // Go through this symbols children and try to find a match
                                    for child in data_type_symbol_node.children(arena) {
                                        if arena[child].get().name == self.name {
                                            return Some(child);
                                        }
                                    }

                                    let data_type_symbol = arena[data_type_symbol_node].get();

                                    // Maybe the data_type_symbol imports a trait that defines the
                                    // symbol?
                                    /*for import in data_type_symbol.imports.iter() {
                                        // let resolved_import =
                                        for child in data_type_symbol_node.children(arena) {
                                            if arena[child].get().name == self.name {
                                                return Some(child);
                                            }
                                        }
                                    }*/

                                    // Next chance: inheritance! Maybe the parent defines the method or property
                                    // we are looking for
                                    // Note again: Multi inheritance is invalid in PHP, but we want to understand
                                    // what the user intended as much as possible, so we let this
                                    // be a vec and only check the first entry that is resolvable
                                    for inherits_from in data_type_symbol.inherits_from.iter() {
                                        // Try to resolve the class which this class inherits from
                                        if let Some(resolved_inheritance_parent) = resolve_reference(
                                            inherits_from,
                                            arena,
                                            &data_type_symbol_node,
                                            global_symbols,
                                        ) {
                                            data_type_symbol_node = resolved_inheritance_parent;

                                            continue 'check_next_parent;
                                        }
                                    }

                                    // Either no parents or no parent was resolvable
                                    break;
                                }
                            }
                        }

                        return None;
                    }
                }

                // TODO Make sure only classes support $this
                if self.name == "self" {
                    let parent_node = arena[*node].parent().unwrap();
                    return Some(arena[parent_node].parent().unwrap());
                }

                if let Some(reference) = self.references.as_ref() {
                    return resolve_reference(reference, arena, node, global_symbols);
                }

                None
            }
            // All else is a definition
            _ => Some(*node),
        }
    }
}

fn resolve_reference(
    reference: &Reference,
    arena: &Arena<Symbol>,
    node: &NodeId,
    global_symbols: &HashMap<String, NodeId>,
) -> Option<NodeId> {
    let symbol_name = if let Some(token) = reference.token.as_ref() {
        token
            .label
            .clone()
            .unwrap_or_else(|| "empty 123".to_owned())
    } else if let Some(type_ref) = reference.type_ref.as_ref() {
        // If we are dealing with an absolute path, we can quickly check the global table and return
        if let Some(Token {
            t: TokenType::NamespaceSeparator,
            ..
        }) = type_ref.first().as_ref()
        {
            if let Some(node) = global_symbols.get(
                &type_ref
                    .iter()
                    .filter(|tok| tok.label.is_some())
                    .map(|tok| tok.label.clone().unwrap())
                    .collect::<Vec<String>>()
                    .join("\\"),
            ) {
                return Some(*node);
            } else {
                return None;
            }
        }

        type_ref
            .last()
            .unwrap()
            .label
            .clone()
            .unwrap_or_else(|| "le fuck".to_owned())
    } else {
        return None;
    };

    let mut node = arena[*node].parent().unwrap();

    loop {
        // Check symbols on this level
        for symbol in node.children(arena) {
            let found_symbol = arena[symbol].get();

            // TODO: Make sure not false positives with strings etc.
            if found_symbol.kind != PhpSymbolKind::Unknown && found_symbol.name == symbol_name {
                return Some(symbol);
            }
        }

        // Check imports if they exist
        let current_node = arena[node].get();
        if let Some(imports) = &current_node.imports {
            for import in imports {
                if import.name() == symbol_name {
                    let full_name = import.full_name();
                    if let Some(node) = global_symbols.get(&full_name) {
                        return Some(*node);
                    }
                }
            }
        }

        // If the current node is a namespace then the symbol could also be on the same level, it could
        // be a sibbling in the same namespace
        if current_node.kind == PhpSymbolKind::Namespace {
            if let Some(node) =
                global_symbols.get(&format!("{}\\{}", current_node.name, symbol_name))
            {
                return Some(*node);
            }
        // If the current node is the file then the last chance would be if the symbol is a global symbol from
        // the stdlib
        } else if current_node.kind == PhpSymbolKind::File {
            if let Some(global_symbol_node) = global_symbols.get(&symbol_name) {
                return Some(*global_symbol_node);
            }
        }

        if let Some(parent) = arena[node].parent() {
            // Up we go
            node = parent;
        } else {
            return None;
        }
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

fn get_type_ref(node: &Node) -> Option<Vec<Token>> {
    match node {
        Node::ReturnType { data_type, .. } => get_type_ref(data_type),
        Node::ArgumentType { type_ref, .. } | Node::DataType { type_ref, .. } => {
            match &**type_ref {
                Node::TypeRef(items) => Some(items.clone()),
                _ => None,
            }
        }
        Node::TypeRef(items) => Some(items.clone()),
        _ => None,
    }
}

pub fn document_symbol(
    arena: &mut Arena<Symbol>,
    enclosing: &NodeId,
    node: &Node,
    parent: Option<NodeId>,
) -> Result<NodeId, String> {
    match node {
        Node::Unary { expr, .. } => document_symbol(arena, enclosing, expr, parent),
        Node::Binary { left, right, token } => {
            // TODO: Check if left arm is a variable and the token is an assignment operator.
            // If so, get the type of the right arm and assign it as type to the variable
            let right = document_symbol(arena, enclosing, right, parent)?;
            let left = document_symbol(arena, enclosing, left, parent)?;

            if token.t == TokenType::Assignment {
                let references = { arena[right].get().references.clone() };
                if let Some(references) = references.as_ref() {
                    let left_symbol = arena[left].get_mut();
                    left_symbol.data_types.push(references.clone());
                }
            }

            enclosing.append(left, arena);
            enclosing.append(right, arena);

            Ok(left)
        }
        Node::Ternary {
            check,
            true_arm,
            false_arm,
            ..
        } => {
            let check = document_symbol(arena, enclosing, check, parent)?;
            enclosing.append(check, arena);
            if let Some(true_arm) = true_arm {
                let true_arm = document_symbol(arena, enclosing, true_arm, parent)?;

                enclosing.append(true_arm, arena);
                enclosing.append(document_symbol(arena, enclosing, false_arm, parent)?, arena);

                return Ok(true_arm);
            }
            let false_arm = document_symbol(arena, enclosing, false_arm, parent)?;

            enclosing.append(false_arm, arena);

            return Ok(false_arm);
        }
        Node::Call { callee, .. } => document_symbol(arena, enclosing, callee, parent),
        Node::Grouping(node) => document_symbol(arena, enclosing, node, parent),
        Node::New { class, .. } => document_symbol(arena, enclosing, class, parent),
        Node::Clone { object, .. } => document_symbol(arena, enclosing, object, parent),
        // Not working yet for some reason
        Node::StaticMember { class, member, .. } => {
            // Resolve the object (which can also be a callee)
            let object_node = document_symbol(arena, enclosing, class, parent)?;
            let member_node = document_symbol(arena, enclosing, member, Some(object_node))?;

            enclosing.append(member_node, arena);

            Ok(member_node)
        }
        // Not working yet for some reason
        Node::StaticMethod { class, method, .. } => {
            // Resolve the object (which can also be a callee)
            let object_node = document_symbol(arena, enclosing, class, parent)?;
            let member_node = document_symbol(arena, enclosing, method, Some(object_node))?;

            enclosing.append(member_node, arena);

            Ok(member_node)
        }
        // Remember: We see the members in reverse order! $a->b()->c() starts at c!
        Node::Member { object, member, .. } => {
            let mut stack = Vec::new();

            let mut current_object = object;
            let mut object_node = loop {
                match current_object.as_ref() {
                    Node::Call { callee, .. } => {
                        current_object = callee;
                    }
                    Node::Member { object, member, .. } => {
                        current_object = object;
                        stack.push(member);
                    }
                    _ => break document_symbol(arena, enclosing, current_object, parent)?,
                }
            };

            enclosing.append(object_node, arena);

            // Connect the stack items as parents to each other, but in reverse order to start with the second one (the first one was already
            // done in the loop before)
            for stack_member in stack.iter().rev() {
                object_node = document_symbol(arena, enclosing, stack_member, Some(object_node))?;

                enclosing.append(object_node, arena);
            }

            let member_node = document_symbol(arena, enclosing, member, Some(object_node))?;

            enclosing.append(member_node, arena);

            Ok(member_node)
        }
        Node::Field { array, index, .. } => {
            let array_node = document_symbol(arena, enclosing, array, parent)?;

            if let Some(index) = index {
                let index_node = document_symbol(arena, enclosing, index, Some(array_node))?;
                enclosing.append(array_node, arena);

                Ok(index_node)
            } else {
                Ok(array_node)
            }
        }
        Node::Literal(token) => {
            let kind = if token.is_string() {
                PhpSymbolKind::String
            } else if token.is_number() {
                PhpSymbolKind::Number
            } else {
                PhpSymbolKind::Unknown
            };

            let child = arena.new_node(Symbol {
                name: token.label.clone().unwrap_or_else(|| token.to_string()),
                kind,
                range: get_range(node.range()),
                selection_range: get_range(node.range()),
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);

            Ok(child)
        }
        Node::TypeRef(parts) => {
            let reference = Reference::type_ref(
                parts
                    .iter()
                    //.filter(|t| t.label.is_some())
                    .map(|t| t.clone())
                    .collect(),
            );

            let child = arena.new_node(Symbol {
                name: parts.last().unwrap().label.clone().unwrap(),
                kind: PhpSymbolKind::Unknown,
                range: get_range(node.range()),
                selection_range: get_range(node.range()),
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: Some(reference),
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);

            Ok(child)
        }

        Node::Const { name, .. } => {
            let child = arena.new_node(Symbol {
                name: name.clone().label.unwrap(),
                kind: PhpSymbolKind::Constant,
                range: get_range(node.range()),
                selection_range: get_range(node.range()),
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);

            Ok(child)
        }

        Node::LexicalVariable { variable, .. }
        | Node::StaticVariable { variable, .. }
        | Node::AliasedVariable { variable, .. }
        | Node::DynamicVariable { variable, .. } => {
            let child = arena.new_node(Symbol::from(variable));

            enclosing.append(child, arena);

            Ok(child)
        }

        Node::Function {
            token, return_type, ..
        } => {
            let data_types = if let Some(data_type) = return_type {
                if let Some(type_ref) = get_type_ref(data_type) {
                    vec![Reference::type_ref(type_ref)]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };

            let child = arena.new_node(Symbol {
                name: String::from("Anonymous function"),
                kind: PhpSymbolKind::Function,
                range: get_range(node.range()),
                selection_range: get_range(token.range()),
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types,
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        Node::FunctionArgument {
            name,
            argument_type,
            ..
        } => {
            let data_types = if let Some(argument_type) = argument_type {
                if let Some(type_ref) = get_type_ref(argument_type) {
                    let type_ref_ref = Reference::type_ref(type_ref.clone());

                    let new_symbol = if type_ref_ref.is_builtin() {
                        Symbol {
                            references: Some(type_ref_ref.clone()),
                            kind: PhpSymbolKind::BuiltInType,
                            ..Symbol::from(type_ref.last().unwrap())
                        }
                    } else {
                        Symbol {
                            references: Some(type_ref_ref.clone()),
                            ..Symbol::from(type_ref.last().unwrap())
                        }
                    };

                    // Register node for the type_ref as well
                    let child = arena.new_node(new_symbol);
                    enclosing.append(child, arena);

                    vec![type_ref_ref]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };

            let child = arena.new_node(Symbol {
                data_types,
                parent,
                ..Symbol::from(name)
            });

            enclosing.append(child, arena);

            Ok(child)
        }
        Node::Class { token, extends, .. } => {
            let inherits_from = if let Some(extends) = extends {
                extends
                    .iter()
                    .map(|parent| get_type_ref(parent))
                    .filter(|mapped| mapped.is_some())
                    .map(|filtered| Reference::type_ref(filtered.unwrap()))
                    .collect()
            } else {
                Vec::new()
            };

            let child = arena.new_node(Symbol {
                name: String::from("Anonymous class"),
                kind: PhpSymbolKind::Class,
                range: get_range(node.range()),
                selection_range: get_range(token.range()),
                detail: None,
                deprecated: None,
                inherits_from,
                parent,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        Node::NamespaceBlock {
            token, type_ref, ..
        } => {
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

            let child = arena.new_node(Symbol {
                name,
                kind: PhpSymbolKind::Namespace,
                range: get_range(node.range()),
                selection_range: get_range(token.range()),
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });
            enclosing.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        Node::ClassStatement { name, .. } | Node::TraitStatement { name, .. } => {
            let selection_range = get_range(name.range());
            let name = name.clone().label.unwrap();
            let range = get_range(node.range());

            let child = arena.new_node(Symbol {
                name,
                kind: PhpSymbolKind::Class,
                range,
                selection_range,
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });
            enclosing.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        Node::Interface { name, extends, .. } => {
            let selection_range = get_range(name.range());
            let name = name.clone().label.unwrap();
            let range = get_range(node.range());
            let inherits_from = if let Some(extends) = extends {
                extends
                    .iter()
                    .map(|parent| get_type_ref(parent))
                    .filter(|mapped| mapped.is_some())
                    .map(|filtered| Reference::type_ref(filtered.unwrap()))
                    .collect()
            } else {
                Vec::new()
            };

            let child = arena.new_node(Symbol {
                name,
                kind: PhpSymbolKind::Interface,
                range,
                selection_range,
                detail: None,
                deprecated: None,
                inherits_from,
                parent,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });
            enclosing.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        Node::ClassConstantDefinitionStatement { name, .. } => {
            let range = get_range(node.range());
            let child = arena.new_node(Symbol {
                name: name.clone().label.unwrap(),
                kind: PhpSymbolKind::Constant,
                range,
                selection_range: range,
                detail: None,
                deprecated: None,

                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });
            enclosing.append(child, arena);

            Ok(child)
        }
        Node::PropertyDefinitionStatement {
            name, data_type, ..
        } => {
            let range = get_range(node.range());

            let data_types = if let Some(data_type) = data_type {
                if let Some(type_ref) = get_type_ref(data_type) {
                    vec![Reference::type_ref(type_ref)]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };
            let child = arena.new_node(Symbol {
                name: name.clone().label.unwrap(),
                kind: PhpSymbolKind::Property,
                range,
                selection_range: range,
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types,
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);
            Ok(child)
        }
        Node::MethodDefinitionStatement {
            name,
            function,
            is_static,
            ..
        } => {
            let return_type =
                if let Node::FunctionDefinitionStatement { return_type, .. } = function.as_ref() {
                    return_type
                } else {
                    return Err("Invalid function".to_owned());
                };

            let data_types = if let Some(data_type) = return_type {
                if let Some(type_ref) = get_type_ref(&*data_type) {
                    vec![Reference::type_ref(type_ref)]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };

            let child = arena.new_node(Symbol {
                name: name.clone().label.unwrap(),
                kind: PhpSymbolKind::Method,
                range: get_range(node.range()),
                selection_range: get_range(name.range()),
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types,
                is_static: is_static.is_some(),
                imports: None,
            });

            enclosing.append(child, arena);

            for c in function.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        Node::FunctionDefinitionStatement { return_type, .. } => {
            let range = get_range(node.range());
            let name = "Anonymous function def".to_owned();
            let data_types = if let Some(data_type) = return_type {
                if let Some(type_ref) = get_type_ref(&*data_type) {
                    vec![Reference::type_ref(type_ref)]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };

            let child = arena.new_node(Symbol {
                name,
                kind: PhpSymbolKind::Function,
                range,
                selection_range: range,
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types,
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        Node::NamedFunctionDefinitionStatement { name, function, .. } => {
            let return_type =
                if let Node::FunctionDefinitionStatement { return_type, .. } = function.as_ref() {
                    return_type
                } else {
                    return Err(format!("Invalid function {:?}", function));
                };

            let data_types = if let Some(data_type) = return_type {
                if let Some(type_ref) = get_type_ref(&*data_type) {
                    vec![Reference::type_ref(type_ref)]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            };

            let child = arena.new_node(Symbol {
                name: name.clone().label.unwrap(),
                kind: PhpSymbolKind::Function,
                range: get_range(node.range()),
                selection_range: get_range(name.range()),
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types,
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);

            for c in function.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        Node::Variable(token) => {
            let child = arena.new_node(Symbol::from(token));

            enclosing.append(child, arena);

            Ok(child)
        }
        Node::Array { .. } | Node::OldArray { .. } => {
            let child = arena.new_node(Symbol {
                name: "".to_string(),
                kind: PhpSymbolKind::Array,
                range: get_range(node.range()),
                selection_range: get_range(node.range()),
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });

            enclosing.append(child, arena);

            for c in node.children() {
                collect_symbols(arena, &child, c)?;
            }

            Ok(child)
        }
        _ => Err(format!("Unexpected {:?}", node)),
    }
}
