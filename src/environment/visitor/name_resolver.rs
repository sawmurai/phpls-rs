use super::{super::PhpSymbolKind, Symbol};
use super::{workspace_symbol::get_type_ref, Visitor};
use super::{workspace_symbol::get_type_refs, NextAction};
use crate::environment::symbol::Visibility;
use crate::environment::{scope::Reference as SymbolReference, Notification};
use crate::parser::node::{Node as AstNode, NodeRange, TypeRef};
use crate::parser::token::{Token, TokenType};
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use tower_lsp::lsp_types::DiagnosticSeverity;

#[derive(Debug, Clone)]
pub struct Reference {
    /// Location of the reference in the current document
    pub range: NodeRange,

    /// NodeId the reference if pointing to
    pub node: NodeId,
}

impl Reference {
    pub fn new(range: NodeRange, node: NodeId) -> Self {
        Reference { range, node }
    }
}

type UniqueNodeRange = (NodeId, NodeRange);

pub struct NameResolver<'a> {
    global_scope: &'a HashMap<String, NodeId>,

    /// Contains locally defined variables and functions
    local_scopes: Vec<HashMap<String, NodeId>>,

    /// Contains a stack of references to $this
    current_class: Option<NodeId>,

    /// Current scope container for storage of new symbols
    /// Usually a method / function body or a file
    scope_container: NodeId,

    /// Collect document references for multiple files identified by their node id
    document_references: HashMap<NodeId, Vec<Reference>>,

    diagnostics: Vec<Notification>,

    cache: HashMap<UniqueNodeRange, Option<NodeId>>,
}

impl<'a> NameResolver<'a> {
    pub fn new(global_scope: &'a HashMap<String, NodeId>, scope_container: NodeId) -> Self {
        NameResolver {
            global_scope,
            local_scopes: vec![HashMap::new()],
            current_class: None,
            document_references: HashMap::new(),
            scope_container,
            diagnostics: Vec::new(),
            cache: HashMap::new(),
        }
    }

    /// Return the collected references
    pub fn references(&self) -> HashMap<NodeId, Vec<Reference>> {
        self.document_references.clone()
    }

    /// Push a diagnostic message to the queue
    pub fn diagnostic(
        &mut self,
        file: String,
        range: NodeRange,
        message: String,
        severity: DiagnosticSeverity,
    ) {
        self.diagnostics.push(Notification {
            file,
            range,
            message,
            severity,
        });
    }

    /// Enter a new scope
    pub fn push_scope(&mut self) {
        self.local_scopes.push(HashMap::new());
    }

    /// Enter a new scope and prefill it with the current scope
    pub fn clone_scope(&mut self) {
        let new = if let Some(last) = self.local_scopes.last() {
            last.clone()
        } else {
            HashMap::new()
        };

        self.local_scopes.push(new);
    }

    /// Leave the current scope and discard it
    pub fn pop_scope(&mut self) {
        self.local_scopes.pop();
    }

    /// Enter a new class
    pub fn enter_class(&mut self, node: NodeId) {
        self.current_class = Some(node);
        self.scope_container = node;
    }

    /// Enter a new class
    pub fn leave_class(&mut self) {
        self.current_class = None;
    }

    /// Return a local symbol by its name. The name is stored in the token, so its sufficient
    /// to just pass the token
    pub fn get_local(&self, token: &Token) -> Option<NodeId> {
        // Resolve $this to current class
        if let Some(label) = token.label.as_ref() {
            if token.t == TokenType::Variable {
                if label == "this" {
                    if let Some(current_class) = self.current_class {
                        return Some(current_class);
                    }
                }
            }

            if let Some(top_scope) = self.local_scopes.last() {
                if let Some(node) = top_scope.get(label) {
                    return Some(*node);
                }
            }
        }

        None
    }

    /// Register a new reference to a local symbol
    pub fn reference_local(&mut self, file: NodeId, token: &Token, node: &NodeId) {
        self.reference(file, Reference::new(token.range(), *node))
    }

    /// Register a new reference to a symbol
    pub fn reference(&mut self, file: NodeId, reference: Reference) {
        self.document_references
            .entry(file)
            .or_insert_with(Vec::new)
            .push(reference)
    }

    /// Declare a local symbol, usually a variable or a function if it has not
    /// been declared before
    pub fn declare_local_if_new(&mut self, file: NodeId, token: &Token, t: NodeId) {
        if let Some(top_scope) = self.local_scopes.last_mut() {
            if let Some(name) = token.label.as_ref() {
                if let Some(node) = top_scope.get(name) {
                    self.document_references
                        .entry(file)
                        .or_insert_with(Vec::new)
                        .push(Reference::new(token.range(), *node));
                } else {
                    top_scope.insert(name.clone(), t);
                    self.reference_local(file, token, &t);
                }
            }
        }
    }

    /// Declare a local symbol and overwrite an existing declaration
    /// The use case for this is variable shadowing and defining arguments of an
    /// arrow function in another scope that already defined a variable with the same
    /// name.
    pub fn declare_or_overwrite_local(
        &mut self,
        file: NodeId,
        token: &Token,
        t: NodeId,
        add_ref: bool,
    ) {
        if let Some(top_scope) = self.local_scopes.last_mut() {
            if let Some(label) = token.label.as_ref() {
                top_scope.insert(label.clone(), t);
            }
        }

        if add_ref {
            self.reference_local(file, token, &t);
        }
    }

    /// Resolve fully qualified
    pub fn resolve_fully_qualified(&mut self, name: &str) -> Option<NodeId> {
        if let Some(node) = self.global_scope.get(&name.to_lowercase()) {
            Some(*node)
        } else {
            None
        }
    }

    /// Resolve a TypeRef `Some\Name\Space` to the node of the definition of that symbol
    pub fn resolve_type_ref(
        &mut self,
        type_ref: &TypeRef,
        arena: &Arena<Symbol>,
        context_anchor: &NodeId,
        register_ref: bool,
    ) -> Option<NodeId> {
        if type_ref.is_builtin() || type_ref.is_empty() {
            return None;
        }

        let fully_qualified = type_ref.is_fully_qualified();

        let file = context_anchor.ancestors(arena).last().unwrap();
        let file_symbol = arena[file].get();

        // Maybe do the following only if the name starts with a backslash?

        let name = type_ref.root().unwrap();
        let range = type_ref.range();

        let cache_key = (file, range);
        if let Some(cached) = self.cache.get(&cache_key) {
            return *cached;
        }

        if name == "self" || name == "static" {
            if self.current_class.is_some() {
                self.cache.insert(cache_key, self.current_class);

                return self.current_class;
            }

            self.diagnostics.push(Notification::error(
                file_symbol.name().to_owned(),
                String::from("Can only be used inside a class"),
                range,
            ));

            self.cache.insert(cache_key, None);
            return None;
        }

        if name == "parent" {
            if let Some(current_class) = self.current_class {
                let parent_class =
                    arena[current_class]
                        .get()
                        .get_unique_parent(current_class, self, arena);

                self.cache.insert(cache_key, parent_class);
                return parent_class;
            }
            self.diagnostics.push(Notification::error(
                file_symbol.name().to_owned(),
                String::from("Parent can only be used inside a class"),
                range,
            ));

            self.cache.insert(cache_key, None);
            return None;
        }

        let mut current_available_imports = HashMap::new();
        if let Some(imports) = file_symbol.imports.as_ref() {
            for import in imports.all() {
                current_available_imports.insert(import.name().to_lowercase(), import.full_name());
            }
        }

        // First attempt: Try to find the parent namespace block
        let current_namespace = if let Some(ns) = context_anchor.ancestors(arena).find_map(|a| {
            let s = arena[a].get();

            if s.kind == PhpSymbolKind::Namespace {
                Some(s.name())
            } else {
                None
            }
        }) {
            ns
        // Alternative: Find the global namespace of the file
        } else if let Some(ns) = file.children(arena).find_map(|c| {
            let s = arena[c].get();
            if s.kind == PhpSymbolKind::Namespace {
                Some(s.name())
            } else {
                None
            }
        }) {
            ns
        // Well, must be the global namespace then
        } else {
            ""
        };

        // Simple, if fully qualified do not tamper with the name
        let joined_name = if fully_qualified {
            type_ref.to_fqdn()[1..].to_string()
        } else if let Some(import) = current_available_imports.get(&name.to_lowercase()) {
            // If not fully qualified, check imports
            if type_ref.len() > 1 {
                let end = type_ref
                    .stem()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join("");

                format!("{}{}", import, end)
            } else {
                import.to_owned()
            }
        } else if !current_namespace.is_empty() {
            // Next try the name in the current namespace
            format!("{}\\{}", current_namespace, name)
        } else {
            // Otherwise use a fqdn but cut off the leading backslash
            type_ref.to_fqdn()
        };

        // Ensure case-insensitivity
        let normalized_joined_name = joined_name.to_lowercase();

        let (node, comp_name) = if let Some(node) = self.global_scope.get(&normalized_joined_name) {
            (Some(*node), joined_name)
        } else if let Some(node) = self.global_scope.get(&name.to_lowercase()) {
            (Some(*node), name)
        } else {
            self.diagnostics.push(Notification::error(
                file_symbol.name().to_owned(),
                format!("Unresolvable type ({}) '{}'", fully_qualified, joined_name),
                range,
            ));

            (None, String::from(""))
        };

        if let Some(node) = node {
            if register_ref {
                self.reference(file, Reference::new(range, node));
            }

            if !arena[node].get().fqdn_matches(&comp_name) {
                self.diagnostics.push(Notification::warning(
                    file_symbol.name().to_owned(),
                    format!(
                        "Case mismatch between call and definition {} vs {}",
                        &comp_name,
                        arena[node].get().fqdn(),
                    ),
                    range,
                ));
            }
        }

        self.cache.insert(cache_key, node);

        node
    }
}

pub struct NameResolveVisitor<'a, 'b: 'a> {
    resolver: &'b mut NameResolver<'a>,
    file: NodeId,
}

impl<'a, 'b: 'a> NameResolveVisitor<'a, 'b> {
    pub fn new(resolver: &'b mut NameResolver<'a>, file: NodeId) -> Self {
        NameResolveVisitor { resolver, file }
    }

    pub fn references(&self) -> HashMap<NodeId, Vec<Reference>> {
        self.resolver.references()
    }

    pub fn diagnostics(&self) -> std::slice::Iter<'_, Notification> {
        // TODO: Rather return an iterator?
        self.resolver.diagnostics.iter()
    }
}

/// The NameResolveVisitor walks the AST and creates a list of references
/// Furthermore it adds variables to the arena underneath the files entry
///
/// Whenever the visitor enters a new scope that allows the definition of local symbols, like variables
/// or functions, a container in the arena is put into the resolver, so that
/// the variables can be stored somewhere
/// Whenever a class is entered the variable $this and the symbols self, parent etc. are updated with
/// the correct symbol
impl<'a, 'b: 'a> Visitor for NameResolveVisitor<'a, 'b> {
    /// Decides if a symbol is worth collecting
    fn visit(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId) -> NextAction {
        match node {
            AstNode::UseStatement { .. } => NextAction::ProcessChildren(parent),
            AstNode::UseDeclaration { declaration, .. } => {
                let name = declaration.name();

                if let Some(resolved) = self.resolver.resolve_fully_qualified(&name) {
                    if !arena[resolved].get().fqdn_matches(&name) {
                        self.resolver.diagnostic(
                            arena[self.file].get().name().to_owned(),
                            declaration.range(),
                            String::from("Case mismatch between call and definition"),
                            DiagnosticSeverity::Warning,
                        );
                    }

                    self.resolver
                        .reference(self.file, Reference::new(node.range(), resolved));
                }

                NextAction::Abort
            }
            AstNode::TypeRef(type_ref) => {
                self.resolver
                    .resolve_type_ref(type_ref, arena, &parent, true);

                NextAction::Abort
            }
            AstNode::ClassStatement { name, .. }
            | AstNode::TraitStatement { name, .. }
            | AstNode::Interface { name, .. } => {
                // Register $this in the current scope
                if let Some(current_class) =
                    self.resolver
                        .resolve_type_ref(&vec![name.clone()].into(), arena, &parent, true)
                {
                    self.resolver.enter_class(current_class);

                    NextAction::ProcessChildren(current_class)
                } else {
                    NextAction::Abort
                }
            }
            AstNode::MethodDefinitionStatement { doc_comment, .. } => {
                // Is there a doc comment?
                if let Some(doc_comment) = doc_comment {
                    if let AstNode::DocComment { return_type, .. } = doc_comment.as_ref() {
                        return_type
                            .iter()
                            .filter_map(get_type_ref)
                            .for_each(|type_ref| {
                                self.resolver
                                    .resolve_type_ref(&type_ref, arena, &parent, true);
                            });
                    }
                }

                // Hand over this method's scope as the parent scope
                // of the variables within the methods body, including
                // the arguments
                NextAction::ProcessChildren(self.resolver.scope_container)
            }
            AstNode::ArrowFunction { .. } | AstNode::NamedFunctionDefinitionStatement { .. } => {
                NextAction::ProcessChildren(self.resolver.scope_container)
            }

            AstNode::Variable(token) => {
                if let Some(node) = self.resolver.get_local(token) {
                    self.resolver.reference_local(self.file, token, &node);
                } else {
                    let child = arena.new_node(Symbol::from(token));

                    self.resolver.scope_container.append(child, arena);
                    self.resolver.declare_local_if_new(self.file, token, child);
                }

                NextAction::Abort
            }
            AstNode::DocCommentVar { name, types, .. } => {
                let mut data_types = Vec::new();
                if let Some(types) = types {
                    for type_ref in types {
                        if let Some(node) = self
                            .resolver
                            .resolve_type_ref(&type_ref, arena, &parent, true)
                        {
                            data_types.push(SymbolReference::node(type_ref.clone(), node));
                        } else {
                            data_types.push(SymbolReference::type_ref(type_ref.clone()));
                        }
                    }
                }

                let child = if let Some(existing) = self.resolver.get_local(name) {
                    arena[existing].get_mut().data_types.extend(data_types);

                    existing
                } else {
                    arena.new_node(Symbol {
                        data_types,
                        ..Symbol::from(name)
                    })
                };

                self.resolver.scope_container.append(child, arena);
                self.resolver.declare_local_if_new(self.file, name, child);

                NextAction::Abort
            }
            AstNode::CatchBlock { var, types, .. } => {
                let mut data_types = Vec::with_capacity(types.len());

                for data_type in types {
                    if let Some(type_ref) = get_type_ref(data_type) {
                        let type_ref_ref = SymbolReference::type_ref(type_ref.clone());

                        data_types.push(type_ref_ref);
                    }
                }

                let child = arena.new_node(Symbol {
                    data_types,
                    ..Symbol::from(var)
                });

                self.resolver.scope_container.append(child, arena);
                self.resolver
                    .declare_or_overwrite_local(self.file, var, child, false);

                NextAction::ProcessChildren(parent)
            }
            AstNode::ForEachStatement { collection, kv, .. } => {
                let collection = if let AstNode::Variable(collection) = collection.as_ref() {
                    if let Some(collection) = self.resolver.get_local(collection) {
                        arena[collection].get()
                    } else {
                        // Not resolvable .. too bad
                        return NextAction::ProcessChildren(parent);
                    }
                } else {
                    // Not a variable, so no easy way (yet) of determing the type. Later on we will have a
                    // way of getting the return types of expressions
                    return NextAction::ProcessChildren(parent);
                };

                if let AstNode::ArrayElement { value, .. } = kv.as_ref() {
                    if let AstNode::Variable(item) = value.as_ref() {
                        let data_types = collection
                            .data_types
                            .iter()
                            .filter_map(|reference| {
                                if reference.type_ref.is_multiple() {
                                    Some(SymbolReference::node(
                                        reference.type_ref.to_collection_item(),
                                        reference.node.unwrap(),
                                    ))
                                } else {
                                    None
                                }
                            })
                            .collect();

                        let mut s = Symbol::from(item);
                        s.data_types = data_types;
                        let child = arena.new_node(s);

                        self.resolver.scope_container.append(child, arena);
                        self.resolver
                            .declare_or_overwrite_local(self.file, item, child, false);
                    }
                }

                NextAction::ProcessChildren(parent)
            }
            AstNode::FunctionArgument {
                name, doc_comment, ..
            } => {
                // Get the symbol of this argument. We can safely assume that unwrap won't fail
                // because the argument will also have been visited by the WorkspaceSymbolVisitor
                let current_argument =
                    self.resolver
                        .scope_container
                        .children(arena)
                        .find_map(|child| {
                            let argument = arena[child].get();
                            if let Some(name) = name.label.as_ref() {
                                if argument.name().eq(name) {
                                    return Some(child);
                                }
                            }

                            None
                        });

                if let Some(current_argument) = current_argument {
                    let current_argument_symbol = arena[current_argument].get();
                    let resolved_references = current_argument_symbol
                        .data_types
                        .iter()
                        .map(|tr| {
                            (
                                tr,
                                self.resolver
                                    .resolve_type_ref(&tr.type_ref, arena, &parent, true),
                            )
                        })
                        .map(|(tr, node)| {
                            if let Some(node) = node {
                                SymbolReference::node(tr.type_ref.to_owned(), node)
                            } else {
                                SymbolReference::type_ref(tr.type_ref.to_owned())
                            }
                        });

                    arena[current_argument].get_mut().data_types = resolved_references.collect();

                    self.resolver.declare_or_overwrite_local(
                        self.file,
                        name,
                        current_argument,
                        true,
                    );

                    if let Some(doc_comment) = doc_comment {
                        if let AstNode::DocCommentParam { name, .. } = doc_comment.as_ref() {
                            self.resolver
                                .reference_local(self.file, name, &current_argument);
                        }
                    }
                }

                NextAction::Abort
            }
            AstNode::ReturnType { .. } => {
                get_type_refs(node).iter().for_each(|tr| {
                    self.resolver.resolve_type_ref(&tr, arena, &parent, true);
                });

                NextAction::Abort
            }
            AstNode::Binary { left, right, token } => {
                if token.t == TokenType::Assignment {
                    let data_type = self.resolve_member_type(&right, arena);
                    if let AstNode::Variable(token) = left.as_ref() {
                        let child = if let Some(data_type) = data_type {
                            arena.new_node(Symbol {
                                data_types: vec![SymbolReference::node(
                                    vec![token.clone()].into(),
                                    data_type,
                                )],
                                ..Symbol::from(token)
                            })
                        } else {
                            arena.new_node(Symbol::from(token))
                        };

                        self.resolver.scope_container.append(child, arena);
                        self.resolver.declare_local_if_new(self.file, token, child);
                    } else {
                        self.resolve_member_type(&left, arena);
                    }
                    NextAction::Abort
                } else {
                    NextAction::ProcessChildren(parent)
                }
            }
            AstNode::StaticMember { .. } | AstNode::Member { .. } => {
                self.resolve_member_type(node, arena);

                // This might result in children which are Members / StaticMembers being processed
                // more than once. But without processing the children there is (currently) no way to get
                // to the parameters of calls etc.
                NextAction::Abort

                //NextAction::ProcessChildren(parent)
            }
            AstNode::NamespaceStatement { .. } => NextAction::Abort,
            _ => NextAction::ProcessChildren(parent),
        }
    }

    fn before(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, _parent: NodeId) {
        match node {
            AstNode::NamedFunctionDefinitionStatement { name, .. }
            | AstNode::MethodDefinitionStatement { name, .. } => {
                // Push scope for method arguments and body
                self.resolver.push_scope();

                let method_name = name.to_string();

                // Find this method in the current scope
                for method in self.resolver.scope_container.children(arena) {
                    if arena[method].get().name() == method_name {
                        self.resolver.scope_container = method;
                        self.resolver
                            .reference(self.file, Reference::new(name.range(), method));

                        return;
                    }
                }
            }
            AstNode::Property { name, .. } | AstNode::Const { name, .. } => {
                if let Some(label) = name.label.as_ref() {
                    for container in self.resolver.scope_container.children(arena) {
                        if arena[container].get().name() == label {
                            self.resolver
                                .reference(self.file, Reference::new(name.range(), container));

                            return;
                        }
                    }
                }
            }
            AstNode::ArrowFunction { token, .. } => {
                self.resolver.clone_scope();
                let method_name = format!("af-{:?}", token.range());

                // Find this method in the current scope
                for method in self.resolver.scope_container.children(arena) {
                    if arena[method].get().name() == method_name {
                        self.resolver.scope_container = method;

                        return;
                    }
                }
            }

            _ => (),
        }
    }

    fn after(&mut self, node: &AstNode, _arena: &mut Arena<Symbol>, parent: NodeId) {
        match node {
            AstNode::ClassStatement { .. }
            | AstNode::TraitStatement { .. }
            | AstNode::Interface { .. } => {
                self.resolver.leave_class();
                self.resolver.scope_container = parent;
            }
            AstNode::MethodDefinitionStatement { .. }
            | AstNode::NamedFunctionDefinitionStatement { .. }
            | AstNode::ArrowFunction { .. } => {
                self.resolver.pop_scope();
                self.resolver.scope_container = parent;
            }
            _ => (),
        }
    }
}

impl<'a, 'b: 'a> NameResolveVisitor<'a, 'b> {
    /// Resolve a member chain like
    /// ```php
    /// $object->method()->member;
    /// ```
    /// and return the type of the last link, in this case of member
    fn resolve_member_type(&mut self, node: &AstNode, arena: &mut Arena<Symbol>) -> Option<NodeId> {
        let mut reversed_chain = Vec::with_capacity(5);

        let mut current_object = node;
        let (root_node, minimal_visibility) = 'root_node: loop {
            match current_object {
                AstNode::Binary { left, right, token } => {
                    if token.t == TokenType::Assignment {
                        if let AstNode::AliasedVariable { .. } = left.as_ref() {
                            return None;
                        }

                        if let AstNode::Variable(token) = left.as_ref() {
                            let data_type = self.resolve_member_type(&right, arena);

                            let child = if let Some(data_type) = data_type {
                                arena.new_node(Symbol {
                                    data_types: vec![SymbolReference::node(
                                        vec![token.clone()].into(),
                                        data_type,
                                    )],
                                    ..Symbol::from(token)
                                })
                            } else {
                                arena.new_node(Symbol::from(token))
                            };

                            self.resolver.scope_container.append(child, arena);
                            self.resolver.declare_local_if_new(self.file, token, child);

                            current_object = left;
                        } else {
                            return None;
                        }
                    } else {
                        self.resolve_member_type(left, arena);

                        return self.resolve_member_type(right, arena);
                    }
                }
                AstNode::Ternary {
                    check,
                    true_arm,
                    false_arm,
                    ..
                } => {
                    self.resolve_member_type(check, arena);
                    if let Some(true_arm) = true_arm {
                        self.resolve_member_type(true_arm, arena);
                    }

                    self.resolve_member_type(false_arm, arena);

                    return None;
                }
                AstNode::Unary { expr, .. }
                | AstNode::Clone { object: expr, .. }
                | AstNode::New { class: expr, .. } => {
                    current_object = expr;
                }
                AstNode::Grouping(inside) => {
                    current_object = inside;
                }
                AstNode::Call {
                    callee, parameters, ..
                } => {
                    parameters.iter().for_each(|param| {
                        self.resolve_member_type(param, arena);
                    });

                    current_object = callee;
                }
                AstNode::Array { elements, .. } => {
                    elements.iter().for_each(|param| {
                        self.resolve_member_type(param, arena);
                    });

                    return None;
                }
                AstNode::ArrayElement { value, .. } => {
                    self.resolve_member_type(value.as_ref(), arena);

                    return None;
                }
                AstNode::StaticMember { object, member, .. }
                | AstNode::Member { object, member, .. } => {
                    // TODO: Add info if called statically or dynamically to raise error later on
                    reversed_chain.push(member);
                    current_object = object;
                }
                AstNode::Variable(token) => {
                    if let Some(node) = self.resolver.get_local(token) {
                        self.resolver.reference_local(self.file, &token, &node);

                        if let Some(name) = token.label.as_ref() {
                            // TODO: Once tested, reduce this if as the test is already done in resolver::get_local
                            if name == "this" {
                                // In this case this was automatically resolved to the current class
                                break (Some(node), Visibility::Private);
                            } else {
                                // In this case we need to resolve to the type of the variable
                                for reference in &arena[node].get().data_types {
                                    if let Some(referenced_node) = reference.node {
                                        break 'root_node (
                                            // Return the referenced node
                                            Some(referenced_node),
                                            // Now, the required visibility depends on if we are in the same class or not
                                            if let Some(current_class) = self.resolver.current_class
                                            {
                                                if current_class == referenced_node {
                                                    Visibility::Private
                                                } else {
                                                    Visibility::Public
                                                }
                                            } else {
                                                Visibility::Public
                                            },
                                        );
                                    } else {
                                        if let Some(resolved_data_type) =
                                            self.resolver.resolve_type_ref(
                                                &reference.type_ref,
                                                arena,
                                                &node,
                                                true,
                                            )
                                        {
                                            break 'root_node (
                                                Some(resolved_data_type),
                                                Visibility::Public,
                                            );
                                        }
                                    }
                                }

                                break (Some(node), Visibility::Public);
                            }
                        } else {
                            break (Some(node), Visibility::Public);
                        };
                    }

                    return None;
                }
                AstNode::Literal(token) => match token.t {
                    TokenType::TypeSelf | TokenType::Static => {
                        break (self.resolver.current_class, Visibility::Private);
                    }
                    TokenType::Parent => {
                        if let Some(current_class) = self.resolver.current_class {
                            break (
                                arena[current_class].get().get_unique_parent(
                                    current_class,
                                    self.resolver,
                                    arena,
                                ),
                                Visibility::Protected,
                            );
                        } else {
                            return None;
                        }
                    }
                    TokenType::ConstantEncapsedString | TokenType::EncapsedAndWhitespaceString => {
                        return None;
                    }
                    _ => {
                        let sc = self.resolver.scope_container;

                        break (
                            self.resolver.resolve_type_ref(
                                &vec![token.clone()].into(),
                                arena,
                                &sc,
                                true,
                            ),
                            Visibility::Public,
                        );
                    }
                },
                AstNode::TypeRef(tokens) => {
                    let scope_container = self.resolver.scope_container;

                    if let Some(node) =
                        self.resolver
                            .resolve_type_ref(tokens, arena, &scope_container, true)
                    {
                        break (Some(node), Visibility::Public);
                    } else {
                        return None;
                    }
                }
                AstNode::Field { array, .. } => {
                    if let AstNode::Variable(collection) = array.as_ref() {
                        if let Some(var) = self.resolver.get_local(collection) {
                            self.resolver.reference_local(self.file, &collection, &var);

                            for reference in &arena[var].get().data_types {
                                if reference.type_ref.is_multiple() {
                                    if let Some(referenced_node) = reference.node {
                                        break 'root_node (
                                            Some(referenced_node),
                                            Visibility::Public,
                                        );
                                    }
                                }
                            }
                        }
                    }
                    return None;
                }
                _ => {
                    node.children().iter().for_each(|c| {
                        self.resolve_member_type(c, arena);
                    });

                    return None;
                }
            }
        };

        // Store the name of the currently viewed file to be able to display diagnostics
        let file_name = arena[self
            .resolver
            .scope_container
            .ancestors(arena)
            .last()
            .unwrap()]
        .get()
        .name();

        if let Some(mut root_node) = root_node {
            // $this (root_node) will have resolved to the definition of the class
            // of the object
            'link_loop: for link in reversed_chain.iter().rev() {
                // Get the definition of the current parent and try to find "link" in it
                // Loop backwards through the inheritance chain, from the object towards its ancestors

                let link_name = if let AstNode::Literal(token) = link.as_ref() {
                    if token.to_string() == "class" {
                        return None;
                    }

                    link.name()
                } else if let AstNode::Variable(token) = link.as_ref() {
                    // TODO: Make sure the variable is called statically like Test::$myVar
                    token.label.as_ref().unwrap().clone()
                } else {
                    link.name()
                };

                if let Some(child) = arena[root_node]
                    .get()
                    .get_all_symbols(root_node, self.resolver, arena)
                    .get(&link_name.to_lowercase())
                {
                    let child_symbol = arena[child.symbol].get();

                    if child.visibility >= minimal_visibility {
                        if child.alias != link_name {
                            self.resolver.diagnostic(
                                file_name.to_owned(),
                                link.range(),
                                String::from("Case mismatch between call and definition"),
                                DiagnosticSeverity::Warning,
                            );
                        }
                    } else {
                        self.resolver.diagnostic(
                            file_name.to_owned(),
                            link.range(),
                            String::from("Method is not accessible from this scope"),
                            DiagnosticSeverity::Error,
                        );

                        return None;
                    }

                    match link.as_ref() {
                        AstNode::Variable(token) | AstNode::Literal(token) => {
                            // Register reference here
                            self.resolver
                                .reference_local(self.file, &token, &child.symbol);
                        }
                        _ => (),
                    }

                    match child_symbol.kind {
                        PhpSymbolKind::Property | PhpSymbolKind::Method => {
                            for data_type in &child_symbol.data_types {
                                let first_type = data_type.type_ref.root_token_type();
                                if TokenType::TypeSelf == first_type
                                    || TokenType::Static == first_type
                                {
                                    continue 'link_loop;
                                }

                                if "$this" == data_type.type_ref.root().unwrap() {
                                    continue 'link_loop;
                                }

                                if TokenType::Parent == first_type {
                                    if let Some(root_parent) = arena[root_node]
                                        .get()
                                        .get_unique_parent(root_node, self.resolver, arena)
                                    {
                                        root_node = root_parent;

                                        continue 'link_loop;
                                    } else {
                                        self.resolver.diagnostic(
                                                file_name.to_owned(),
                                                link.range(),
                                                String::from(
                                                    "Method returns an instance of its parent, but its class has no parent or the parent could not be resolved."
                                                ),
                                                DiagnosticSeverity::Warning
                                            );

                                        return None;
                                    }
                                }

                                if let Some(resolved_type) = self.resolver.resolve_type_ref(
                                    &data_type.type_ref,
                                    arena,
                                    &root_node,
                                    true,
                                ) {
                                    root_node = resolved_type;

                                    // Got a match, stop and proceeed with the next link
                                    continue 'link_loop;
                                }
                            }

                            // No data type resolveable ... no way to proceed
                            return None;
                        }
                        _ => {
                            root_node = child.symbol;
                        }
                    }
                } else {
                    self.resolver.diagnostic(
                        file_name.to_owned(),
                        link.range(),
                        format!("Unresolvable symbol {}", link.name()),
                        DiagnosticSeverity::Error,
                    );
                    return None;
                }
            }

            // At this point we arrived at the last link and successfully resolved everything
            return Some(root_node);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{backend::Backend, backend::BackendState, environment::get_range, parser};
    use parser::{scanner::Scanner, Parser};

    macro_rules! references {
        ($col:ident, $file:expr) => {
            $col.symbol_references
                .get($file)
                .unwrap()
                .iter()
                .map(|(node, ranges)| {
                    let mut refs = Vec::with_capacity(ranges.len());
                    for _ in ranges {
                        refs.push(format!("{}", $col.arena[node.clone()].get().name()));
                    }

                    refs
                })
                .flatten()
                .collect::<Vec<String>>()
        };
    }

    macro_rules! assert_reference_names {
        ($expected:expr, $actual:expr) => {
            let mut left = $expected;
            let mut right = $actual;
            left.sort();
            right.sort();
            assert_eq!(left, right);
        };
    }

    #[tokio::test]
    async fn test_references_direct_and_inherited_symbols() {
        let mut state = BackendState::default();

        let sources = vec![
            (
                "living.php",
                "<?php namespace App; class Living { public function getPulse() { } }",
            ),
            (
                "animal.php",
                "<?php namespace App; class Animal extends Living { protected function getType() { } }",
            ),
            (
                "cat.php",
                "<?php namespace App; class Cat extends Animal { public const ROFL = 'copter'; public function getName() { } public function getThis(): self {} public function setName($name) { } } ",
            ),
            ("index.php", "<?php use App\\Cat; $kaetzchen = new Cat(); $kaetzchen->getName(); $kaetzchen->getPulse(); echo Cat::class;"),
            ("index2.php", "<?php use App\\Cat; $kaetzchen = ($test = new Cat()); $kaetzchen->getThis()->getThis()->getThis()->getPulse(); $test->getName();"),
            ("index3.php", "<?php use App\\Cat; $kaetzchen = new Cat(); $kaetzchen->setName(($marci = new Cat())->getName()); $marci->getName(); echo Cat::ROFL;"),
        ];

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        assert!(state.diagnostics.is_empty());

        assert_reference_names!(
            vec![
                "Cat",
                "Cat",
                "kaetzchen",
                "getName",
                "kaetzchen",
                "kaetzchen",
                "getPulse",
                "Cat",
            ],
            references!(state, "index.php")
        );

        assert_reference_names!(
            vec![
                "Cat",
                "Cat",
                "test",
                "kaetzchen",
                "kaetzchen",
                "getThis",
                "getThis",
                "getThis",
                "getPulse",
                "test",
                "test",
                "getName",
            ],
            references!(state, "index2.php")
        );

        assert_reference_names!(
            vec![
                "Cat",
                "Cat",
                "Cat",
                "marci",
                "getName",
                "kaetzchen",
                "kaetzchen",
                "setName",
                "marci",
                "marci",
                "getName",
                "Cat",
                "ROFL"
            ],
            references!(state, "index3.php")
        );
    }

    #[tokio::test]
    async fn test_references_function_parameters() {
        let mut state = BackendState::default();

        let sources = vec![
            (
                "living.php",
                "<?php namespace App; interface Living { public function getPulse(); }",
            ),
            (
                "index.php",
                "<?php use App\\Living; function animal_caller(Living $cat) { $cat->getPulse(); }",
            ),
        ];
        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());

        assert_reference_names!(
            vec![
                "Living",
                "Living",
                "animal_caller",
                "cat",
                "cat",
                "getPulse"
            ],
            references!(state, "index.php")
        );
    }

    #[tokio::test]
    async fn test_references_trait_members() {
        let mut state = BackendState::default();

        let sources = vec![
            (
                "living.php",
                "<?php namespace App; trait Living { private $name; public function getName() {} }",
            ),
            ("cat.php", "<?php namespace App; class Cat { use Living; }"),
            (
                "index.php",
                "<?php use App\\Cat; $marci = new Cat(); echo $marci->getName();",
            ),
        ];

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());

        assert_reference_names!(
            vec!["Cat", "Cat", "marci", "marci", "getName",],
            references!(state, "index.php")
        );
    }

    #[tokio::test]
    async fn test_resolves_members_of_references_to_own_class_correctly() {
        let mut state = BackendState::default();

        let sources = vec![
            (
                "living.php",
                "<?php namespace App; class Living { private $name; public function getName() { }
                protected function getOther() {}
                public function test() {
                    $instance = new Living();
                    $instance->getOther();
                    $instance->name;

                    $instance2 = new self();
                    $instance2->getOther();
                    $instance3 = new static();
                    $instance3->getOther();
                } }",
            ),
            ("cat.php", "<?php namespace App; class Cat extends Living { public function test() { $this->getName(); $this->getOther(); } }"),
        ];

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());
        assert_reference_names!(
            vec![
                "Living",
                "Living",
                "getName",
                "getOther",
                "getOther",
                "getOther",
                "getOther",
                "instance",
                "instance",
                "instance",
                "instance2",
                "instance2",
                "instance3",
                "instance3",
                "name",
                "name",
                "test"
            ],
            references!(state, "living.php")
        );
        assert_reference_names!(
            vec!["Cat", "Cat", "Cat", "Living", "getName", "getOther", "test"],
            references!(state, "cat.php")
        );
    }

    #[tokio::test]
    async fn test_resolves_across_namespaces() {
        let mut state = BackendState::default();

        let sources = vec![
            ("living.php", "<?php namespace App1; class Living { }"),
            (
                "cat.php",
                "<?php namespace App2; class Cat extends \\App1\\Living { }",
            ),
            (
                "tiger.php",
                "<?php namespace App2; use App1\\Living as L; class Tiger extends L { }",
            ),
            (
                "anothercat.php",
                "<?php namespace App2; use App1\\Living; class Cat extends Living { }",
            ),
        ];

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }
        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());

        assert_reference_names!(vec!["Cat", "Living"], references!(state, "cat.php"));
        assert_reference_names!(
            vec!["Living", "Living", "Tiger"],
            references!(state, "tiger.php")
        );
        assert_reference_names!(
            vec!["Cat", "Living", "Living"],
            references!(state, "anothercat.php")
        );
    }

    #[tokio::test]
    async fn test_resolves_parent_method() {
        let mut state = BackendState::default();

        let sources = vec![
            ("living.php", "<?php namespace App1; class Living { public function __construct() {} }"),
            (
                "cat.php",
                "<?php namespace App2; use App1\\Living;
                class Cat extends Living { public function __construct() { parent::__construct(); } }",
            ),
        ];

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());

        assert_reference_names!(
            vec!["Cat", "Living", "Living", "__construct", "__construct"],
            references!(state, "cat.php")
        );
    }

    #[tokio::test]
    async fn test_resolves_members_of_interfaces_with_multiple_parents() {
        let mut state = BackendState::default();

        let sources =
            vec![
            ("if1.php", "<?php namespace App1; interface If1 { public function m1(); }"),
            ("if2.php", "<?php namespace App1; interface If2 { public function m2(); }"),
            ("if3.php", "<?php namespace App1; interface If3 extends If1, If2 { }"),
            (
                "index.php",
                "<?php namespace App1; function x(If3 $object) { $object->m1(); $object->m2(); }",
            ),
        ];
        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());

        assert_reference_names!(
            vec!["If3", "m1", "m2", "object", "object", "object", "x"],
            references!(state, "index.php")
        );
    }

    #[tokio::test]
    async fn test_resolves_type_from_doc_comment() {
        let mut state = BackendState::default();

        let sources = vec![
            (
                "c1.php",
                "<?php namespace App1; class Test { /** @var OtherTest */ public string $inst; }",
            ),
            (
                "c2.php",
                "<?php namespace App1; class OtherTest { /** @param OtherTest $var */ public function test($var) {} }",
            ),
            (
                "index.php",
                "<?php namespace App1; $o = new Test(); $o->inst->test();",
            ),
        ];
        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());

        assert_reference_names!(
            vec!["Test", "o", "o", "inst", "test"],
            references!(state, "index.php")
        );

        assert_reference_names!(
            vec!["OtherTest", "OtherTest", "test", "var", "var"],
            references!(state, "c2.php")
        );
    }

    #[tokio::test]
    async fn test_resolves_chained_method_calls() {
        let mut state = BackendState::default();

        let sources = vec![
            ("lp.php", "<?php namespace App1; class P {}"),
            (
                "living.php",
                "<?php namespace App1; class Living extends P { public function me(): Living {}
            public function myself(): self {}
            public function i(): self {}
            public function my_parent(): parent {return new parent(); }}",
            ),
            (
                "index.php",
                "<?php namespace App2; use App1\\Living;
                $inst = new Living(); $inst->me()->myself()->i()->my_parent();",
            ),
        ];

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());
        assert_reference_names!(
            vec![
                "Living",
                "Living",
                "inst",
                "inst",
                "me",
                "myself",
                "i",
                "my_parent"
            ],
            references!(state, "index.php")
        );
    }

    #[tokio::test]
    async fn test_resolves_type_of_array_items_via_vardoc() {
        let mut state = BackendState::default();

        let sources = vec![
            (
                "index.php",
                "<?php
                /** @var P[] $list */
                $list = [];
                
                foreach ($list as $item) {
                    $item->findMe();
                }

                $list[2]->findMe();
                ",
            ),
            ("lp.php", "<?php class P { public function findMe() { }}"),
        ];

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
        }
        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());
        assert_reference_names!(
            vec!["P", "list", "list", "list", "item", "item", "findMe", "list", "findMe",],
            references!(state, "index.php")
        );
    }

    #[tokio::test]
    async fn test_resolves_type_of_interface_method_arguments() {
        let mut state = BackendState::default();

        let sources = vec![
            (
                "if1.php",
                "<?php
                interface TestInterface { public function rofl(Test2 $param): RetVal;}
                ",
            ),
            ("Test2.php", "<?php class Test2 { }"),
            ("RetVal.php", "<?php class RetVal { }"),
        ];

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
        }
        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }

        eprintln!("{:?}", state.diagnostics);
        assert!(state.diagnostics.is_empty());
        assert_reference_names!(
            vec!["TestInterface", "rofl", "Test2", "param", "RetVal"],
            references!(state, "if1.php")
        );
    }
}
