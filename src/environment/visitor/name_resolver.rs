use super::{super::PhpSymbolKind, Symbol};
use super::{workspace_symbol::get_type_ref, Visitor};
use super::{workspace_symbol::get_type_refs, NextAction};
use crate::environment::scope::Reference as SymbolReference;
use crate::environment::symbol::Visibility;
use crate::parser::node::{Node as AstNode, NodeRange};
use crate::parser::token::{Token, TokenType};
use indextree::{Arena, NodeId};
use std::collections::HashMap;

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

pub type Notification = (String, NodeRange, String);

pub struct NameResolver<'a> {
    global_scope: &'a HashMap<String, NodeId>,

    /// Contains locally defined variables and functions
    local_scopes: Vec<HashMap<String, NodeId>>,

    /// Contains a stack of references to $this
    current_class: Option<NodeId>,

    /// Current scope container for storage of new symbols
    /// Usually a method / function body or a file
    scope_container: NodeId,

    pub document_references: Vec<Reference>,

    diagnostics: Vec<Notification>,
}

impl<'a> NameResolver<'a> {
    pub fn new(global_scope: &'a HashMap<String, NodeId>, scope_container: NodeId) -> Self {
        NameResolver {
            global_scope,
            local_scopes: vec![HashMap::new()],
            current_class: None,
            document_references: Vec::new(),
            scope_container,
            diagnostics: Vec::new(),
        }
    }

    /// Return the collected references
    pub fn references(&self) -> Vec<Reference> {
        self.document_references.clone()
    }

    /// Return the collected diagnostics
    pub fn diagnostics(&self) -> Vec<Notification> {
        self.diagnostics.clone()
    }

    /// Push a diagnostic message to the queue
    pub fn diagnostic(&mut self, file: String, range: NodeRange, message: String) {
        self.diagnostics.push((file, range, message));
    }

    /// Enter a new scope
    pub fn push_scope(&mut self) {
        self.local_scopes.push(HashMap::new());
    }

    /// Leave the current scope and discard it
    pub fn pop_scope(&mut self) {
        self.local_scopes.pop();
    }

    /// Enter a new class
    pub fn enter_class(&mut self, node: NodeId) {
        self.current_class = Some(node);
    }

    /// Enter a new class
    pub fn leave_class(&mut self) {
        self.current_class = None;
    }

    /// Return true of the token is an identifier of a built in type
    fn is_builtin(&self, tokens: &[Token]) -> bool {
        if tokens.len() == 1 {
            match tokens[0].t {
                TokenType::TypeString
                | TokenType::TypeSelf
                | TokenType::Static
                | TokenType::Mixed
                | TokenType::TypeArray
                | TokenType::TypeBool
                | TokenType::TypeInt
                | TokenType::TypeFloat
                | TokenType::Null
                | TokenType::TypeObject
                | TokenType::ConstFile
                | TokenType::ConstDir
                | TokenType::ConstClass
                | TokenType::ConstFunction
                | TokenType::ConstMethod
                | TokenType::Callable
                | TokenType::ConstLine
                | TokenType::ConstTrait
                | TokenType::BinaryNumber
                | TokenType::DecimalNumber
                | TokenType::ExponentialNumber
                | TokenType::HexNumber
                | TokenType::LongNumber
                | TokenType::Generator
                | TokenType::Void => return true,
                _ => return false,
            }
        }

        false
    }

    /// Return a local symbol by its name. The name is stored in the token, so its sufficient
    /// to just pass the token
    pub fn get_local(&self, token: &Token) -> Option<NodeId> {
        // Resolve $this to current class
        if token.t == TokenType::Variable {
            if let Some(label) = token.label.as_ref() {
                if label == "this" {
                    if let Some(current_class) = self.current_class {
                        return Some(current_class);
                    }
                }
            }
        }

        if let Some(top_scope) = self.local_scopes.last() {
            if let Some(node) = top_scope.get(&token.clone().label.unwrap()) {
                return Some(*node);
            }
        }

        None
    }

    /// Register a new reference to a local symbol
    pub fn reference_local(&mut self, token: &Token, node: &NodeId) {
        self.document_references
            .push(Reference::new(token.range(), *node))
    }

    /// Declare a local symbol, usually a variable or a function
    pub fn declare_local(&mut self, token: &Token, t: NodeId) {
        if let Some(top_scope) = self.local_scopes.last_mut() {
            let name = token.clone().label.unwrap();

            if let Some(node) = top_scope.get(&name) {
                self.document_references
                    .push(Reference::new(token.range(), *node));
            } else {
                top_scope.insert(name, t);
            }
        }
    }

    /// Resolve fully qualified
    pub fn resolve_fully_qualified(&mut self, name: &str) -> Option<NodeId> {
        if let Some(node) = self.global_scope.get(name) {
            Some(*node)
        } else {
            None
        }
    }

    /// Resolve a TypeRef `Some\Name\Space` to the node of the definition of that symbol
    pub fn resolve_type_ref(
        &mut self,
        tokens: &[Token],
        arena: &Arena<Symbol>,
        context_anchor: &NodeId,
    ) -> Option<NodeId> {
        if self.is_builtin(tokens) {
            return None;
        }

        let fully_qualified =
            !tokens.is_empty() && tokens.first().unwrap().t == TokenType::NamespaceSeparator;

        let tokens = tokens
            .iter()
            .filter(|t| t.label.is_some())
            .cloned()
            .collect::<Vec<Token>>();

        if tokens.is_empty() {
            return None;
        }

        let mut file = *context_anchor;

        let mut current_namespace = String::new();
        let mut current_available_imports = HashMap::new();
        let mut file_symbol;

        loop {
            file_symbol = arena[file].get();

            if file_symbol.kind == PhpSymbolKind::File {
                if let Some(imports) = file_symbol.imports.as_ref() {
                    for import in imports {
                        current_available_imports.insert(import.name(), import.full_name());
                    }
                }
            } else if file_symbol.kind == PhpSymbolKind::Namespace {
                current_namespace = file_symbol.name.clone();
            }

            if let Some(parent) = arena[file].parent() {
                file = parent;
            } else {
                break;
            }
        }

        // Maybe do the following only if the name starts with a backslash?
        let name = tokens.first().unwrap().label.clone().unwrap();

        if name == "self" || name == "static" {
            if self.current_class.is_some() {
                return self.current_class;
            }

            self.diagnostics.push((
                file_symbol.name.clone(),
                (
                    tokens.first().unwrap().range().0,
                    tokens.last().unwrap().range().1,
                ),
                String::from("Can only be used inside a class"),
            ));
        }

        if name == "parent" {
            if let Some(current_class) = self.current_class {
                return arena[current_class]
                    .get()
                    .get_unique_parent(&current_class, self, arena);
            }

            self.diagnostics.push((
                file_symbol.name.clone(),
                (
                    tokens.first().unwrap().range().0,
                    tokens.last().unwrap().range().1,
                ),
                String::from("Parent can only be used inside a class"),
            ));
        }

        // Class was not inside a namespace block, so go and fetch the ns of the file
        if current_namespace.is_empty() {
            if let Some(ns) = file.children(arena).find_map(|c| {
                let s = arena[c].get();
                if s.kind == PhpSymbolKind::Namespace {
                    Some(s.name.clone())
                } else {
                    None
                }
            }) {
                current_namespace = ns;
            }
        }

        // Simple, if fully qualified do not tamper with the name
        let joined_name = if fully_qualified {
            tokens
                .iter()
                .map(|n| n.clone().label.unwrap())
                .collect::<Vec<String>>()
                .join("\\")
        } else if let Some(import) = current_available_imports.get(&name) {
            // If not fully qualified, check imports
            if tokens.len() > 1 {
                let end = tokens
                    .iter()
                    .skip(1)
                    .map(|n| n.clone().label.unwrap_or_else(|| "\\".to_owned()))
                    .collect::<Vec<String>>()
                    .join("");

                format!("{}\\{}", import, end)
            } else {
                import.to_owned()
            }
        } else if !current_namespace.is_empty() {
            // Next try the name in the current namespace
            format!("{}\\{}", current_namespace, name)
        } else {
            // Otherwise use a fqdn but cut off the leading backslash
            tokens
                .iter()
                .map(|n| n.clone().label.unwrap())
                .collect::<Vec<String>>()
                .join("\\")
        };

        if let Some(node) = self.global_scope.get(&joined_name) {
            let range = (
                tokens.first().unwrap().start(),
                tokens.last().unwrap().end(),
            );
            self.document_references.push(Reference::new(range, *node));

            return Some(*node);
        } else if let Some(node) = self.global_scope.get(&format!("\\{}", joined_name)) {
            let range = (
                tokens.first().unwrap().start(),
                tokens.last().unwrap().end(),
            );
            self.document_references.push(Reference::new(range, *node));

            return Some(*node);
        } else if let Some(global_symbol) = self.global_scope.get(&format!("\\{}", name)) {
            let range = (
                tokens.first().unwrap().start(),
                tokens.last().unwrap().end(),
            );

            self.document_references
                .push(Reference::new(range, *global_symbol));
            return Some(*global_symbol);
        }

        self.diagnostics.push((
            file_symbol.name.clone(),
            (
                tokens.first().unwrap().range().0,
                tokens.last().unwrap().range().1,
            ),
            format!("Unresolvable type ({}) '{}'", fully_qualified, joined_name),
        ));

        None
    }
}

pub struct NameResolveVisitor<'a, 'b: 'a> {
    resolver: &'b mut NameResolver<'a>,
}

impl<'a, 'b: 'a> NameResolveVisitor<'a, 'b> {
    pub fn new(resolver: &'b mut NameResolver<'a>) -> Self {
        NameResolveVisitor { resolver }
    }

    pub fn references(&self) -> Vec<Reference> {
        self.resolver.references()
    }

    pub fn diagnostics(&self) -> Vec<Notification> {
        // TODO: Rather return an iterator?
        self.resolver.diagnostics()
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
                if let AstNode::TypeRef(type_ref) = declaration.as_ref() {
                    let name = type_ref
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join("");
                    let range = node.range();
                    if let Some(resolved) = self.resolver.resolve_fully_qualified(&name) {
                        self.resolver
                            .document_references
                            .push(Reference::new(range, resolved));
                    }
                }

                NextAction::Abort
            }
            AstNode::TypeRef(type_ref) => {
                self.resolver.resolve_type_ref(type_ref, arena, &parent);

                NextAction::Abort
            }
            AstNode::ClassStatement { name, .. } | AstNode::TraitStatement { name, .. } => {
                // Register $this in the current scope
                if let Some(current_class) =
                    self.resolver
                        .resolve_type_ref(&vec![name.clone()], arena, &parent)
                {
                    self.resolver.enter_class(current_class);
                }

                NextAction::ProcessChildren(parent)
            }
            AstNode::MethodDefinitionStatement {
                name, doc_comment, ..
            } => {
                // Is there a doc comment?
                if let Some(doc_comment) = doc_comment {
                    if let AstNode::DocComment { return_type, .. } = doc_comment.as_ref() {
                        for rt in return_type {
                            if let Some(type_ref) = get_type_ref(rt) {
                                self.resolver.resolve_type_ref(&type_ref, arena, &parent);
                            }
                        }
                    }
                }

                let method_name = name.to_string();

                if let Some(current_class) = self.resolver.current_class {
                    for method in current_class.children(arena) {
                        if arena[method].get().name == method_name {
                            self.resolver.scope_container = method;

                            break;
                        }
                    }
                }

                // Push scope for method arguments and body
                self.resolver.push_scope();

                NextAction::ProcessChildren(parent)
            }
            AstNode::Variable(token) => {
                if let Some(node) = self.resolver.get_local(token) {
                    self.resolver.reference_local(token, &node);
                } else {
                    let child = arena.new_node(Symbol::from(token));

                    self.resolver.scope_container.append(child, arena);
                    self.resolver.declare_local(token, child);
                }

                NextAction::Abort
            }
            AstNode::DocCommentVar { name, types, .. } => {
                let mut data_types = Vec::new();
                if let Some(types) = types {
                    for t in types {
                        if let Some(type_ref) = get_type_ref(t) {
                            let type_ref_ref = SymbolReference::type_ref(type_ref);

                            data_types.push(type_ref_ref);
                        }
                    }
                }

                let child = arena.new_node(Symbol {
                    data_types,
                    ..Symbol::from(name)
                });
                self.resolver.scope_container.append(child, arena);
                self.resolver.declare_local(name, child);

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
                self.resolver.declare_local(var, child);

                NextAction::ProcessChildren(parent)
            }
            AstNode::FunctionArgument {
                name,
                argument_type,
                doc_comment,
                ..
            } => {
                let mut data_types = Vec::new();

                if let Some(doc_comment) = doc_comment {
                    if let AstNode::DocCommentParam { types, .. } = doc_comment.as_ref() {
                        if let Some(types) = types {
                            for t in types {
                                if let Some(type_ref) = get_type_ref(t) {
                                    data_types.push(SymbolReference::type_ref(type_ref.clone()));
                                    self.resolver.resolve_type_ref(&type_ref, arena, &parent);
                                }
                            }
                        }
                    }
                }

                if let Some(argument_type) = argument_type {
                    get_type_refs(argument_type).iter().for_each(|tr| {
                        self.resolver.resolve_type_ref(&tr, arena, &parent);
                        data_types.push(SymbolReference::type_ref(tr.clone()));
                    });
                }

                let child = arena.new_node(Symbol {
                    data_types,
                    ..Symbol::from(name)
                });

                self.resolver.scope_container.append(child, arena);
                self.resolver.declare_local(name, child);

                NextAction::ProcessChildren(parent)
            }
            AstNode::ReturnType { .. } => {
                get_type_refs(node).iter().for_each(|tr| {
                    self.resolver.resolve_type_ref(&tr, arena, &parent);
                });

                NextAction::Abort
            }
            AstNode::Binary { left, right, token } => {
                let data_type = self.resolve_member_type(&right, arena);

                if token.t == TokenType::Assignment {
                    if let AstNode::Variable(token) = left.as_ref() {
                        let child = if let Some(data_type) = data_type {
                            arena.new_node(Symbol {
                                data_types: vec![SymbolReference::node(&token, data_type)],
                                ..Symbol::from(token)
                            })
                        } else {
                            arena.new_node(Symbol::from(token))
                        };

                        self.resolver.scope_container.append(child, arena);
                        self.resolver.declare_local(token, child);
                    }
                }

                NextAction::ProcessChildren(parent)
            }
            AstNode::StaticMember { .. } | AstNode::Member { .. } => {
                self.resolve_member_type(node, arena);

                // This might result in children which are Members / StaticMembers being processed
                // more than once. But without processing the children there is (currently) no way to get
                // to the parameters of calls etc.
                NextAction::ProcessChildren(parent)
            }
            AstNode::NamespaceStatement { .. } => NextAction::Abort,
            _ => NextAction::ProcessChildren(parent),
        }
    }

    fn before(&mut self, _node: &AstNode) {}

    fn after(&mut self, node: &AstNode) {
        match node {
            AstNode::ClassStatement { .. } => {
                self.resolver.leave_class();
            }
            AstNode::TraitStatement { .. } | AstNode::MethodDefinitionStatement { .. } => {
                self.resolver.pop_scope();
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
        let (root_node, mut minimal_visibility) = 'root_node: loop {
            match current_object {
                AstNode::Clone { object, .. } => {
                    current_object = object;
                }
                AstNode::New { class, .. } => {
                    current_object = class;
                }
                AstNode::Grouping(inside) => {
                    current_object = inside;
                }
                AstNode::Call { callee, .. } => {
                    current_object = callee;
                }
                AstNode::StaticMember { object, member, .. }
                | AstNode::Member { object, member, .. } => {
                    reversed_chain.push(member);
                    current_object = object;
                }
                AstNode::Variable(token) => {
                    if let Some(node) = self.resolver.get_local(token) {
                        self.resolver.reference_local(&token, &node);

                        if let Some(name) = token.label.as_ref() {
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
                                    } else if let Some(type_ref) = reference.type_ref.as_ref() {
                                        if let Some(resolved_data_type) =
                                            self.resolver.resolve_type_ref(type_ref, arena, &node)
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

                    break (None, Visibility::None);
                }
                AstNode::Literal(token) => match token.t {
                    TokenType::TypeSelf | TokenType::Static => {
                        break (self.resolver.current_class, Visibility::Private)
                    }
                    TokenType::Parent => {
                        if let Some(current_class) = self.resolver.current_class {
                            break (
                                arena[current_class].get().get_unique_parent(
                                    &current_class,
                                    self.resolver,
                                    arena,
                                ),
                                Visibility::Protected,
                            );
                        } else {
                            break (None, Visibility::None);
                        }
                    }
                    TokenType::ConstantEncapsedString | TokenType::EncapsedAndWhitespaceString => {
                        break (None, Visibility::None);
                    }
                    _ => {
                        let sc = self.resolver.scope_container;

                        break (
                            self.resolver
                                .resolve_type_ref(&vec![token.clone()], arena, &sc),
                            Visibility::Public,
                        );
                    }
                },
                AstNode::TypeRef(tokens) => {
                    let scope_container = self.resolver.scope_container;

                    if let Some(node) =
                        self.resolver
                            .resolve_type_ref(tokens, arena, &scope_container)
                    {
                        break (Some(node), Visibility::Public);
                    } else {
                        break (None, Visibility::None);
                    }
                }
                _ => break (None, Visibility::None),
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
        .name
        .clone();

        if let Some(mut root_node) = root_node {
            // $this (root_node) will have resolved to the definition of the class
            // of the object
            'link_loop: for link in reversed_chain.iter().rev() {
                // Get the definition of the current parent and try to find "link" in it
                // Loop backwards through the inheritance chain, from the object towards its ancestors

                if let AstNode::Literal(token) = link.as_ref() {
                    if token.to_string() == "class" {
                        return None;
                    }
                }

                loop {
                    // Get the children of the current root_node, that is its properties and methods
                    for child in root_node.children(arena) {
                        // Check all children of the current class
                        let child_symbol = arena[child].get();

                        // This is the correct child
                        if child_symbol.name == link.name() {
                            // Name must be unique, so we can bail out if we find a name that has a bad visibility
                            if child_symbol.visibility < minimal_visibility {
                                self.resolver.diagnostic(
                                    file_name,
                                    link.range(),
                                    String::from(
                                        "Method was found but is not accessible from this scope.",
                                    ),
                                );

                                return None;
                            }

                            match link.as_ref() {
                                AstNode::Variable(token) | AstNode::Literal(token) => {
                                    // Register reference here
                                    self.resolver.reference_local(&token, &child);
                                }
                                _ => (),
                            }

                            match child_symbol.kind {
                                PhpSymbolKind::Property | PhpSymbolKind::Method => {
                                    for data_type in &child_symbol.data_types {
                                        if let Some(type_ref) = data_type.type_ref.as_ref() {
                                            if let Some(resolved_type) = self
                                                .resolver
                                                .resolve_type_ref(&type_ref, arena, &root_node)
                                            {
                                                root_node = resolved_type;

                                                // Got a match, stop and proceeed with the next link
                                                continue 'link_loop;
                                            }
                                        }
                                    }

                                    // No data type resolveable ... no way to proceed
                                    eprintln!("Failed, no resolvable data type");

                                    return None;
                                }
                                _ => {
                                    root_node = child;
                                }
                            }

                            // Done. Get the next link. Remain in the context of where this link was resolved
                            // This might actually be a parent of the object that the current root_node started
                            // as
                            continue 'link_loop;
                        }
                    }

                    let root_symbol = arena[root_node].get();

                    // Go through all used traits
                    if let Some(imports) = root_symbol.imports.as_ref() {
                        for import in imports.iter() {
                            if let Some(used_trait) =
                                self.resolver
                                    .resolve_type_ref(&import.path, arena, &root_node)
                            {
                                for trait_child in used_trait.children(arena) {
                                    let trait_child_symbol = arena[trait_child].get();

                                    if trait_child_symbol.name == link.name() {
                                        match link.as_ref() {
                                            AstNode::Literal(token) => {
                                                // Register reference here
                                                self.resolver.reference_local(&token, &trait_child);
                                            }
                                            _ => (),
                                        }

                                        match trait_child_symbol.kind {
                                            PhpSymbolKind::Property | PhpSymbolKind::Method => {
                                                for data_type in &trait_child_symbol.data_types {
                                                    if let Some(type_ref) =
                                                        data_type.type_ref.as_ref()
                                                    {
                                                        if let Some(resolved_type) =
                                                            self.resolver.resolve_type_ref(
                                                                &type_ref, arena, &root_node,
                                                            )
                                                        {
                                                            root_node = resolved_type;

                                                            // Got a match, stop and proceeed with the next link
                                                            continue 'link_loop;
                                                        }
                                                    }
                                                }

                                                // No data type resolveable ... no way to proceed
                                                eprintln!("Failed, no resolvable data type");

                                                return None;
                                            }
                                            _ => {
                                                root_node = trait_child;
                                            }
                                        }

                                        // Done. Get the next link. Remain in the context of where this link was resolved
                                        // This might actually be a trait use by the object that the current root_node started
                                        // as
                                        continue 'link_loop;
                                    }
                                }
                            }

                            if import.name() == link.name() {
                                eprintln!("Found the link in the imports!");
                            }
                        }
                    }
                    // End of trait usage

                    let current_class = arena[root_node].get();

                    // No parents, so no luck
                    if current_class.inherits_from.is_none() {
                        break;
                    }

                    // Change the iterator to an iterator over the children of this parent. Maybe one of them has the name.
                    // But this time we need to be careful because we must take visibility into account
                    if let Some(parent_class) =
                        current_class.get_unique_parent(&root_node, self.resolver, arena)
                    {
                        minimal_visibility = Visibility::Protected;
                        root_node = parent_class;

                        continue;
                    }

                    //Failed, no resolvable parents
                    self.resolver.diagnostic(
                        file_name,
                        link.range(),
                        "Unresolvable symbol".to_owned(),
                    );

                    return None;
                }

                self.resolver
                    .diagnostic(file_name, link.range(), "Unresolvable symbol".to_owned());
                return None;
            }

            // At this point we arrived at the last link and successfully resolved everything
            return Some(root_node);
        }

        eprintln!(">> Root node unresolvable");

        None
    }
}
