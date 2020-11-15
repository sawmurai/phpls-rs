use super::NextAction;
use super::Visitor;
use super::{super::PhpSymbolKind, Symbol};
use crate::node::{Node as AstNode, NodeRange};
use crate::token::{Token, TokenType};
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

pub struct NameResolver<'a> {
    global_scope: &'a HashMap<String, NodeId>,
    imports: HashMap<String, String>,
    current_namespace: String,

    /// Contains locally defined variables and functions
    local_scopes: Vec<HashMap<String, NodeId>>,

    /// Contains a stack of references to $this
    current_class: Option<NodeId>,

    /// Current scope container for storage of new symbols
    /// Usually a method / function body or a file
    scope_container: NodeId,

    pub document_references: Vec<Reference>,
}

impl<'a> NameResolver<'a> {
    pub fn new(global_scope: &'a HashMap<String, NodeId>, scope_container: NodeId) -> Self {
        NameResolver {
            global_scope,
            imports: HashMap::new(),
            current_namespace: String::new(),
            local_scopes: vec![HashMap::new()],
            current_class: None,
            document_references: Vec::new(),
            scope_container,
        }
    }

    /// Return the collected references
    pub fn references(&self) -> Vec<Reference> {
        self.document_references.clone()
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

    /// Enter a new namespace which is used to resolved symbols in the same namespace
    /// Preload the imports with the imports of the namespace
    pub fn enter_namespace(&mut self, namespace: &str, file: &Symbol) {
        self.current_namespace = namespace.to_owned();
        self.imports.clear();

        if let Some(imports) = file.imports.as_ref() {
            for import in imports {
                self.imports.insert(import.name(), import.full_name());
            }
        }

        eprintln!("{:?}", file);
    }

    /// Resolve fully qualified
    pub fn resolve_fully_qualified(&mut self, name: &str) -> Option<NodeId> {
        if let Some(node) = self.global_scope.get(name) {
            Some(*node)
        } else {
            None
        }
    }

    /// Resolve a TypeRef `Some\Name\Space` to the node if the definition of that symbol
    pub fn resolve_type_ref(&mut self, tokens: &Vec<Token>) -> Option<NodeId> {
        let fully_qualified =
            tokens.len() > 0 && tokens.first().unwrap().t == TokenType::NamespaceSeparator;
        let tokens = tokens
            .iter()
            .filter(|t| t.label.is_some())
            .map(|t| t.clone())
            .collect::<Vec<Token>>();

        if tokens.len() == 0 {
            return None;
        }

        // Maybe do the following only if the name starts with a backslash?
        let name = tokens.first().unwrap().label.clone().unwrap();
        eprintln!("searching for {}, fq. {}", name, fully_qualified);

        // Simple, if fully qualified do not tamper with the name
        let joined_name = if fully_qualified {
            format!(
                "\\{}",
                tokens
                    .iter()
                    .map(|n| n.clone().label.unwrap())
                    .collect::<Vec<String>>()
                    .join("\\")
            )
        } else if let Some(import) = self.imports.get(&name) {
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
        } else if self.current_namespace != "" {
            // Next try the name in the current namespace
            format!("{}\\{}", self.current_namespace, name)
        } else {
            // Otherwise use a fqdn but cut off the leading backslash
            tokens
                .iter()
                .map(|n| n.clone().label.unwrap())
                .collect::<Vec<String>>()
                .join("\\")
        };

        if let Some(node) = self.global_scope.get(&joined_name) {
            eprintln!("YEAH: {}", joined_name);

            let range = (
                tokens.first().unwrap().start(),
                tokens.last().unwrap().end(),
            );
            self.document_references.push(Reference::new(range, *node));

            return Some(*node);
        } else {
            eprintln!("ARGH: {}", joined_name);
        }

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
            AstNode::NamespaceStatement { type_ref, .. } => {
                let tokens = match &**type_ref {
                    AstNode::TypeRef(tokens) => tokens,
                    _ => panic!("This should not happen"),
                };

                let name = tokens
                    .iter()
                    .map(|n| n.clone().label.unwrap_or_else(|| "\\".to_owned()))
                    .collect::<Vec<String>>()
                    .join("");

                self.resolver.enter_namespace(&name, arena[parent].get());

                NextAction::ProcessChildren
            }
            AstNode::TypeRef(type_ref) => {
                self.resolver.resolve_type_ref(type_ref);

                NextAction::Abort
            }
            AstNode::ClassStatement { name, .. } => {
                // Register $this in the current scope
                if let Some(current_class) = self.resolver.resolve_type_ref(&vec![name.clone()]) {
                    self.resolver.enter_class(current_class);
                }

                NextAction::ProcessChildren
            }
            AstNode::MethodDefinitionStatement {
                name, doc_comment, ..
            } => {
                // Is there a doc comment?
                if let Some(doc_comment) = doc_comment {
                    if let AstNode::DocComment { return_type, .. } = doc_comment.as_ref() {
                        for rt in return_type {
                            if let Some(type_ref) = get_type_ref(rt) {
                                if self.resolver.resolve_type_ref(&type_ref).is_none() {
                                    // Add error
                                }
                            }
                        }
                    }
                }

                let method_name = format!("{}", name);

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

                NextAction::ProcessChildren
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
            AstNode::FunctionArgument { name, .. } => {
                let child = arena.new_node(Symbol::from(name));

                self.resolver.scope_container.append(child, arena);
                self.resolver.declare_local(name, child);

                NextAction::ProcessChildren
            }
            AstNode::ReturnType { data_type, .. } => {
                if let Some(type_ref) = get_type_ref(data_type) {
                    if self.resolver.resolve_type_ref(&type_ref).is_none() {
                        // Add error
                    }
                }

                NextAction::Abort
            }
            AstNode::Member { object, member, .. } => {
                let mut reversed_chain = vec![member];

                let mut current_object = object;
                let root_node = loop {
                    match current_object.as_ref() {
                        AstNode::Call { callee, .. } => {
                            current_object = callee;
                        }
                        AstNode::Member { object, member, .. } => {
                            current_object = object;
                            reversed_chain.push(member);
                        }
                        AstNode::Variable(token) => {
                            if let Some(node) = self.resolver.get_local(token) {
                                self.resolver.reference_local(&token, &node);
                                break Some(node);
                            }

                            break None;
                        }
                        AstNode::TypeRef(tokens) => {
                            if let Some(node) = self.resolver.resolve_type_ref(tokens) {
                                break Some(node);
                            }
                        }
                        _ => break None,
                    }
                };

                if let Some(mut root_node) = root_node {
                    // $this (root_node) will have resolved to the definition of the current class

                    'outer: for link in reversed_chain.iter().rev() {
                        // Get the definition of the current parent and try to find "link" in it

                        for child in root_node.children(arena) {
                            // Check all children of the root_node
                            let child_symbol = arena[child].get();

                            // This is the correct child
                            if child_symbol.name == link.name() {
                                match &***link {
                                    AstNode::Variable(token) | AstNode::Literal(token) => {
                                        // Register reference here
                                        self.resolver.reference_local(&token, &child);
                                    }
                                    _ => (),
                                }

                                match child_symbol.kind {
                                    PhpSymbolKind::Property | PhpSymbolKind::Method => {
                                        for data_type in child_symbol.data_types.iter() {
                                            if let Some(type_ref) = data_type.type_ref.as_ref() {
                                                if let Some(resolved_type) =
                                                    self.resolver.resolve_type_ref(&type_ref)
                                                {
                                                    root_node = resolved_type;
                                                }
                                            }
                                        }
                                    }
                                    _ => {
                                        root_node = child;
                                    }
                                }

                                continue 'outer;
                            }
                        }

                        eprintln!("did not find {} in ", link.name());
                        // Nothing found :(
                        break;
                    }
                }

                NextAction::Abort
            }
            _ => NextAction::ProcessChildren,
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

fn get_type_ref(node: &AstNode) -> Option<Vec<Token>> {
    match node {
        AstNode::ReturnType { data_type, .. } => get_type_ref(data_type),
        AstNode::ArgumentType { type_ref, .. } | AstNode::DataType { type_ref, .. } => {
            match &**type_ref {
                AstNode::TypeRef(items) => Some(items.clone()),
                _ => None,
            }
        }
        AstNode::TypeRef(items) => Some(items.clone()),
        AstNode::DocCommentReturn { types, .. } => {
            if let Some(types) = types {
                match &**types {
                    AstNode::TypeRef(items) => Some(items.clone()),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}
