use super::Visitor;
use super::Symbol;
use crate::token::Token;
use super::NextAction;
use crate::node::{Node as AstNode, NodeRange};
use std::collections::HashMap;
use indextree::{Arena, NodeId};

#[derive(Debug, Clone)]
pub struct Reference {
    /// Location of the reference in the current document
    pub range: NodeRange,

    /// NodeId the reference if pointing to
    pub node: NodeId
}

impl Reference {
    pub fn new(range: NodeRange, node: NodeId) -> Self {
        Reference {
            range, node
        }
    }
}

pub struct NameResolver<'a> {
    global_scope: &'a HashMap<String, NodeId>,
    imports: HashMap<String, String>,
    current_namespace: String,

    /// Contains locally defined variables and functions
    local_scopes: Vec<HashMap<String, NodeId>>,

    pub document_references: Vec<Reference>
}

impl<'a> NameResolver<'a> {
    pub fn new(global_scope: &'a HashMap<String, NodeId>) -> Self {
        NameResolver {
            global_scope,
            imports: HashMap::new(),
            current_namespace: String::new(),
            local_scopes: vec![HashMap::new()],
            document_references: Vec::new()
        }
    }

    pub fn references(&self) -> Vec<Reference> {
        self.document_references.clone()
    }

    pub fn push_scope(&mut self) {
        self.local_scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.local_scopes.pop();
    }

    pub fn get_local(&self, token: &Token) -> Option<NodeId> {
        if let Some(top_scope) = self.local_scopes.last() {
            if let Some(node) = top_scope.get(&token.clone().label.unwrap()) {
                return Some(*node);
            }
        }

        None
    }

    pub fn reference_local(&mut self, token: &Token, node: &NodeId) {
        eprintln!("Referencing local variable");
        self.document_references.push(Reference::new(token.range(), *node))
    }

    pub fn declare_local(&mut self, token: &Token, t: NodeId) {
        if let Some(top_scope) = self.local_scopes.last_mut() {
            let name = token.clone().label.unwrap();

            if let Some(node) = top_scope.get(&name) {
                self.document_references.push(Reference::new(token.range(), *node));
            } else {
                top_scope.insert(name, t);
            }
        }
    }

    pub fn enter_namespace(&mut self, namespace: &str) {
        self.current_namespace = namespace.to_owned();
    }

    pub fn resolve_type_ref(&mut self, tokens: &Vec<Token>) -> Option<&NodeId> {
        let tokens = tokens.iter().filter(|t| t.label.is_some()).map(|t| t.clone()).collect::<Vec<Token>>();

        if tokens.len() == 0 {
            return None;
        }

        let mut joined_name = tokens
        .iter()
        .map(|n| n.clone().label.unwrap())
        .collect::<Vec<String>>()
        .join("\\");
        let name = tokens.first().unwrap().label.clone().unwrap();

        // Prefer imported symbol
        if let Some(import) = self.imports.get(&name) {
            joined_name = if tokens.len() > 1 {
                let end = tokens.iter().skip(1).map(|n| n.clone().label.unwrap_or_else(|| "\\".to_owned()))
                .collect::<Vec<String>>()
                .join("");
                format!("{}\\{}", import, end)
            } else {
                import.to_owned()
            };
        }

        if let Some(node) = self.global_scope.get(&joined_name) {
            eprintln!("YEAH: {}", joined_name);

            let range = (tokens.first().unwrap().start(), tokens.last().unwrap().end());
            self.document_references.push(Reference::new(range, *node))
        } else {
            eprintln!("ARGH: {}", joined_name);
        }

        return self.global_scope.get(&joined_name);
    }
}

pub struct NameResolveVisitor<'a, 'b: 'a> {
    resolver: &'b mut NameResolver<'a>
}

impl<'a, 'b: 'a> NameResolveVisitor<'a, 'b> {
    pub fn new(resolver: &'b mut NameResolver<'a>) -> Self {
        NameResolveVisitor {
            resolver
        }
    }

    pub fn references(&self) -> Vec<Reference> {
        self.resolver.references()
    }
}

/// The NameResolveVisitor walks the AST and creates a list of references
/// Furthermore it adds variables to the arena underneath the files entry
impl<'a, 'b: 'a> Visitor for NameResolveVisitor<'a, 'b>  {

    /// Decides if a symbol is worth collecting
    fn visit(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId) -> NextAction {
        match node {
            AstNode::NamespaceStatement { type_ref, .. } => {
                let name = match &**type_ref {
                    AstNode::TypeRef(tokens) =>
                        tokens
                            .iter()
                            .map(|n| n.clone().label.unwrap_or_else(|| "\\".to_owned()))
                            .collect::<Vec<String>>()
                            .join(""),

                    _ => panic!("This should not happen"),
                };

                self.resolver.enter_namespace(&name);

                NextAction::ProcessChildren
            },
            AstNode::TypeRef(type_ref) => {
                self.resolver.resolve_type_ref(type_ref);

                NextAction::Abort
            }
            AstNode::ClassStatement { .. } => {
                self.resolver.push_scope();

                NextAction::ProcessChildren
            }
            AstNode::MethodDefinitionStatement {
                doc_comment,
                ..
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

                // Push scope for method arguments and body
                self.resolver.push_scope();

                NextAction::ProcessChildren
            }
            AstNode::Variable(token) => {
                if let Some(node) = self.resolver.get_local(token) {
                    self.resolver.reference_local(token, &node);
                } else {
                    let child = arena.new_node(Symbol::from(token));

                    parent.append(child, arena);
                    self.resolver.declare_local(token, child);
                }

                NextAction::Abort
            }
            AstNode::FunctionArgument{name, ..} => {
                let child = arena.new_node(Symbol::from(name));

                parent.append(child, arena);
                self.resolver.declare_local(name, child);

                NextAction::Abort
            }
            AstNode::Call {..} => {
                // Do the magic here to parse $this->is->a->call()->or()->not();
                NextAction::ProcessChildren
            }
            _ => NextAction::ProcessChildren
        }
    }

    fn before(&mut self, _node: &AstNode) {

    }

    fn after(&mut self, node: &AstNode) {
        match node {
            AstNode::ClassStatement { .. }
            | AstNode::TraitStatement { .. }
            | AstNode::MethodDefinitionStatement { .. } => {
                self.resolver.pop_scope();
            },
            _ => ()
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