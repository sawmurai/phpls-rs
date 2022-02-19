use super::super::get_range;
use super::super::import::{collect_alterations, collect_uses};
use super::NextAction;
use super::Symbol;
use super::Visitor;
use crate::parser::node::Node as AstNode;
use crate::{environment::scope::Reference, parser::node::ClassStatement};
use crate::{
    environment::symbol::{FunctionParameter, PhpSymbolKind, Visibility},
    parser::node::TypeRef,
};
use indextree::{Arena, NodeId};

#[derive(Default)]
pub struct WorkspaceSymbolVisitor {
    namespace: Option<String>,
}

impl WorkspaceSymbolVisitor {
    pub fn new() -> Self {
        WorkspaceSymbolVisitor { namespace: None }
    }
}

impl Visitor for WorkspaceSymbolVisitor {
    /// Decides if a symbol is worth collecting
    fn visit(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId) -> NextAction {
        match node {
            // In case of a function definition we immediately go down since the function
            // itself was already processed by either the MethodDefinition or the NamedFunctionDefinition
            // All that is left now is to define the arguments
            AstNode::FunctionDefinitionStatement { .. } => NextAction::ProcessChildren(parent),
            AstNode::UseFunctionStatement { .. } => NextAction::ProcessChildren(parent),
            AstNode::UseStatement { .. } => NextAction::ProcessChildren(parent),
            AstNode::UseTraitStatement { .. } => NextAction::ProcessChildren(parent),
            AstNode::DocComment { .. } => NextAction::ProcessChildren(parent),
            AstNode::GroupedUse { .. }
            | AstNode::UseDeclaration { .. }
            | AstNode::UseFunction { .. }
            | AstNode::UseConst { .. } => {
                // Either get the file if parent is a namespace or the class if parent is a block
                let mut file_symbol = if let Some(grandparent) = arena[parent].parent() {
                    arena[grandparent].get_mut()
                } else {
                    arena[parent].get_mut()
                };

                if let Some(imports) = file_symbol.imports.as_mut() {
                    imports.extend(collect_uses(node, &[].into()));
                } else {
                    file_symbol.imports = Some(collect_uses(node, &[].into()).into());
                }

                NextAction::Abort
            }
            AstNode::UseTraitInsteadOf { .. }
            | AstNode::UseTraitAs { .. }
            | AstNode::UseTraitAlterationBlock { .. }
            | AstNode::UseTrait { .. } => {
                let mut class_symbol = arena[parent].get_mut();

                if let Some(imports) = class_symbol.imports.as_mut() {
                    imports.extend(collect_uses(node, &[].into()));
                } else {
                    class_symbol.imports = Some(collect_uses(node, &[].into()).into());
                }

                if let Some(imports) = class_symbol.import_resolutions.as_mut() {
                    imports.extend(collect_alterations(node));
                } else {
                    class_symbol.import_resolutions = Some(collect_alterations(node));
                }

                NextAction::Abort
            }
            AstNode::Block { .. } => NextAction::ProcessChildren(parent),
            AstNode::NamespaceStatement { type_ref, .. } => {
                let (name, selection_range) = match type_ref.as_ref() {
                    AstNode::TypeRef(tokens) => (tokens.to_fqdn(), get_range(tokens.range())),
                    _ => panic!("This should not happen"),
                };

                let symbol = Symbol {
                    kind: PhpSymbolKind::Namespace,
                    name,
                    selection_range,
                    range: arena[parent].get().selection_range,
                    ..Symbol::default()
                };

                let new_node = arena.new_node(symbol);
                parent.append(new_node, arena);

                NextAction::ProcessChildren(new_node)
            }
            AstNode::Const { name, .. } => {
                let child = arena.new_node(Symbol {
                    name: name.to_string(),
                    kind: PhpSymbolKind::Constant,
                    range: get_range(node.range()),
                    selection_range: get_range(node.range()),
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::Abort
            }
            AstNode::ClassStatement(ClassStatement {
                name,
                extends,
                implements,
                doc_comment,
                attributes,
                ..
            }) => {
                let inherits_from = extends.as_ref().map(|extends| {
                    get_type_ref(extends)
                        .iter()
                        .flat_map(|extends| vec![Reference::type_ref(extends.clone())])
                        .collect()
                });
                let selection_range = get_range(name.range());
                let s_name = name.to_string();
                let range = get_range(node.range());

                let namespace = self.namespace.as_ref().cloned();

                let mut data_types = vec![Reference::type_ref(vec![name.clone()].into())];
                if let Some(implements) = implements {
                    data_types.extend(
                        implements
                            .iter()
                            .flat_map(get_type_ref)
                            .map(Reference::type_ref),
                    );
                }

                let is_attribute = attributes.iter().any(|attribute| {
                    if let AstNode::Attribute { expressions, .. } = attribute {
                        expressions.iter().any(|expr| {
                            if let AstNode::TypeRef(tr) = expr {
                                tr.root_token().to_string() == "Attribute"
                            } else {
                                false
                            }
                        })
                    } else {
                        false
                    }
                });

                let child = arena.new_node(Symbol {
                    namespace,
                    name: s_name,
                    kind: PhpSymbolKind::Class,
                    range,
                    selection_range,
                    inherits_from,
                    data_types,
                    deprecated: deprecated_from_doc!(doc_comment),
                    is_attribute,
                    ..Symbol::default()
                });

                parent.append(child, arena);

                child.append(
                    arena.new_node(Symbol {
                        name: "class".to_string(),
                        kind: PhpSymbolKind::MagicConst,
                        range: selection_range,
                        selection_range,
                        is_static: true,
                        ..Symbol::default()
                    }),
                    arena,
                );

                NextAction::ProcessChildren(child)
            }
            AstNode::TraitStatement {
                name, doc_comment, ..
            } => {
                let selection_range = get_range(name.range());
                let name = name.to_string();
                let range = get_range(node.range());

                let namespace = self.namespace.as_ref().cloned();

                let child = arena.new_node(Symbol {
                    namespace,
                    name,
                    kind: PhpSymbolKind::Trait,
                    range,
                    selection_range,
                    deprecated: deprecated_from_doc!(doc_comment),
                    ..Symbol::default()
                });
                parent.append(child, arena);

                NextAction::ProcessChildren(child)
            }
            AstNode::Interface {
                name,
                extends,
                doc_comment,
                ..
            } => {
                let selection_range = get_range(name.range());
                let name = name.to_string();
                let range = get_range(node.range());
                let inherits_from = extends.as_ref().map(|extends| {
                    extends
                        .iter()
                        .flat_map(get_type_ref)
                        .map(Reference::type_ref)
                        .collect()
                });

                let namespace = self.namespace.as_ref().cloned();

                let child = arena.new_node(Symbol {
                    namespace,
                    name,
                    kind: PhpSymbolKind::Interface,
                    range,
                    selection_range,
                    inherits_from,
                    deprecated: deprecated_from_doc!(doc_comment),
                    ..Symbol::default()
                });
                parent.append(child, arena);

                NextAction::ProcessChildren(child)
            }
            AstNode::ClassConstantDefinitionStatement {
                consts,
                doc_comment,
                ..
            } => {
                let deprecated = deprecated_from_doc!(doc_comment);

                for constant in consts {
                    if let AstNode::ClassConstant {
                        name, visibility, ..
                    } = constant
                    {
                        let mut data_types = Vec::new();
                        ref_from_doc!(doc_comment, data_types, var_docs);

                        let range = get_range(node.range());
                        let child = arena.new_node(Symbol {
                            name: name.to_string(),
                            kind: PhpSymbolKind::Constant,
                            range,
                            selection_range: range,
                            visibility: Visibility::from(visibility),
                            data_types,
                            deprecated,
                            ..Symbol::default()
                        });
                        parent.append(child, arena);
                    }
                }

                NextAction::Abort
            }
            AstNode::DocCommentProperty { name, types, .. } => {
                let range = get_range(node.range());
                let mut combined_data_types = Vec::new();

                if let Some(data_types) = types {
                    for rt in data_types {
                        combined_data_types.push(Reference::type_ref(rt.clone()));
                    }
                }

                let child = arena.new_node(Symbol {
                    name: name.clone().label.unwrap(),
                    kind: PhpSymbolKind::Property,
                    range,
                    selection_range: range,
                    data_types: combined_data_types,
                    visibility: Visibility::Private,
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::Abort
            }
            AstNode::PropertyDefinitionStatement {
                properties,
                data_type,
                doc_comment,
                visibility,
                ..
            } => {
                let deprecated = deprecated_from_doc!(doc_comment);
                let range = get_range(node.range());

                let mut data_types = if let Some(data_type) = data_type {
                    get_type_refs(data_type)
                        .iter()
                        .map(|tr| Reference::type_ref(tr.clone()))
                        .collect()
                } else {
                    Vec::new()
                };

                ref_from_doc!(doc_comment, data_types, var_docs);

                for prop in properties {
                    if let AstNode::Property { name, .. } = prop {
                        let child = arena.new_node(Symbol {
                            name: name.to_string(),
                            kind: PhpSymbolKind::Property,
                            range,
                            selection_range: range,
                            data_types: data_types.clone(),
                            visibility: Visibility::from(visibility),
                            deprecated,
                            ..Symbol::default()
                        });

                        parent.append(child, arena);
                    }
                }

                NextAction::Abort
            }
            AstNode::MethodDefinitionStatement {
                name,
                function,
                is_static,
                doc_comment,
                visibility,
                ..
            } => {
                let return_type =
                    if let AstNode::FunctionDefinitionStatement { return_type, .. } =
                        function.as_ref()
                    {
                        return_type
                    } else {
                        return NextAction::Abort;
                    };

                let mut data_types = if let Some(data_type) = return_type {
                    get_type_refs(&*data_type)
                        .iter()
                        .map(|tr| Reference::type_ref(tr.clone()))
                        .collect()
                } else {
                    Vec::new()
                };

                ref_from_doc!(doc_comment, data_types, return_type);

                let range = if let Some(doc_comment) = doc_comment {
                    (doc_comment.as_ref(), node).into()
                } else {
                    node.range()
                };

                let child = arena.new_node(Symbol {
                    name: name.to_string(),
                    kind: PhpSymbolKind::Method,
                    range: get_range(range),
                    selection_range: get_range(name.range()),
                    data_types,
                    is_static: is_static.is_some(),
                    visibility: Visibility::from(visibility),
                    deprecated: deprecated_from_doc!(doc_comment),
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::ProcessChildren(child)
            }

            AstNode::ArrowFunction { token, .. } => {
                // TODO: Add data type callable
                let child = arena.new_node(Symbol {
                    name: format!("af-{:?}", token.range()),
                    kind: PhpSymbolKind::Function,
                    range: get_range(node.range()),
                    selection_range: get_range(token.range()),
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::ProcessChildren(child)
            }

            AstNode::NamedFunctionDefinitionStatement { name, function, .. } => {
                let (return_type, doc_comment) = if let AstNode::FunctionDefinitionStatement {
                    doc_comment,
                    return_type,
                    ..
                } = function.as_ref()
                {
                    (return_type, doc_comment)
                } else {
                    return NextAction::Abort;
                };

                let mut data_types = if let Some(data_type) = return_type {
                    get_type_refs(data_type)
                        .iter()
                        .map(|tr| Reference::type_ref(tr.clone()))
                        .collect()
                } else {
                    Vec::new()
                };

                ref_from_doc!(doc_comment, data_types, return_type);

                let range = if let Some(doc_comment) = doc_comment {
                    (doc_comment.as_ref(), node).into()
                } else {
                    node.range()
                };

                let child = arena.new_node(Symbol {
                    name: name.to_string(),
                    kind: PhpSymbolKind::Function,
                    range: get_range(range),
                    selection_range: get_range(name.range()),
                    data_types,
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::ProcessChildren(child)
            }
            AstNode::FunctionArgument {
                name,
                argument_type,
                doc_comment,
                ..
            } => {
                let mut data_types = if let Some(data_type) = argument_type {
                    get_type_refs(data_type)
                        .iter()
                        .map(|tr| Reference::type_ref(tr.clone()))
                        .collect()
                } else {
                    Vec::new()
                };

                if let Some(param) = doc_comment {
                    data_types.extend(
                        get_type_refs(param)
                            .iter()
                            .map(|tr| Reference::type_ref(tr.clone()))
                            .collect::<Vec<Reference>>(),
                    )
                }

                let child = arena.new_node(Symbol {
                    name: name.to_string(),
                    kind: PhpSymbolKind::FunctionParameter,
                    range: get_range(node.range()),
                    selection_range: get_range(name.range()),
                    data_types,
                    deprecated: deprecated_from_doc!(doc_comment),
                    ..Symbol::default()
                });

                parent.append(child, arena);

                arena[parent]
                    .get_mut()
                    .parameters
                    .push(FunctionParameter::new(&child, node));

                NextAction::Abort
            }
            AstNode::DefineStatement { name, .. } => {
                if let AstNode::Literal(token) = name.as_ref() {
                    let simple_name = token.label.clone().unwrap();

                    let child = arena.new_node(Symbol {
                        name: simple_name,
                        kind: PhpSymbolKind::Constant,
                        range: get_range(node.range()),
                        selection_range: get_range(name.range()),
                        ..Symbol::default()
                    });

                    parent.append(child, arena);
                }

                NextAction::Abort
            }

            _ => NextAction::ProcessChildren(parent),
        }
    }

    fn before(&mut self, node: &AstNode, _arena: &mut Arena<Symbol>, _parent: NodeId) {
        match node {
            AstNode::NamespaceStatement { type_ref, .. } => {
                if let AstNode::TypeRef(type_ref) = type_ref.as_ref() {
                    self.namespace = Some(type_ref.to_fqdn());
                }
            }
            AstNode::NamespaceBlock { type_ref, .. } => {
                if let Some(type_ref) = type_ref {
                    if let AstNode::TypeRef(type_ref) = type_ref.as_ref() {
                        self.namespace = Some(type_ref.to_fqdn());
                    }
                } else {
                    self.namespace = None;
                }
            }
            _ => (),
        }
    }

    fn after(&mut self, node: &AstNode, _arena: &mut Arena<Symbol>, _parent: NodeId) {
        if let AstNode::NamespaceBlock { .. } = node {
            self.namespace = None;
        }
    }
}

pub(crate) fn get_type_refs(node: &AstNode) -> Vec<TypeRef> {
    match node {
        AstNode::ReturnType { data_type, .. } => get_type_refs(data_type),
        AstNode::DataType { type_refs, .. } => type_refs.iter().flat_map(get_type_ref).collect(),
        AstNode::DocCommentVar { types, .. }
        | AstNode::DocCommentReturn { types, .. }
        | AstNode::DocCommentParam { types, .. } => {
            if let Some(types) = types {
                types.clone()
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}

pub(crate) fn get_type_ref(node: &AstNode) -> Vec<TypeRef> {
    if let AstNode::TypeRef(tokens) = node {
        return vec![tokens.clone()];
    }

    if let AstNode::DocCommentReturn { types, .. } = node {
        if let Some(types) = types {
            return types.iter().map(|t| t.clone()).collect();
        }
    }

    vec![]
}
