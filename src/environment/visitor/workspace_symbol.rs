use super::super::get_range;
use super::super::import::collect_uses;
use super::NextAction;
use super::Symbol;
use super::Visitor;
use crate::environment::symbol::{PhpSymbolKind, Visibility};
use crate::environment::{import::namespace_to_string, scope::Reference};
use crate::parser::node::Node as AstNode;
use crate::parser::token::{name as token_name, range as token_range, Token};
use indextree::{Arena, NodeId};

pub struct WorkspaceSymbolVisitor {
    namespace_stack: Vec<String>,
}

impl WorkspaceSymbolVisitor {
    pub fn new() -> Self {
        WorkspaceSymbolVisitor {
            namespace_stack: Vec::new(),
        }
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
                    imports.extend(collect_uses(node, &[]));
                } else {
                    file_symbol.imports = Some(collect_uses(node, &[]));
                }

                NextAction::Abort
            }
            AstNode::UseTraitAlterationBlock { .. }
            | AstNode::UseTraitInsteadOf { .. }
            | AstNode::UseTraitAs { .. }
            | AstNode::UseTrait { .. } => {
                let mut class_symbol = arena[parent].get_mut();

                if let Some(imports) = class_symbol.imports.as_mut() {
                    imports.extend(collect_uses(node, &[]));
                } else {
                    class_symbol.imports = Some(collect_uses(node, &[]));
                }

                NextAction::Abort
            }
            AstNode::Block { .. } => NextAction::ProcessChildren(parent),
            AstNode::NamespaceStatement { type_ref, .. } => {
                let (name, selection_range) = match type_ref.as_ref() {
                    AstNode::TypeRef(tokens) => {
                        (token_name(tokens), get_range(token_range(tokens)))
                    }
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
                    name: name.clone().label.unwrap(),
                    kind: PhpSymbolKind::Constant,
                    range: get_range(node.range()),
                    selection_range: get_range(node.range()),
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::Abort
            }
            AstNode::ClassStatement {
                name,
                extends,
                implements,
                ..
            } => {
                let inherits_from = if let Some(extends) = extends {
                    if let Some(extends) = get_type_ref(extends) {
                        Some(vec![Reference::type_ref(extends)])
                    } else {
                        None
                    }
                } else {
                    None
                };
                let selection_range = get_range(name.range());
                let s_name = name.clone().label.unwrap();
                let range = get_range(node.range());

                let namespace = if let Some(ns) = self.namespace_stack.last() {
                    Some(ns.clone())
                } else {
                    None
                };

                let mut data_types = vec![Reference::type_ref(vec![name.clone()])];
                if let Some(implements) = implements {
                    data_types.extend(
                        implements
                            .iter()
                            .filter_map(get_type_ref)
                            .map(Reference::type_ref),
                    );
                }

                let child = arena.new_node(Symbol {
                    namespace,
                    name: s_name,
                    kind: PhpSymbolKind::Class,
                    range,
                    selection_range,
                    inherits_from,
                    data_types,
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
            AstNode::TraitStatement { name, .. } => {
                let selection_range = get_range(name.range());
                let name = name.clone().label.unwrap();
                let range = get_range(node.range());

                let namespace = if let Some(ns) = self.namespace_stack.last() {
                    Some(ns.clone())
                } else {
                    None
                };

                let child = arena.new_node(Symbol {
                    namespace,
                    name,
                    kind: PhpSymbolKind::Class,
                    range,
                    selection_range,
                    ..Symbol::default()
                });
                parent.append(child, arena);

                NextAction::ProcessChildren(child)
            }
            AstNode::Interface { name, extends, .. } => {
                let selection_range = get_range(name.range());
                let name = name.clone().label.unwrap();
                let range = get_range(node.range());
                let inherits_from = if let Some(extends) = extends {
                    Some(
                        extends
                            .iter()
                            .filter_map(get_type_ref)
                            .map(Reference::type_ref)
                            .collect(),
                    )
                } else {
                    None
                };

                let namespace = if let Some(ns) = self.namespace_stack.last() {
                    Some(ns.clone())
                } else {
                    None
                };

                let child = arena.new_node(Symbol {
                    namespace,
                    name,
                    kind: PhpSymbolKind::Interface,
                    range,
                    selection_range,
                    inherits_from,
                    ..Symbol::default()
                });
                parent.append(child, arena);

                NextAction::ProcessChildren(child)
            }
            AstNode::ClassConstantDefinitionStatement { .. } => NextAction::ProcessChildren(parent),
            AstNode::ClassConstant {
                name, visibility, ..
            } => {
                let range = get_range(node.range());
                let child = arena.new_node(Symbol {
                    name: name.clone().label.unwrap(),
                    kind: PhpSymbolKind::Constant,
                    range,
                    selection_range: range,
                    visibility: Visibility::from(visibility),
                    ..Symbol::default()
                });
                parent.append(child, arena);

                NextAction::Abort
            }
            AstNode::DocCommentProperty { name, types, .. } => {
                let range = get_range(node.range());
                let mut combined_data_types = Vec::new();

                if let Some(data_types) = types {
                    for rt in data_types {
                        combined_data_types.extend(
                            get_type_refs(rt)
                                .iter()
                                .map(|tr| Reference::type_ref(tr.clone()))
                                .collect::<Vec<Reference>>(),
                        );
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
                name,
                data_type,
                doc_comment,
                visibility,
                ..
            } => {
                let range = get_range(node.range());

                let mut data_types = if let Some(data_type) = data_type {
                    get_type_refs(data_type)
                        .iter()
                        .map(|tr| Reference::type_ref(tr.clone()))
                        .collect()
                } else {
                    Vec::new()
                };

                // Is there a doc comment?

                ref_from_doc!(doc_comment, data_types, var_docs);

                let child = arena.new_node(Symbol {
                    name: name.clone().label.unwrap(),
                    kind: PhpSymbolKind::Property,
                    range,
                    selection_range: range,
                    data_types,
                    visibility: Visibility::from(visibility),
                    ..Symbol::default()
                });

                parent.append(child, arena);

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

                let child = arena.new_node(Symbol {
                    name: name.clone().label.unwrap(),
                    kind: PhpSymbolKind::Method,
                    range: get_range(node.range()),
                    selection_range: get_range(name.range()),
                    data_types,
                    is_static: is_static.is_some(),
                    visibility: Visibility::from(visibility),
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
                    eprintln!("Invalid function {:?}", function);

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

                let child = arena.new_node(Symbol {
                    name: name.clone().label.unwrap(),
                    kind: PhpSymbolKind::Function,
                    range: get_range(node.range()),
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
                    name: name.clone().label.unwrap(),
                    kind: PhpSymbolKind::FunctionParameter,
                    range: get_range(node.range()),
                    selection_range: get_range(name.range()),
                    data_types,
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::Abort
            }
            AstNode::DefineStatement { name, .. } => {
                if let AstNode::Literal(token) = &**name {
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

            _ => NextAction::Abort,
        }
    }

    fn before(&mut self, node: &AstNode, _arena: &mut Arena<Symbol>, _parent: NodeId) {
        match node {
            AstNode::NamespaceStatement { type_ref, .. } => {
                if let AstNode::TypeRef(type_ref) = type_ref.as_ref() {
                    self.namespace_stack.push(namespace_to_string(type_ref))
                }
            }
            AstNode::NamespaceBlock { type_ref, .. } => {
                if let Some(type_ref) = type_ref {
                    if let AstNode::TypeRef(type_ref) = type_ref.as_ref() {
                        self.namespace_stack.push(namespace_to_string(type_ref))
                    }
                }
            }
            _ => (),
        }
    }

    fn after(&mut self, node: &AstNode, _arena: &mut Arena<Symbol>, _parent: NodeId) {
        match node {
            AstNode::NamespaceBlock { .. } => {
                self.namespace_stack.pop();
            }
            _ => (),
        }
    }
}

pub(crate) fn get_type_refs(node: &AstNode) -> Vec<Vec<Token>> {
    match node {
        AstNode::ReturnType { data_type, .. } => get_type_refs(data_type),
        AstNode::DataType { type_refs, .. } => type_refs.iter().filter_map(get_type_ref).collect(),
        AstNode::DocCommentVar { types, .. }
        | AstNode::DocCommentReturn { types, .. }
        | AstNode::DocCommentParam { types, .. } => {
            if let Some(types) = types {
                types.iter().filter_map(get_type_ref).collect()
            } else {
                Vec::new()
            }
        }
        _ => Vec::new(),
    }
}

pub(crate) fn get_type_ref(node: &AstNode) -> Option<Vec<Token>> {
    if let AstNode::TypeRef(tokens) = node {
        return Some(tokens.clone());
    }

    None
}
