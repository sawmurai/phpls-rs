use super::super::get_range;
use super::super::import::collect_uses;
use super::NextAction;
use super::Symbol;
use super::Visitor;
use crate::environment::scope::Reference;
use crate::environment::symbol::{PhpSymbolKind, Visibility};
use crate::parser::node::Node as AstNode;
use crate::parser::token::Token;

use indextree::{Arena, NodeId};

pub struct WorkspaceSymbolVisitor {}

impl Visitor for WorkspaceSymbolVisitor {
    /// Decides if a symbol is worth collecting
    fn visit(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId) -> NextAction {
        match node {
            AstNode::UseFunctionStatement { .. } => NextAction::ProcessChildren,
            AstNode::UseStatement { .. } => NextAction::ProcessChildren,
            AstNode::UseTraitStatement { .. } => NextAction::ProcessChildren,
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
                    imports.extend(collect_uses(node, &Vec::new()));
                } else {
                    file_symbol.imports = Some(collect_uses(node, &Vec::new()));
                }

                NextAction::Abort
            }
            AstNode::UseTraitAlterationBlock { .. }
            | AstNode::UseTraitInsteadOf { .. }
            | AstNode::UseTraitAs { .. }
            | AstNode::UseTrait { .. } => {
                let mut class_symbol = arena[parent].get_mut();

                if let Some(imports) = class_symbol.imports.as_mut() {
                    imports.extend(collect_uses(node, &Vec::new()));
                } else {
                    class_symbol.imports = Some(collect_uses(node, &Vec::new()));
                }

                NextAction::Abort
            }
            AstNode::Block { .. } => NextAction::ProcessChildren,
            AstNode::NamespaceStatement { type_ref, .. } => {
                let (name, selection_range) = match &**type_ref {
                    AstNode::TypeRef(tokens) => (
                        tokens
                            .iter()
                            .map(|n| n.to_string())
                            .collect::<Vec<String>>()
                            .join(""),
                        // Range from first part of name until the last one
                        get_range((
                            tokens.first().unwrap().range().0,
                            tokens.last().unwrap().range().1,
                        )),
                    ),
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

                NextAction::ProcessChildren
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
            AstNode::ClassStatement { name, extends, .. } => {
                let inherits_from = if let Some(extends) = extends {
                    extends
                        .iter()
                        .map(|parent| get_type_ref(parent))
                        .filter(std::option::Option::is_some)
                        .map(|filtered| Reference::type_ref(filtered.unwrap()))
                        .collect()
                } else {
                    Vec::new()
                };
                let selection_range = get_range(name.range());
                let name = name.clone().label.unwrap();
                let range = get_range(node.range());

                let child = arena.new_node(Symbol {
                    name,
                    kind: PhpSymbolKind::Class,
                    range,
                    selection_range,
                    inherits_from,
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::ProcessChildren
            }
            AstNode::TraitStatement { name, .. } => {
                let selection_range = get_range(name.range());
                let name = name.clone().label.unwrap();
                let range = get_range(node.range());

                let child = arena.new_node(Symbol {
                    name,
                    kind: PhpSymbolKind::Class,
                    range,
                    selection_range,
                    ..Symbol::default()
                });
                parent.append(child, arena);

                NextAction::ProcessChildren
            }
            AstNode::Interface { name, extends, .. } => {
                let selection_range = get_range(name.range());
                let name = name.clone().label.unwrap();
                let range = get_range(node.range());
                let inherits_from = if let Some(extends) = extends {
                    extends
                        .iter()
                        .map(|parent| get_type_ref(parent))
                        .filter(std::option::Option::is_some)
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
                    inherits_from,
                    ..Symbol::default()
                });
                parent.append(child, arena);

                NextAction::ProcessChildren
            }
            AstNode::ClassConstantDefinitionStatement {
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
            AstNode::PropertyDefinitionStatement {
                name,
                data_type,
                doc_comment,
                visibility,
                ..
            } => {
                let range = get_range(node.range());

                let mut data_types = if let Some(data_type) = data_type {
                    if let Some(type_ref) = get_type_ref(data_type) {
                        vec![Reference::type_ref(type_ref)]
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };

                // Is there a doc comment?
                if let Some(doc_comment) = doc_comment {
                    if let AstNode::DocComment { var_docs, .. } = doc_comment.as_ref() {
                        for rt in var_docs {
                            if let Some(type_ref) = get_type_ref(rt) {
                                if !type_ref.is_empty() {
                                    data_types.push(Reference::type_ref(type_ref));
                                }
                            }
                        }
                    }
                }

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
                        // Err("Invalid function".to_owned());
                        return NextAction::Abort;
                    };

                let mut data_types = if let Some(data_type) = return_type {
                    if let Some(type_ref) = get_type_ref(&*data_type) {
                        vec![Reference::type_ref(type_ref)]
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };

                // Is there a doc comment?
                if let Some(doc_comment) = doc_comment {
                    if let AstNode::DocComment { return_type, .. } = doc_comment.as_ref() {
                        for rt in return_type {
                            if let Some(type_ref) = get_type_ref(rt) {
                                data_types.push(Reference::type_ref(type_ref));
                            }
                        }
                    }
                }

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

                NextAction::ProcessChildren
            }

            AstNode::NamedFunctionDefinitionStatement { name, function, .. } => {
                let return_type =
                    if let AstNode::FunctionDefinitionStatement { return_type, .. } =
                        function.as_ref()
                    {
                        return_type
                    } else {
                        eprintln!("Invalid function {:?}", function);

                        return NextAction::Abort;
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
                    inherits_from: Vec::new(),
                    data_types,
                    ..Symbol::default()
                });

                parent.append(child, arena);

                NextAction::Abort
            }

            _ => NextAction::Abort,
        }
    }

    fn before(&mut self, _node: &AstNode) {}

    fn after(&mut self, _node: &AstNode) {}
}

// TODO: Think about supporting multiple types
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
        AstNode::DocCommentVar { types, .. }
        | AstNode::DocCommentReturn { types, .. }
        | AstNode::DocCommentParam { types, .. } => {
            if let Some(types) = types {
                for t in types {
                    if let AstNode::TypeRef(items) = t {
                        return Some(items.clone());
                    };
                }
            }

            None
        }
        _ => None,
    }
}
