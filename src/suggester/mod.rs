use crate::environment::{
    symbol::{PhpSymbolKind, Symbol},
    visitor::name_resolver::{NameResolver, Reference},
};
use crate::{environment::get_range, environment::in_range, parser::node::Node as AstNode};
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use tower_lsp::lsp_types::Position;

pub struct Suggestion {
    pub label: String,
    pub description: String,
}

impl Suggestion {
    pub fn new(label: &str, description: &str) -> Self {
        Self {
            label: label.to_owned(),
            description: description.to_owned(),
        }
    }
}

impl From<&Symbol> for Suggestion {
    fn from(symbol: &Symbol) -> Self {
        Suggestion::new(&symbol.name, &symbol.detail().unwrap_or("".to_string()))
    }
}

/// Find the node at the current cursor position, if any, and return it
/// along with the path to it, as the nodes themselve do not have links to
/// their parents
fn find<'a>(
    n: &'a AstNode,
    position: &Position,
    mut ancestors: Vec<&'a AstNode>,
) -> Option<(&'a AstNode, Vec<&'a AstNode>)> {
    ancestors.push(n);

    for c in n.children() {
        if in_range(position, &get_range(c.range())) {
            return find(c, position, ancestors);
        }
    }

    if in_range(position, &get_range(n.range())) {
        return Some((n, ancestors));
    }

    None
}

/// Find variables to suggest by going up the ast from the current node to
/// the next function boundary and then collecting all child-variables, as
/// these should be within the scope
fn suggest_variables(ancestors: Vec<&AstNode>) -> Vec<Suggestion> {
    let mut suggestions = Vec::new();

    let in_class = ancestors
        .iter()
        .find(|n| match n {
            AstNode::ClassStatement { .. } => true,
            _ => false,
        })
        .is_some();
    if in_class {
        suggestions.push(Suggestion::new("$this", ""));
    }

    let mut root = ancestors.first().unwrap();

    for ancestor in ancestors.iter().rev() {
        if ancestor.scope_boundary() {
            root = ancestor;

            break;
        }
    }

    for descedant in root.descendants() {
        match descedant {
            AstNode::Variable(name) => suggestions.push(Suggestion::new(&name.to_string(), "")),
            _ => (),
        }
    }

    suggestions
}

fn suggest_member(
    referenced_node: NodeId,
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
) -> Vec<Suggestion> {
    let mut suggestions = Vec::new();

    let resolved_object_variable = arena[referenced_node].get();
    eprintln!(
        "Found the reference: {} ({:?})",
        resolved_object_variable.name, resolved_object_variable.kind
    );

    // Direct children ($this, parent, self ...)
    if resolved_object_variable.kind == PhpSymbolKind::Class {
        suggestions.extend(members_of_parents(referenced_node, &arena, &global_symbols).drain(0..));

        referenced_node.children(&arena).for_each(|child| {
            suggestions.push(Suggestion::from(arena[child].get()));
        });
        eprintln!("Reference is a class, done");

        return suggestions;
    }

    // Children of data types with nodes
    resolved_object_variable
        .data_types
        .iter()
        .filter_map(|dt| dt.node)
        .for_each(|node: NodeId| {
            eprintln!("Reference has a node");
            for child in node.children(&arena) {
                suggestions.extend(suggest_member(child, &arena, &global_symbols).drain(0..));
            }
        });

    // Children of data types that are merely type refs. Jump to the type ref and find its reference
    resolved_object_variable
        .data_types
        .iter()
        .filter_map(|dt| dt.type_ref.clone())
        .for_each(|type_ref| {
            eprintln!("Reference has a type ref");

            let mut resolver = NameResolver::new(&global_symbols, referenced_node);
            if let Some(node) = resolver.resolve_type_ref(&type_ref, &arena, &referenced_node) {
                suggestions.extend(members_of_parents(node, &arena, &global_symbols).drain(0..));
            }
        });

    suggestions
}

/// Suggest members of the referenced node and its parents
fn members_of_parents(
    reference_node: NodeId,
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
) -> Vec<Suggestion> {
    let mut suggestions = Vec::new();

    let dt_class = arena[reference_node].get();

    let parents = dt_class.inherits_from.clone();
    for parent in parents {
        if let Some(type_ref) = parent.type_ref.as_ref() {
            let mut resolver = NameResolver::new(&global_symbols, reference_node);

            if let Some(resolved_parent) =
                resolver.resolve_type_ref(type_ref, &arena, &reference_node)
            {
                resolved_parent.children(&arena).for_each(|child| {
                    let child_symbol = arena[child].get();

                    suggestions.push(Suggestion::from(child_symbol));
                });

                suggestions
                    .extend(members_of_parents(resolved_parent, arena, global_symbols).drain(0..));
            }
        }
    }

    suggestions
}

pub fn get_suggestions_at(
    trigger: Option<char>,
    mut pos: Position,
    ast: &Vec<AstNode>,
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
    references: &Vec<Reference>,
) -> Vec<Suggestion> {
    if let Some('>') = trigger {
        pos.character -= 1;
    }

    let (node, mut ancestors) = if let Some((node, ancestors)) =
        ast.iter().filter_map(|n| find(n, &pos, Vec::new())).nth(0)
    {
        (node, ancestors)
    } else {
        return Vec::new();
    };

    let mut suggestions = Vec::new();

    match node {
        AstNode::Missing(..) | AstNode::Literal(..) => {
            // Pop off the last item which is the node itself
            ancestors.pop();

            if let Some(parent) = ancestors.pop() {
                let mut range = parent.range();

                // Find the parent reference
                if let AstNode::Member { object, .. } = parent {
                    if let AstNode::Call { callee, .. } = &**object {
                        if let AstNode::Member { member, .. } = &**callee {
                            range = member.range();
                        }
                    }
                }
                let pos = Position {
                    line: range.0 .0 as u64,
                    character: range.0 .1 as u64,
                };

                for reference in references {
                    if in_range(&pos, &get_range(reference.range)) {
                        suggestions.extend(
                            suggest_member(reference.node, &arena, &global_symbols).drain(0..),
                        );

                        break;
                    }
                }
            }
        }
        _ => match trigger {
            Some('$') => suggestions.extend(suggest_variables(ancestors)),
            Some('>') => (),
            None => {
                suggestions.extend(suggest_variables(ancestors));
                suggestions.extend(
                    global_symbols
                        .iter()
                        .map(|(key, value)| arena[*value].get())
                        .map(Suggestion::from)
                        .collect::<Vec<Suggestion>>(),
                )
            }
            _ => suggestions.extend(
                global_symbols
                    .iter()
                    .map(|(key, value)| arena[*value].get())
                    .map(Suggestion::from)
                    .collect::<Vec<Suggestion>>(),
            ),
        },
    }

    suggestions
}

#[cfg(test)]
mod tests {
    use crate::environment::symbol::{PhpSymbolKind, Symbol};

    use super::super::backend::NodeMapMutex;
    use super::super::parser::scanner::Scanner;
    use super::super::parser::Parser;

    #[test]
    fn test_suggest_members_of_class_and_its_parents() {
        let animal = Symbol {
            kind: PhpSymbolKind::Class,
            name: "Cat".to_owned(),
            ..Symbol::default()
        };
    }
}
