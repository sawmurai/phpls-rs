use crate::environment::{
    symbol::{PhpSymbolKind, Symbol, Visibility},
    visitor::name_resolver::{NameResolver, Reference},
};
use crate::{environment::get_range, environment::in_range, parser::node::Node as AstNode};
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use tower_lsp::lsp_types::Position;

#[derive(Debug)]
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

/// Collect the members of a class
///
/// * referenced_node: The node representing the class whose members to collect
/// * arena: A reference to the memory arena
/// * global_symbols: The lookup table of globally registered symbols
fn members_of(
    referenced_node: NodeId,
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
) -> Vec<NodeId> {
    let mut suggestions = Vec::new();
    let resolved_object_variable = arena[referenced_node].get();
    eprintln!(
        "Found the reference: {} ({:?})",
        resolved_object_variable.name, resolved_object_variable.kind
    );

    // Direct children ($this, parent, self ...)
    if resolved_object_variable.kind == PhpSymbolKind::Class {
        suggestions
            .extend(members_of_parents_of(referenced_node, &arena, &global_symbols).drain(..));

        referenced_node.children(&arena).for_each(|child| {
            suggestions.push(child);
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
            suggestions.extend(members_of(node, &arena, &global_symbols).drain(..));
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
                suggestions.extend(members_of_parents_of(node, &arena, &global_symbols).drain(..));
            }
        });

    suggestions
}

/// Collect the members of a classes ancestors
///
/// * referenced_node: The node representing the class whose members to collect
/// * arena: A reference to the memory arena
/// * global_symbols: The lookup table of globally registered symbols
fn members_of_parents_of(
    reference_node: NodeId,
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
) -> Vec<NodeId> {
    let mut suggestions = Vec::new();

    let dt_class = arena[reference_node].get();

    let mut resolver = NameResolver::new(&global_symbols, reference_node);
    if let Some(parent) = dt_class.get_parent_node(&reference_node, &mut resolver, arena) {
        eprintln!("which could be resolved");
        parent.children(&arena).for_each(|child| {
            suggestions.push(child);
        });

        suggestions.extend(members_of_parents_of(parent, arena, global_symbols).drain(..));
    }

    suggestions
}

/// Get suggestions for a given position in the code
///
/// * trigger: Optionally, a character that triggered the completion request
/// * pos: The cursor position
/// * symbol_under_cursor: The current symbol under the cursor. Can be a method body, a class body, a file or a variable
/// * ast: A full AST of the current source. Required since the symbols are only stored in a reduced manner
/// * arena: A reference to the memory arena storing all the symbols
/// * global_symbols: A lookup table to globally available symbols
/// * references: Collected references in the current document
pub fn get_suggestions_at(
    trigger: Option<char>,
    mut pos: Position,
    symbol_under_cursor: NodeId,
    ast: &Vec<AstNode>,
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
    references: &Vec<Reference>,
) -> Vec<NodeId> {
    if let Some('>') = trigger {
        pos.character -= 1;
    }

    let (node, mut ancestors) = if let Some((node, mut ancestors)) =
        ast.iter().filter_map(|n| find(n, &pos, Vec::new())).nth(0)
    {
        // Pop off the last item which is the node itself
        ancestors.pop();
        (node, ancestors)
    } else {
        eprintln!("Did not find the node");
        return Vec::new();
    };

    let mut suggestions = Vec::new();

    let parent = ancestors.pop();

    eprintln!(">>> {:?}", node);

    match node {
        AstNode::Missing(..) | AstNode::Literal(..) => {
            if let Some(parent) = parent {
                let mut range = parent.range();

                // Find the reference for the parent, i.e. the $object in $object->|
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

                eprintln!("Searching for references at {:?}", range.0);

                for reference in references {
                    if in_range(&pos, &get_range(reference.range)) {
                        let current_class = symbol_under_cursor.ancestors(&arena).find(|n| {
                            let s = arena[*n].get();

                            return s.kind == PhpSymbolKind::Class;
                        });

                        // Collect a list of all accessible members of this class and its parents
                        let mut accessible_members = Vec::new();
                        if let Some(current_class) = current_class {
                            accessible_members.extend(current_class.children(&arena));

                            let mut current_parent = current_class;
                            loop {
                                let mut resolver =
                                    NameResolver::new(&global_symbols, current_parent);
                                if let Some(parent) = arena[current_parent].get().get_parent_node(
                                    &current_parent,
                                    &mut resolver,
                                    &arena,
                                ) {
                                    accessible_members.extend(parent.children(&arena).filter(
                                        |n| arena[*n].get().visibility >= Visibility::Protected,
                                    ));

                                    current_parent = parent;
                                } else {
                                    break;
                                }
                            }
                        }

                        suggestions.extend(
                            members_of(reference.node, &arena, &global_symbols)
                                .drain(..)
                                .filter(|n| {
                                    let s = arena[*n].get();

                                    // Either the element is accessible from this scope anyway or its public ...
                                    s.visibility >= Visibility::Public
                                        || accessible_members.contains(&n)

                                    // ... or we ignore it
                                })
                                .collect::<Vec<NodeId>>(),
                        );

                        break;
                    }
                }
            }
        }
        _ => match trigger {
            //Some('$') => suggestions.extend(suggest_variables(ancestors)),
            Some('>') => (),
            None => {
                // suggestions.extend(suggest_variables(ancestors));
                suggestions.extend(
                    global_symbols
                        .iter()
                        .map(|(key, value)| value.to_owned())
                        .collect::<Vec<NodeId>>(),
                )
            }
            _ => suggestions.extend(
                global_symbols
                    .iter()
                    .map(|(key, value)| value.to_owned())
                    .collect::<Vec<NodeId>>(),
            ),
        },
    }

    suggestions
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::super::parser::token::*;
    use crate::{
        backend::Backend,
        environment::{
            get_range,
            import::SymbolImport,
            scope::Reference,
            symbol::{PhpSymbolKind, Symbol, Visibility},
        },
        parser,
    };
    use indextree::Arena;
    use parser::{scanner::Scanner, Parser};
    use std::sync::Arc;
    use tokio::sync::Mutex;
    use tower_lsp::lsp_types::Position;

    macro_rules! variable {
        ($arena:expr, $name:expr, $node: expr) => {{
            let s = Symbol {
                kind: PhpSymbolKind::Variable,
                name: $name.to_string(),
                data_types: vec![Reference::node(
                    &Token::named(TokenType::Variable, 0, 0, $name),
                    $node,
                )],
                ..Symbol::default()
            };

            $arena.new_node(s)
        }};
    }

    macro_rules! child {
        ($arena:expr, $type: expr, $name:expr, $vis:expr) => {{
            let s = Symbol {
                kind: PhpSymbolKind::Method,
                name: $name.to_string(),
                visibility: $vis,
                ..Symbol::default()
            };

            $arena.new_node(s)
        }};
    }

    macro_rules! class {
        ($arena:expr, $name:expr, $parent:expr) => {
            if $parent == "" {
                let s = Symbol {
                    kind: PhpSymbolKind::Class,
                    name: $name.to_string(),
                    inherits_from: None,
                    ..Symbol::default()
                };

                $arena.new_node(s)
            } else {
                let s = Symbol {
                    kind: PhpSymbolKind::Class,
                    name: $name.to_string(),
                    inherits_from: Some(Reference::type_ref(vec![Token::named(
                        TokenType::Identifier,
                        0,
                        0,
                        $parent,
                    )])),
                    ..Symbol::default()
                };

                $arena.new_node(s)
            }
        };
    }

    macro_rules! file {
        ($arena: expr, $name:expr$(, $imports:expr)?) => {{
            let _imports: Option<Vec<SymbolImport>> = None;
            $(
                let _imports = Some($imports
                        .iter()
                        .map(|i| SymbolImport {
                            path: vec![Token::named(TokenType::Identifier, 0, 0, &i)],
                            alias: None,
                        })
                        .collect::<Vec<SymbolImport>>());
            )?

            let s = Symbol {
                kind: PhpSymbolKind::File,
                name: $name.to_string(),
                inherits_from: None,
                imports: _imports,
                ..Symbol::default()
            };

            $arena.new_node(s)
        }};
    }

    #[test]
    fn test_collects_members_of_class_and_its_parents() {
        let mut arena = Arena::new();

        let animal_file_node = file!(arena, "Animal.php");
        let animal_node = class!(arena, "Animal", "");
        let am1_node = child!(arena, PhpSymbolKind::Method, "getName", Visibility::Public);
        animal_node.append(am1_node, &mut arena);

        let am2_node = child!(
            arena,
            PhpSymbolKind::Method,
            "getInternal",
            Visibility::Private
        );
        animal_node.append(am2_node, &mut arena);

        animal_file_node.append(animal_node, &mut arena);

        let cat_file_node = file!(arena, "Cat.php", ["Animal"]);
        let cat_node = class!(arena, "Cat", "Animal");
        cat_file_node.append(cat_node, &mut arena);

        let cm1_node = child!(
            arena,
            PhpSymbolKind::Method,
            "getCatType",
            Visibility::Public
        );
        cat_node.append(cm1_node, &mut arena);
        let cm2_node = child!(
            arena,
            PhpSymbolKind::Method,
            "getPrivateCatType",
            Visibility::Private
        );
        cat_node.append(cm2_node, &mut arena);

        let cat_instance_node = variable!(arena, "$cat", cat_node);
        cat_file_node.append(cat_instance_node, &mut arena);

        let mut global_symbols = HashMap::new();
        global_symbols.insert("Animal".to_string(), animal_node);
        global_symbols.insert("Cat".to_string(), cat_node);

        let actual = super::members_of(cat_instance_node, &arena, &global_symbols);

        assert_eq!(4, actual.len());
        assert_eq!(am1_node, actual[0]);
        assert_eq!(am2_node, actual[1]);
        assert_eq!(cm1_node, actual[2]);
        assert_eq!(cm2_node, actual[3]);
    }

    #[tokio::test]
    async fn test_suggests_accessible_members_of_class_and_its_parents() {
        let arena = Arc::new(Mutex::new(Arena::new()));
        let global_symbols = Arc::new(Mutex::new(HashMap::new()));
        let files = Arc::new(Mutex::new(HashMap::new()));
        let diagnostics = Arc::new(Mutex::new(HashMap::new()));
        let symbol_references = Arc::new(Mutex::new(HashMap::new()));

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
                "<?php namespace App; class Cat extends Animal { public function getName() { } }",
            ),
            ("index.php", "<?php use App\\Cat; $cat = new Cat(); $cat->"),
        ];

        for (resolve, collect) in [(false, true), (true, false)].iter() {
            for (file, source) in sources.iter() {
                let mut scanner = Scanner::new(*source);
                scanner.scan().unwrap();

                let dr = scanner.document_range();
                let pr = Parser::ast(scanner.tokens).unwrap();

                Backend::reindex(
                    *file,
                    &pr.0,
                    &get_range(dr),
                    *resolve,
                    *collect,
                    arena.clone(),
                    global_symbols.clone(),
                    symbol_references.clone(),
                    files.clone(),
                    diagnostics.clone(),
                )
                .await
                .unwrap();
            }
        }

        let files = files.lock().await;
        let arena = arena.lock().await;
        let global_symbols = global_symbols.lock().await;
        let symbol_references = symbol_references.lock().await;
        let mut scanner = Scanner::new(&sources[3].1);
        scanner.scan().unwrap();
        let pr = Parser::ast(scanner.tokens).unwrap();
        let pos = Position {
            line: 0,
            character: 43,
        };
        let references = symbol_references.get("index.php").unwrap();
        let suc = files.get("index.php").unwrap();
        let actual = super::get_suggestions_at(
            Some('>'),
            pos,
            *suc,
            &pr.0,
            &arena,
            &global_symbols,
            references,
        );

        assert_eq!("getPulse", arena[actual[0]].get().name);
        assert_eq!("getName", arena[actual[1]].get().name);
    }
}
