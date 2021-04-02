use crate::{
    backend::FileReferenceMap,
    environment::{
        symbol::{PhpSymbolKind, Symbol, Visibility},
        visitor::name_resolver::NameResolver,
    },
    parser::{
        node::NodeRange,
        token::{Token, TokenType},
    },
};
use crate::{environment::get_range, environment::in_range, parser::node::Node as AstNode};
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use tower_lsp::lsp_types::Position;

#[derive(Debug, PartialEq, Eq)]
pub enum SuggestionContext {
    /// When suggesting a symbol in the use blocks
    Import,

    /// When refering to a symbol and the context is not known
    Unknown,

    /// When calling new
    ConstructorCall,

    /// When simply calling a function, method or property
    Call,

    /// When using a variable
    Reference,

    /// When the suggestion is just a keyword
    Keyword,
}

#[derive(Debug)]
pub struct Suggestion {
    pub node: Option<NodeId>,
    pub token: Option<TokenType>,
    pub context: SuggestionContext,
    pub replace: Option<NodeRange>,
}

impl Suggestion {
    pub fn token(token: TokenType, replace: Option<NodeRange>) -> Self {
        Self {
            token: Some(token),
            node: None,
            context: SuggestionContext::Keyword,
            replace,
        }
    }

    pub fn node(node: NodeId, role: SuggestionContext, replace: Option<NodeRange>) -> Self {
        Self {
            token: None,
            node: Some(node),
            context: role,
            replace,
        }
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
    //eprintln!(
    //    "Found the reference: {} ({:?})",
    //    resolved_object_variable.name, resolved_object_variable.kind
    //);

    // Direct children ($this, parent, self ...)
    if resolved_object_variable.kind == PhpSymbolKind::Class
        || resolved_object_variable.kind == PhpSymbolKind::Interface
    {
        suggestions
            .extend(members_of_parents_of(referenced_node, &arena, &global_symbols).drain(..));

        referenced_node.children(&arena).for_each(|child| {
            suggestions.push(child);
        });
        //eprintln!("Reference is a class or interface, done");

        return suggestions;
    }

    // Children of data types with nodes
    resolved_object_variable
        .data_types
        .iter()
        .filter_map(|dt| dt.node)
        .for_each(|node: NodeId| {
            //eprintln!("Reference has a node");
            suggestions.extend(members_of(node, &arena, &global_symbols).drain(..));
        });

    // Children of data types that are merely type refs. Jump to the type ref and find its reference
    resolved_object_variable
        .data_types
        .iter()
        .filter_map(|dt| dt.type_ref.as_ref())
        .for_each(|type_ref| {
            //eprintln!("Reference has a type ref");

            let mut resolver = NameResolver::new(&global_symbols, referenced_node);

            if let Some(node) = resolver.resolve_type_ref(type_ref, &arena, &referenced_node, false)
            {
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
    let mut resolver = NameResolver::new(&global_symbols, reference_node);

    arena[reference_node]
        .get()
        .get_inherited_symbols(&reference_node, &mut resolver, arena)
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
    pos: Position,
    symbol_under_cursor: NodeId,
    ast: &Vec<AstNode>,
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
    references: &FileReferenceMap,
) -> Vec<Suggestion> {
    let mut no_magic_const = false;

    let (node, mut ancestors) = if let Some((node, mut ancestors)) =
        ast.iter().filter_map(|n| find(n, &pos, Vec::new())).nth(0)
    {
        // Pop off the last item which is the node itself
        ancestors.pop();
        (node, ancestors)
    } else if let Some('$') = trigger {
        // Maybe the $ is the last char of the entire source
        return symbol_under_cursor
            .children(arena)
            .filter_map(|node| {
                if arena[node].get().kind == PhpSymbolKind::Variable {
                    Some(Suggestion::node(node, SuggestionContext::Reference, None))
                } else {
                    None
                }
            })
            .collect();
    } else {
        return global_symbols
            .iter()
            .map(|(_, node)| Suggestion::node(*node, SuggestionContext::Unknown, None))
            .collect();
    };

    let parent = ancestors.pop();

    let mut built_in_references = FileReferenceMap::new();

    match node {
        AstNode::GroupedUse {
            parent: declaration,
            ..
        }
        | AstNode::UseDeclaration { declaration, .. } => {
            let prefix = declaration.normalized_name();

            return global_symbols
                .iter()
                .filter_map(|(key, value)| {
                    if key.starts_with(&prefix) {
                        Some(Suggestion::node(
                            *value,
                            SuggestionContext::Import,
                            Some(declaration.range()),
                        ))
                    } else {
                        None
                    }
                })
                .collect();
        }
        AstNode::Block { .. } => match parent {
            Some(AstNode::ClassStatement { .. }) => {
                return vec![
                    Suggestion::token(TokenType::Public, None),
                    Suggestion::token(TokenType::Private, None),
                    Suggestion::token(TokenType::Protected, None),
                ];
            }
            _ => (),
        },
        AstNode::Variable(..) | AstNode::AliasedVariable { .. } => {
            if let Some(parent_scope) = arena[symbol_under_cursor].parent() {
                return parent_scope
                    .children(arena)
                    .filter_map(|node| {
                        if arena[node].get().kind == PhpSymbolKind::Variable {
                            Some(Suggestion::node(node, SuggestionContext::Reference, None))
                        } else {
                            None
                        }
                    })
                    .collect();
            }
        }
        AstNode::Missing(..) | AstNode::Literal(..) | AstNode::Member { .. } => {
            match node {
                AstNode::Member { object, .. } => {
                    match object.as_ref() {
                        AstNode::Variable(name) => {
                            // do something like
                            // arena[scope_under_cursor].get().resolve(name).children()
                            if name.label == Some(String::from("this")) {
                                if let Some(parent_class) = arena[symbol_under_cursor].parent() {
                                    built_in_references
                                        .entry(parent_class)
                                        .or_insert_with(Vec::new)
                                        .push(object.range());
                                }
                            }
                        }
                        _ => (),
                    }
                }
                _ => (),
            }

            if let Some(parent) = parent {
                let mut range = parent.range();

                // Find the reference for the parent, i.e. the $object in $object->|
                if let AstNode::Member { object, .. } = parent {
                    no_magic_const = true;
                    if let AstNode::Call { callee, .. } = object.as_ref() {
                        if let AstNode::Member { member, .. } = callee.as_ref() {
                            range = member.range();
                        }
                    } else if let AstNode::Variable(token) = object.as_ref() {
                        if Some(String::from("this")) == token.label {
                            if let Some(parent_class) = arena[symbol_under_cursor].parent() {
                                built_in_references
                                    .entry(parent_class)
                                    .or_insert_with(Vec::new)
                                    .push(range);
                            }
                        }
                    }
                }

                let pos = Position {
                    line: range.0 .0 as u64,
                    character: range.0 .1 as u64,
                };

                for (node, ranges) in references.iter().chain(built_in_references.iter()) {
                    if ranges
                        .iter()
                        .find(|r| in_range(&pos, &get_range(**r)))
                        .is_none()
                    {
                        continue;
                    }

                    // Data types of the parent reference. Go through all data types and collect the things
                    // that are visibile from the current class

                    let current_class = symbol_under_cursor.ancestors(&arena).find(|n| {
                        let s = arena[*n].get();

                        return s.kind == PhpSymbolKind::Class;
                    });

                    // Collect a list of all accessible members of this class and its parents
                    let mut accessible_members = Vec::new();

                    if let Some(current_class) = current_class {
                        accessible_members.extend(current_class.children(&arena));

                        let mut resolver = NameResolver::new(&global_symbols, current_class);

                        accessible_members.extend(
                            arena[current_class]
                                .get()
                                .get_inherited_symbols(&current_class, &mut resolver, &arena)
                                .iter()
                                .filter(|n| arena[**n].get().visibility >= Visibility::Protected),
                        );
                    }

                    let mut resolver = NameResolver::new(&global_symbols, symbol_under_cursor);

                    let mut suggestions: Vec<Suggestion> = Vec::new();
                    let static_only = Some(':') == trigger;
                    arena[*node]
                        .get()
                        .data_types
                        .iter()
                        .filter_map(|dt_reference| {
                            if let Some(type_ref) = dt_reference.type_ref.as_ref() {
                                if let Some(tr) = dt_reference.type_ref.as_ref() {
                                    if let Some(first) = tr.first() {
                                        if let Some(label) = first.label.as_ref() {
                                            if label == "$this" {
                                                return arena[*node].parent();
                                            }
                                        }
                                    }
                                }
                                if let Some(referenced_node) = resolver.resolve_type_ref(
                                    &type_ref,
                                    arena,
                                    &symbol_under_cursor,
                                    false,
                                ) {
                                    return Some(referenced_node);
                                }

                                return None;
                            }
                            return dt_reference.node;
                        })
                        .for_each(|node| {
                            suggestions.extend(
                                members_of(node, &arena, &global_symbols)
                                    .drain(..)
                                    .filter_map(|n| {
                                        let s = arena[n].get();

                                        if static_only && !s.is_static
                                            || no_magic_const && s.kind == PhpSymbolKind::MagicConst
                                        {
                                            return None;
                                        }

                                        // Either the element is accessible from this scope anyway or its public ...
                                        if s.visibility >= Visibility::Public
                                            || accessible_members.contains(&n)
                                        {
                                            Some(Suggestion::node(n, SuggestionContext::Call, None))
                                        } else {
                                            None
                                        }

                                        // ... or we ignore it
                                    })
                                    .collect::<Vec<Suggestion>>(),
                            );
                        });

                    return suggestions;
                }
            }
        }
        _ => {
            return global_symbols
                .iter()
                .filter_map(|(_key, value)| {
                    if arena[*value]
                        .get()
                        .normalized_name()
                        .starts_with(&node.normalized_name())
                    {
                        Some(Suggestion::node(
                            *value,
                            SuggestionContext::Unknown,
                            Some(node.range()),
                        ))
                    } else {
                        None
                    }
                })
                .collect();
        }
    }

    //suggestions.dedup_by(|a, b| arena[*a].get().name == arena[*b].get().name);

    vec![]
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::super::parser::token::*;
    use crate::{
        backend::Backend,
        backend::BackendState,
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
    use tower_lsp::lsp_types::Position;

    macro_rules! variable {
        ($arena:expr, $name:expr, $node: expr) => {{
            let s = Symbol {
                kind: PhpSymbolKind::Variable,
                name: $name.to_string(),
                data_types: vec![Reference::node(
                    &[Token::named(TokenType::Variable, 0, 0, $name)],
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
                    inherits_from: Some(vec![Reference::type_ref(vec![Token::named(
                        TokenType::Identifier,
                        0,
                        0,
                        $parent,
                    )])]),
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

    fn suggestions(
        sources: &[(&str, &str)],
        file: usize,
        line: u64,
        character: u64,
        tc: Option<char>,
    ) -> Vec<String> {
        let mut state = BackendState::default();

        for (file, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file, &pr.0, &get_range(dr), &mut state).unwrap();
            Backend::collect_references(*file, &pr.0, &mut state, None).unwrap();
        }
        let mut scanner = Scanner::new(&sources[file].1);
        scanner.scan().unwrap();
        let pr = Parser::ast(scanner.tokens).unwrap();
        let pos = Position { line, character };

        let references = state.symbol_references.get(sources[file].0).unwrap();
        let suc = state.files.get(sources[file].0).unwrap();
        let file = state.arena[*suc].get();
        let mut suggestions = super::get_suggestions_at(
            tc,
            pos,
            file.symbol_at(&pos, *suc, &state.arena),
            &pr.0,
            &state.arena,
            &state.global_symbols,
            &references,
        );

        suggestions
            .drain(..)
            .map(|n| {
                if let Some(node) = n.node {
                    state.arena[node].get().fqdn()
                } else {
                    n.token.unwrap().to_string()
                }
            })
            .collect::<Vec<String>>()
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
        global_symbols.insert("animal".to_string(), animal_node);
        global_symbols.insert("cat".to_string(), cat_node);

        let actual = super::members_of(cat_instance_node, &arena, &global_symbols);

        assert_eq!(4, actual.len());
        assert_eq!(am1_node, actual[0]);
        assert_eq!(am2_node, actual[1]);
        assert_eq!(cm1_node, actual[2]);
        assert_eq!(cm2_node, actual[3]);
    }

    #[tokio::test]
    async fn test_suggests_accessible_members_of_class_and_its_parents() {
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
                "aniinterface.php",
                "<?php namespace App; interface AniInter { public function getName() { } }",
            ),
            (
                "cat.php",
                "<?php namespace App; class Cat extends Animal implements AniInter { public function getName() { } }",
            ),
            ("index.php", "<?php use App\\Cat; $cat = new Cat(); $cat->"),
            ("index2.php", "<?php use App\\AniInter; function x(AniInter $y) {$y->}"),
        ];

        let actual = suggestions(&sources, 4, 0, 43, Some('>'));
        assert_eq!("getPulse", actual[0]);
        assert_eq!("getName", actual[1]);

        let actual = suggestions(&sources, 5, 0, 54, Some('>'));
        assert!(actual.contains(&&"getName".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_variables() {
        let sources = vec![("index.php", "<?php $cat = 'Marci'; $")];

        let actual = suggestions(&sources, 0, 0, 22, Some('$'));
        assert!(actual.contains(&&"cat".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_global_symbols() {
        let sources = vec![
            (
                "stdlib.php",
                "<?php function array_a() {} function array_b() {} function strpos() {}",
            ),
            ("index.php", "<?php array_"),
        ];

        let actual = suggestions(&sources, 1, 0, 12, None);
        assert!(actual.contains(&&"array_a".to_string()));
        assert!(actual.contains(&&"array_b".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_members_of_this() {
        let sources = vec![(
            "animal.php",
            "<?php class Animal { private $name; public function getName() { $this-> }}",
        )];

        let actual = suggestions(&sources, 0, 0, 71, Some('>'));

        assert!(actual.contains(&&"getName".to_string()));
        assert!(actual.contains(&&"name".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_class_after_new() {
        let sources = vec![
            ("animal.php", "<?php class Animal { } function x() {}"),
            ("index.php", "<?php Animal::"),
        ];

        let actual = suggestions(&sources, 1, 0, 14, None);

        assert!(actual.contains(&&"class".to_string()));
    }

    #[test]
    fn test_suggests_method_modifiers_in_class_body() {
        let sources = [
            ("animal.php", "<?php class Animal {  } "),
            ("animal2.php", "<?php class Animal { p } "),
        ];
        let actual = suggestions(&sources, 0, 0, 21, None);

        assert!(actual.contains(&"public".to_string()));
        assert!(actual.contains(&"private".to_string()));
        assert!(actual.contains(&"protected".to_string()));
        assert_eq!(3, actual.len());

        let actual = suggestions(&sources, 1, 0, 22, None);

        assert!(actual.contains(&"public".to_string()));
        assert!(actual.contains(&"private".to_string()));
        assert!(actual.contains(&"protected".to_string()));
        assert_eq!(3, actual.len());
    }

    #[tokio::test]
    async fn test_suggests_namespaces_after_use_on_file_level() {
        let sources = vec![
            ("index.php", "<?php use "),
            ("index2.php", "<?php use App\\"),
            ("index3.php", "<?php use App\\Living\\"),
            (
                "animal.php",
                "<?php namespace App\\Living; class Animal { }",
            ),
            ("car.php", "<?php namespace App\\Inanimate; class Car { }"),
        ];

        let actual = suggestions(&sources, 0, 0, 9, Some(' '));

        assert!(actual.contains(&&"App\\Living\\Animal".to_string()));
        assert!(actual.contains(&&"App\\Inanimate\\Car".to_string()));

        let actual = suggestions(&sources, 1, 0, 15, Some('\\'));
        assert!(actual.contains(&&"App\\Living\\Animal".to_string()));
        assert!(actual.contains(&&"App\\Inanimate\\Car".to_string()));

        let actual = suggestions(&sources, 2, 0, 23, Some('\\'));
        assert!(actual.contains(&&"App\\Living\\Animal".to_string()));
    }
}
