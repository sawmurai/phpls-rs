use crate::{
    backend::FileReferenceMap,
    environment::{
        symbol::{PhpSymbolKind, Symbol, SymbolAlias, Visibility},
        visitor::name_resolver::NameResolver,
    },
    parser::{node::NodeRange, token::TokenType},
};
use crate::{environment::get_range, environment::in_range, parser::node::Node as AstNode};
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use tower_lsp::lsp_types::Position;

#[derive(Debug, PartialEq, Eq)]
pub enum SuggestionContext {
    /// When suggesting a symbol in the use blocks
    Import,

    /// When using a trait
    TraitUsage,

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
    pub alias: Option<String>,
    pub is_this: bool,
}

impl Suggestion {
    pub fn token(token: TokenType, replace: Option<NodeRange>) -> Self {
        Self {
            token: Some(token),
            node: None,
            context: SuggestionContext::Keyword,
            replace,
            alias: None,
            is_this: false,
        }
    }

    pub fn node(node: NodeId, role: SuggestionContext, replace: Option<NodeRange>) -> Self {
        Self {
            token: None,
            node: Some(node),
            context: role,
            replace,
            alias: None,
            is_this: false,
        }
    }

    pub fn aliased(
        node: NodeId,
        alias: &str,
        role: SuggestionContext,
        replace: Option<NodeRange>,
    ) -> Self {
        Self {
            token: None,
            node: Some(node),
            context: role,
            replace,
            alias: Some(alias.to_owned()),
            is_this: false,
        }
    }

    pub fn this(node: NodeId, role: SuggestionContext) -> Self {
        Self {
            token: None,
            node: Some(node),
            context: role,
            replace: None,
            alias: Some(String::from("this")),
            is_this: true,
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

/// Get imports that start with the prefix of
fn suggest_imports_starting_with(
    global_symbols: &HashMap<String, NodeId>,
    declaration: &AstNode,
) -> Vec<Suggestion> {
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

/// Get traits that start with the prefix of
fn suggest_traits_starting_with(
    global_symbols: &HashMap<String, NodeId>,
    declaration: &AstNode,
    arena: &Arena<Symbol>,
    node: &AstNode,
) -> Vec<Suggestion> {
    let prefix = declaration.normalized_name();

    return global_symbols
        .iter()
        .filter_map(|(_, value)| {
            let symbol = arena[*value].get();

            if symbol.kind != PhpSymbolKind::Trait {
                return None;
            }

            if symbol.normalized_name().starts_with(&prefix) {
                Some(Suggestion::node(
                    *value,
                    SuggestionContext::TraitUsage,
                    Some(node.range()),
                ))
            } else {
                None
            }
        })
        .collect();
}

/// Get imports that start with the prefix of
fn suggest_symbol_starting_with(
    global_symbols: &HashMap<String, NodeId>,
    arena: &Arena<Symbol>,
    node: &AstNode,
) -> Vec<Suggestion> {
    let prefix = node.normalized_name();
    return global_symbols
        .iter()
        .filter_map(|(_, value)| {
            if arena[*value].get().normalized_name().starts_with(&prefix) {
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

/// Suggest variables (and function parameters) in the scope
fn suggest_variables_of_scope(scope: NodeId, arena: &Arena<Symbol>) -> Vec<Suggestion> {
    let mut suggestions: Vec<Suggestion> = scope
        .children(arena)
        .filter_map(|node| {
            let kind = arena[node].get().kind;
            if kind == PhpSymbolKind::Variable || kind == PhpSymbolKind::FunctionParameter {
                Some(Suggestion::node(node, SuggestionContext::Reference, None))
            } else {
                None
            }
        })
        .collect();

    if let Some(parent_class) = scope
        .ancestors(arena)
        .find(|node| arena[*node].get().kind == PhpSymbolKind::Class)
    {
        suggestions.push(Suggestion::this(parent_class, SuggestionContext::Reference));
    }

    suggestions
}

/// Suggest members (node) of the symbol (parent)
fn suggest_members_of_symbol(
    trigger: Option<char>,
    node: &AstNode,
    parent: Option<&AstNode>,
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
    symbol_under_cursor: NodeId,
    references: &FileReferenceMap,
) -> Vec<Suggestion> {
    let mut no_magic_const = false;

    let parent = if let Some(parent) = parent {
        parent
    } else {
        return vec![];
    };

    let prefix = node.name();
    let mut parent_range = parent.range();

    // Find the reference for the parent, i.e. the $object in $object->|
    // If a $this is encountered it we already found the parent
    let mut resolved_parent = None;
    if let AstNode::Member { object, .. } = node {
        parent_range = object.range();
        no_magic_const = true;
    } else if let AstNode::Member { object, .. } = parent {
        no_magic_const = true;
        if let AstNode::Call { callee, .. } = object.as_ref() {
            if let AstNode::Member { member, .. } = callee.as_ref() {
                parent_range = member.range();
            }
        } else if let AstNode::Member { member, .. } = object.as_ref() {
            parent_range = member.range();
        } else if let AstNode::Variable(token) = object.as_ref() {
            if Some(String::from("this")) == token.label {
                resolved_parent = arena[symbol_under_cursor].parent();
            }
        }
    }

    let pos = Position {
        line: parent_range.start_line,
        character: parent_range.start_col,
    };

    // Check if there is a reference to the $object in $object->callee by going
    // through all references in the current file and finding a match on the parent position
    let resolved_parent = if let Some(resolved_parent) = resolved_parent.as_ref() {
        resolved_parent
    } else if let Some((node, _)) = references
        .iter()
        .find(|(_, ranges)| ranges.iter().any(|r| in_range(&pos, &get_range(*r))))
    {
        node
    } else {
        return vec![];
    };

    // Data types of the parent reference. Go through all data types and collect the things
    // that are visibile from the current class
    let current_class = symbol_under_cursor.ancestors(arena).find(|n| {
        let s = arena[*n].get();

        s.kind == PhpSymbolKind::Class
    });

    // Collect a list of all accessible members of this class and its parents
    let mut accessible_members = HashMap::new();
    if let Some(current_class) = current_class {
        current_class.children(arena).for_each(|c| {
            let symbol = arena[c].get();
            accessible_members.insert(
                symbol.normalized_name(),
                SymbolAlias::new(c, symbol.name(), symbol.visibility, current_class),
            );
        });

        let cc_symbol = arena[current_class].get();
        let mut resolver = NameResolver::new(global_symbols, current_class);

        accessible_members.extend(cc_symbol.get_imports(current_class, &mut resolver, arena));
        cc_symbol
            .get_inherited_symbols(current_class, &mut resolver, arena)
            .drain()
            .filter(|(_, n)| arena[n.symbol].get().visibility >= Visibility::Protected)
            .for_each(|(name, node)| {
                accessible_members.insert(name, node);
            });
    }

    let mut resolver = NameResolver::new(global_symbols, symbol_under_cursor);

    let mut suggestions: Vec<Suggestion> = Vec::new();
    let static_only = Some(':') == trigger;
    arena[*resolved_parent]
        .get()
        .data_types
        .iter()
        .filter_map(|dt_reference| {
            if dt_reference.node.is_some() {
                return dt_reference.node;
            }

            if "$this" == dt_reference.type_ref.root().unwrap() {
                return arena[*resolved_parent].parent();
            }

            resolver.resolve_type_ref(&dt_reference.type_ref, arena, &symbol_under_cursor, false)
        })
        .for_each(|node| {
            let mut resolver = NameResolver::new(global_symbols, symbol_under_cursor);
            suggestions.extend(
                arena[node]
                    .get()
                    .get_all_symbols(node, &mut resolver, arena)
                    .iter()
                    .filter_map(|(_, n)| {
                        if !n.alias.starts_with(&prefix) {
                            return None;
                        }

                        let s = arena[n.symbol].get();

                        if static_only && !s.is_static
                            || no_magic_const && s.kind == PhpSymbolKind::MagicConst
                        {
                            return None;
                        }

                        // Either the element is accessible from this scope anyway or its public ...
                        if n.visibility >= Visibility::Public
                            || accessible_members.contains_key(&s.normalized_name())
                        {
                            Some(Suggestion::aliased(
                                n.symbol,
                                n.alias,
                                SuggestionContext::Call,
                                None,
                            ))
                        } else {
                            None
                        }

                        // ... or we ignore it
                    })
                    .collect::<Vec<Suggestion>>(),
            );
        });

    suggestions
}

fn suggest_keywords(arena: &Arena<Symbol>, symbol_under_cursor: NodeId) -> Vec<Suggestion> {
    match arena[symbol_under_cursor].get().kind {
        PhpSymbolKind::File => {
            return vec![
                Suggestion::token(TokenType::Namespace, None),
                Suggestion::token(TokenType::Use, None),
                Suggestion::token(TokenType::Class, None),
                Suggestion::token(TokenType::Function, None),
                Suggestion::token(TokenType::Const, None),
                Suggestion::token(TokenType::Trait, None),
                Suggestion::token(TokenType::Interface, None),
            ];
        }
        _ => vec![],
    }
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
    ast: &[AstNode],
    arena: &Arena<Symbol>,
    global_symbols: &HashMap<String, NodeId>,
    references: &FileReferenceMap,
) -> Vec<Suggestion> {
    let (node, mut ancestors) =
        if let Some((node, mut ancestors)) = ast.iter().find_map(|n| find(n, &pos, Vec::new())) {
            // Pop off the last item which is the node itself
            ancestors.pop();
            (node, ancestors)
        } else if let Some('$') = trigger {
            return suggest_variables_of_scope(symbol_under_cursor, arena);
        } else {
            return suggest_keywords(arena, symbol_under_cursor);
        };

    match node {
        AstNode::UseTrait { type_ref, .. } => {
            return suggest_traits_starting_with(global_symbols, type_ref, arena, node);
        }
        AstNode::GroupedUse {
            parent: declaration,
            ..
        }
        | AstNode::UseDeclaration { declaration, .. } => {
            return suggest_imports_starting_with(global_symbols, declaration);
        }
        AstNode::Variable(..) | AstNode::AliasedVariable { .. } => {
            return suggest_variables_of_scope(symbol_under_cursor, arena);
        }
        AstNode::Missing(..) | AstNode::Literal(..) | AstNode::Member { .. } => {
            let parent = Some(*ancestors.last().unwrap());
            return suggest_members_of_symbol(
                trigger,
                node,
                parent,
                arena,
                global_symbols,
                symbol_under_cursor,
                references,
            );
        }
        _ => (),
    }

    let mut is_class_block = false;

    while let Some(ancestor) = ancestors.pop() {
        match ancestor {
            AstNode::ClassStatement { .. } => {
                is_class_block = true;
                break;
            }
            AstNode::Block { .. } => {
                if let Some(AstNode::ClassStatement { .. }) = ancestors.last() {
                    is_class_block = true;
                }

                break;
            }
            _ => (),
        }
    }

    if is_class_block {
        return vec![
            Suggestion::token(TokenType::Public, None),
            Suggestion::token(TokenType::Private, None),
            Suggestion::token(TokenType::Protected, None),
        ];
    }

    suggest_symbol_starting_with(global_symbols, arena, node)
}

#[cfg(test)]
mod tests {
    use crate::{backend::BackendState, parser};
    use parser::{scanner::Scanner, Parser};
    use tower_lsp::lsp_types::Position;

    fn suggestions(
        sources: &[(&str, &str)],
        file: usize,
        line: u32,
        character: u32,
        tc: Option<char>,
    ) -> Vec<String> {
        let mut state = BackendState::default();

        crate::backend::tests::populate_state(&mut state, sources);

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
                if let Some(alias) = n.alias {
                    alias
                } else if let Some(node) = n.node {
                    state.arena[node].get().fqdn()
                } else {
                    n.token.unwrap().to_string()
                }
            })
            .collect::<Vec<String>>()
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
                "<?php namespace App; interface AniInter { public function getName(); }",
            ),
            (
                "cat.php",
                "<?php namespace App; class Cat extends Animal implements AniInter { public function getName() { } }",
            ),
            ("index.php", r"<?php use App\Cat; $cat = new Cat(); $cat->"),
            ("index2.php", r"<?php use App\AniInter; function x(AniInter $y) {$y->}"),
            ("index3.php", r"<?php use App\Living; class T {  /** @var Living */private $thing; public function test() { $this->thing->    } }"),
        ];

        let actual = suggestions(&sources, 4, 0, 43, Some('>'));
        assert!(actual.contains(&&"getPulse".to_string()));
        assert!(actual.contains(&&"getName".to_string()));

        let actual = suggestions(&sources, 5, 0, 53, Some('>'));
        assert!(actual.contains(&&"getName".to_string()));

        let actual = suggestions(&sources, 6, 0, 106, Some('>'));
        assert!(actual.contains(&&"getPulse".to_string()));
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

        assert_eq!(2, actual.len());
        assert!(actual.contains(&&"getName".to_string()));
        assert!(actual.contains(&&"name".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_members_of_this_starting_with() {
        let sources = vec![(
            "animal.php",
            "<?php class Animal { private $name; public function getName() { $this->ge }}",
        )];

        let actual = suggestions(&sources, 0, 0, 73, Some('>'));

        assert_eq!(1, actual.len());
        assert!(actual.contains(&&"getName".to_string()));
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
            ("index2.php", r"<?php use App\"),
            ("index3.php", r"<?php use App\Living\"),
            (
                "animal.php",
                "<?php namespace App\\Living; class Animal { }",
            ),
            ("car.php", "<?php namespace App\\Inanimate; class Car { }"),
        ];

        let actual = suggestions(&sources, 0, 0, 9, Some(' '));

        assert!(actual.contains(&&"App\\Living\\Animal".to_string()));
        assert!(actual.contains(&&"App\\Inanimate\\Car".to_string()));

        let actual = suggestions(&sources, 1, 0, 14, Some('\\'));
        assert!(actual.contains(&&"App\\Living\\Animal".to_string()));
        assert!(actual.contains(&&"App\\Inanimate\\Car".to_string()));

        let actual = suggestions(&sources, 2, 0, 21, Some('\\'));
        assert!(actual.contains(&&"App\\Living\\Animal".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_keywords_in_file() {
        let sources = vec![("index.php", "<?php ")];

        let actual = suggestions(&sources, 0, 0, 6, None);

        assert!(actual.contains(&&"namespace".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_trait_after_use_in_class() {
        let sources = vec![
            ("index.php", "<?php class Test { use T}"),
            ("test.php", "<?php trait TheTrait {}"),
            ("test2.php", "<?php class TheClass {}"),
        ];

        let actual = suggestions(&sources, 0, 0, 24, None);

        assert_eq!(1, actual.len());
        assert!(actual.contains(&&"TheTrait".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_aliased_trait_method_and_ignores_errors() {
        let sources = vec![
            (
                "index.php",
                "<?php use TheClass; $m = new TheClass(); $m-> ",
            ),
            ("test.php", "<?php trait A { public function m1() {} }"),
            ("test4.php", "<?php trait B { public function m1() {} }"),
            (
                "test2.php",
                r"<?php class TheClass {
                    use A, B {
                        A::m1 as private schnggel;
                        A::m1 as public m2;
                        m1 as m1alias;
                        B::m1 as public m3;
                        C::test insteadof A,B;
                    }
                    use A;
                    use B;
                }",
            ),
        ];

        let actual = suggestions(&sources, 0, 0, 45, Some('>'));

        assert_eq!(4, actual.len());
        assert!(actual.contains(&&"m1".to_string()));
        assert!(actual.contains(&&"m1alias".to_string()));
        assert!(actual.contains(&&"m2".to_string()));
        assert!(actual.contains(&&"m3".to_string()));
    }

    #[tokio::test]
    async fn test_suggests_within_array_field() {
        let sources = vec![(
            "index.php",
            "<?php class TheClass { public function x () { $lol = 1; $this->prop[$] = 1; }} ",
        )];

        let actual = suggestions(&sources, 0, 0, 69, Some('$'));

        assert_eq!(2, actual.len());
        assert!(actual.contains(&&"lol".to_string()));
        assert!(actual.contains(&&"this".to_string()));
    }
}
