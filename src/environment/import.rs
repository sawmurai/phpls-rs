use crate::environment::symbol::{PhpSymbolKind, Symbol};
use crate::parser::node::Node;
use crate::parser::token::{to_fqdn, Token};
use tower_lsp::lsp_types::{Position, Range};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SymbolImport {
    pub path: Vec<Token>,
    pub alias: Option<Token>,
}

pub fn namespace_to_string(path: &[Token]) -> String {
    to_fqdn(path)
}

impl SymbolImport {
    pub fn name(&self) -> String {
        if let Some(alias) = &self.alias {
            alias.label.clone().unwrap()
        } else {
            if self.path.is_empty() {
                return "".to_string();
            }

            self.path
                .last()
                .unwrap()
                .label
                .clone()
                .unwrap_or_else(|| "".to_string())
        }
    }

    pub fn full_name(&self) -> String {
        to_fqdn(&self.path)
    }
}

impl From<&SymbolImport> for Symbol {
    fn from(symbol_import: &SymbolImport) -> Symbol {
        let start = symbol_import.path.first().unwrap().start();
        let end = if let Some(alias) = symbol_import.alias.as_ref() {
            alias.end()
        } else {
            symbol_import.path.last().unwrap().end()
        };

        let range = Range {
            start: Position {
                line: u64::from(start.0),
                character: u64::from(start.1),
            },
            end: Position {
                line: u64::from(end.0),
                character: u64::from(end.1),
            },
        };

        Symbol {
            name: symbol_import.full_name(),
            kind: PhpSymbolKind::Import,
            range,
            selection_range: range,
            ..Symbol::default()
        }
    }
}

/// Collect symbol imports underneath the current node
pub fn collect_uses(node: &Node, prefix: &[Token]) -> Vec<SymbolImport> {
    let mut collected_uses = Vec::new();

    match node {
        Node::UseTraitStatement { traits_usages, .. } => {
            traits_usages
                .iter()
                .for_each(|n| collected_uses.extend(collect_uses(n, prefix)));
        }
        Node::UseTrait { type_ref } => {
            collected_uses.extend(collect_uses(type_ref, prefix));
        }
        Node::UseStatement { imports, .. } => {
            imports
                .iter()
                .for_each(|n| collected_uses.extend(collect_uses(n, prefix)));
        }
        Node::UseFunctionStatement { imports, .. } => {
            imports
                .iter()
                .for_each(|n| collected_uses.extend(collect_uses(n, prefix)));
        }
        Node::UseConstStatement { imports, .. } => {
            imports
                .iter()
                .for_each(|n| collected_uses.extend(collect_uses(n, prefix)));
        }
        Node::GroupedUse { parent, uses, .. } => {
            uses.iter().for_each(|n| {
                collected_uses.extend(collect_uses(n, &collect_uses(parent, prefix)[0].path))
            });
        }
        Node::UseDeclaration {
            declaration, alias, ..
        } => {
            let import = &collect_uses(declaration, prefix)[0];

            collected_uses.push(SymbolImport {
                alias: alias.clone(),
                ..import.clone()
            });
        }
        Node::UseFunction {
            function, alias, ..
        } => {
            let import = &collect_uses(function, prefix)[0];

            collected_uses.push(SymbolImport {
                alias: alias.clone(),
                ..import.clone()
            });
        }
        Node::UseConst {
            constant, alias, ..
        } => {
            let import = &collect_uses(constant, prefix)[0];

            collected_uses.push(SymbolImport {
                alias: alias.clone(),
                ..import.clone()
            });
        }
        Node::TypeRef(tokens) => {
            let mut ns = prefix.to_owned();
            ns.extend(tokens.clone());
            collected_uses.push(SymbolImport {
                path: ns,
                alias: None,
            });
        }
        _ => {}
    }

    collected_uses
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::token::{Token, TokenType};

    #[test]
    fn test_collects_use_statement() {
        let use_statement = Node::UseStatement {
            token: Token::new(TokenType::Use, 1, 1),
            imports: vec![Node::UseDeclaration {
                token: Some(Token::new(TokenType::Use, 1, 1)),
                declaration: Box::new(Node::TypeRef(vec![Token::named(
                    TokenType::Identifier,
                    1,
                    1,
                    "IncludedSymbol",
                )])),
                aliased: None,
                alias: None,
            }],
        };

        let expected = SymbolImport {
            path: vec![Token {
                col: 1,
                line: 1,
                t: TokenType::Identifier,
                label: Some("IncludedSymbol".to_owned()),
            }],
            alias: None,
        };
        assert_eq!(expected, collect_uses(&use_statement, &vec![])[0]);
    }

    #[test]
    fn test_collects_use_trait() {
        let trait_use = Node::UseTraitStatement {
            token: Token::new(TokenType::Use, 1, 1),
            traits_usages: vec![Node::UseTrait {
                type_ref: Box::new(Node::TypeRef(vec![Token::named(
                    TokenType::Identifier,
                    1,
                    1,
                    "IncludedSymbol",
                )])),
            }],
        };

        let expected = SymbolImport {
            path: vec![Token {
                col: 1,
                line: 1,
                t: TokenType::Identifier,
                label: Some("IncludedSymbol".to_owned()),
            }],
            alias: None,
        };
        assert_eq!(expected, collect_uses(&trait_use, &vec![])[0]);
    }
}
