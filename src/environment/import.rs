use crate::parser::node::Node;
use crate::parser::token::Token;
use crate::{
    environment::symbol::{PhpSymbolKind, Symbol},
    parser::node::TypeRef,
};
use tower_lsp::lsp_types::{Position, Range};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SymbolImport {
    pub path: TypeRef,
    pub alias: Option<Token>,
}
#[derive(Clone, Debug, Default)]
pub struct SymbolImportBlock(Vec<SymbolImport>);

impl SymbolImportBlock {
    pub fn all(&self) -> std::slice::Iter<SymbolImport> {
        self.0.iter()
    }

    pub fn extend(&mut self, imports: Vec<SymbolImport>) {
        self.0.extend(imports);
    }
}

impl From<Vec<SymbolImport>> for SymbolImportBlock {
    fn from(block: Vec<SymbolImport>) -> SymbolImportBlock {
        SymbolImportBlock(block)
    }
}

#[derive(Clone, Debug)]
pub enum TraitUseAlteration {
    As {
        visibility: Option<Token>,
        class: Option<TypeRef>,
        member: String,
        alias: Option<Token>,
    },
    InsteadOf {
        class: TypeRef,
        member: String,
        instead_ofs: Vec<TypeRef>,
    },
}

impl SymbolImport {
    pub fn name(&self) -> String {
        if let Some(alias) = &self.alias {
            alias.label.clone().unwrap()
        } else if let Some(tip) = self.path.tip() {
            tip.to_owned()
        } else {
            String::from("")
        }
    }

    pub fn full_name(&self) -> String {
        self.path.to_fqdn()
    }
}

impl From<&SymbolImport> for Symbol {
    fn from(symbol_import: &SymbolImport) -> Symbol {
        let start = symbol_import.path.range().0;
        let end = if let Some(alias) = symbol_import.alias.as_ref() {
            alias.end()
        } else {
            symbol_import.path.range().1
        };

        let range = Range {
            start: Position {
                line: u32::from(start.0),
                character: u32::from(start.1),
            },
            end: Position {
                line: u32::from(end.0),
                character: u32::from(end.1),
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

pub fn collect_alterations(node: &Node) -> Vec<TraitUseAlteration> {
    let mut rules = Vec::new();
    match node {
        Node::UseTraitAlterationBlock { alterations, .. } => {
            // Aight, this section should output something like
            // - traits: A, B, C, D
            // - alterations / rules / modifiers:
            // - A::rofl instead of C, B <- check if C and B have a rofl, if a has a rofl and if D has no rofl
            // - rofl as private <- check if there is a rofl at all and if its only in one of the A, B, C or D
            //
            // Overall check if:
            // - no conflicts left unresolved

            for alteration in alterations {
                match alteration {
                    Node::UseTraitAs {
                        left,
                        member,
                        visibility,
                        as_name,
                        ..
                    } => {
                        let class = if let Some(Node::TypeRef(tr)) = left.as_deref() {
                            Some(tr.clone())
                        } else {
                            None
                        };

                        rules.push(TraitUseAlteration::As {
                            class,
                            visibility: visibility.clone(),
                            alias: as_name.clone(),
                            member: member.name(),
                        });
                    }
                    Node::UseTraitInsteadOf {
                        left,
                        member,
                        insteadof_list,
                        ..
                    } => {
                        let class = if let Some(Node::TypeRef(tr)) = left.as_deref() {
                            tr.clone()
                        } else {
                            // TODO: There must always be a class:: ... rewrite parser to enforce it
                            continue;
                        };
                        let instead_ofs = insteadof_list
                            .iter()
                            .map(|tr| match tr {
                                Node::TypeRef(tr) => tr.clone(),
                                _ => unreachable!("Impossibru"),
                            })
                            .collect();
                        rules.push(TraitUseAlteration::InsteadOf {
                            class,
                            member: member.name(),
                            instead_ofs,
                        });
                    }
                    _ => (),
                }
            }
        }
        Node::UseTraitStatement { traits_usages, .. } => {
            traits_usages
                .iter()
                .for_each(|n| rules.extend(collect_alterations(n)));
        }
        _ => (),
    }

    rules
}

/// Collect symbol imports underneath the current node
pub fn collect_uses(node: &Node, prefix: &TypeRef) -> Vec<SymbolImport> {
    let mut collected_uses = Vec::new();

    match node {
        Node::UseTraitAlterationBlock {
            alteration_group_type_refs,
            ..
        } => {
            alteration_group_type_refs
                .iter()
                .for_each(|n| collected_uses.extend(collect_uses(n, prefix)));
        }
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
            collected_uses.push(SymbolImport {
                path: TypeRef::append(prefix, tokens),
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
            token: Token::new(TokenType::Use, 1, 1, 0),
            imports: vec![Node::UseDeclaration {
                token: Some(Token::new(TokenType::Use, 1, 1, 0)),
                declaration: Box::new(Node::TypeRef(
                    vec![Token::named(
                        TokenType::Identifier,
                        1,
                        1,
                        0,
                        "IncludedSymbol",
                    )]
                    .into(),
                )),
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
                offset: Some(0),
            }]
            .into(),
            alias: None,
            ..SymbolImport::default()
        };
        assert_eq!(expected, collect_uses(&use_statement, &vec![].into())[0]);
    }

    #[test]
    fn test_collects_use_trait() {
        let trait_use = Node::UseTraitStatement {
            token: Token::new(TokenType::Use, 1, 1, 0),
            traits_usages: vec![Node::UseTrait {
                type_ref: Box::new(Node::TypeRef(
                    vec![Token::named(
                        TokenType::Identifier,
                        1,
                        1,
                        0,
                        "IncludedSymbol",
                    )]
                    .into(),
                )),
            }],
        };

        let expected = SymbolImport {
            path: vec![Token {
                col: 1,
                line: 1,
                t: TokenType::Identifier,
                label: Some("IncludedSymbol".to_owned()),
                offset: Some(0),
            }]
            .into(),
            alias: None,
            ..SymbolImport::default()
        };
        assert_eq!(expected, collect_uses(&trait_use, &vec![].into())[0]);
    }
}
