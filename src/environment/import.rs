use crate::node::Node;
use crate::token::Token;

#[derive(Clone, Debug, Default)]
pub struct SymbolImport {
    pub path: Vec<Token>,
    pub alias: Option<Token>,
}

impl SymbolImport {
    pub fn name(&self) -> String {
        if let Some(alias) = &self.alias {
            alias.label.clone().unwrap_or_else(|| "Anonymous alias".to_owned())
        } else {
            self.path.last().unwrap().label.clone().unwrap()
        }
    }

    pub fn full_name(&self) -> String {
        if let Some(alias) = &self.alias {
            alias.label.clone().unwrap_or_else(|| String::new())
        } else {
            self.path.iter().map(|p| p.label.clone().unwrap_or_else(|| "\\".to_owned())).collect::<String>()
        }
    }
}

/// Collect symbol imports underneath the current node
pub fn collect_uses(node: &Node, prefix: &Vec<Token>) -> Vec<SymbolImport> {
    let mut collected_uses = Vec::new();

    match node {
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
            let mut ns = prefix.clone();
            ns.extend(tokens.clone());
            collected_uses.push(SymbolImport {
                path: ns,
                alias: None
            });
        }
        _ => {}
    }

    collected_uses
}
