use crate::environment::import::collect_uses;
use crate::environment::import::SymbolImport;
use crate::environment::scope::Scope;
use crate::environment::symbol::Symbol;
use crate::node::Node;
use tower_lsp::lsp_types::{DocumentHighlight, Position, Range, SymbolKind};

pub mod import;
pub mod scope;
pub mod symbol;

#[derive(Default)]
pub struct Environment {
    /// Uses / imports in the current file
    pub uses: Vec<SymbolImport>,
}

/// Return all references to the symbol under the cursor at `position`
pub async fn document_highlights(
    position: &Position,
    scope: &Scope,
) -> Option<Vec<DocumentHighlight>> {
    let all_symbols: Vec<Symbol> = scope.all_definitions().await;

    if let Some(symbol) = symbol_under_cursor(&all_symbols, position) {
        if let Some(container) = symbol.references {
            return Some(
                container
                    .iter()
                    .map(|usage| DocumentHighlight {
                        range: usage.range,
                        kind: None,
                    })
                    .collect::<Vec<DocumentHighlight>>(),
            );
        }
    }

    None
}
pub async fn hover(position: &Position, scope: &Scope) -> Option<Symbol> {
    let all_symbols: Vec<Symbol> = scope.all_definitions().await;

    symbol_under_cursor(&all_symbols, position)
}

impl Environment {
    pub fn cache_uses(&mut self, ast: &[Node]) {
        self.uses = ast
            .iter()
            .map(|node| collect_uses(&node, &Vec::new()))
            .collect::<Vec<Vec<SymbolImport>>>()
            .concat()
    }
}

pub async fn fqdn(name: &str, scope: &Scope) -> String {
    let all_symbols: Vec<Symbol> = scope.all_definitions().await;

    for s in all_symbols
        .iter()
        .filter(|s| s.kind == SymbolKind::Namespace)
    {
        return format!("{}\\{}", s.name, name);
    }

    return name.to_owned();
}

pub fn symbol_under_cursor<'a>(symbols: &'a [Symbol], position: &Position) -> Option<Symbol> {
    for symbol in symbols {
        if in_range(position, &symbol.selection_range) {
            return Some(symbol.clone());
        } else if in_range(position, &symbol.range) {
            if let Some(children) = symbol.children.as_ref() {
                return symbol_under_cursor(&children, position);
            } else {
                return Some(symbol.clone());
            }
        }
    }

    return None;
}

pub fn usages_of_symbol<'a>(
    symbol: &Symbol,
    symbols: &'a [Symbol],
    container: &mut Vec<&'a Symbol>,
) {
    for child in symbols {
        if child.name == symbol.name && child.kind == symbol.kind {
            container.push(&child);
        } else if let Some(children) = child.children.as_ref() {
            usages_of_symbol(symbol, &children, container);
        }
    }
}

fn in_range(position: &Position, range: &Range) -> bool {
    // Before the start or behind the end
    if position.line > range.end.line || position.line < range.start.line {
        return false;
    }

    // Within the lines
    if position.line > range.start.line && position.line < range.end.line {
        return true;
    }

    // On the start line but before the start
    if position.line == range.start.line && position.character < range.start.character {
        return false;
    }

    // On the end line but behind the end
    if position.line == range.end.line && position.character > range.end.character {
        return false;
    }

    true
}
