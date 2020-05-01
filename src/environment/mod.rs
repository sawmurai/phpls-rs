use crate::node::{collect_symbols, collect_uses, Node, Scope, SymbolImport};
use crate::parser::Error;
use std::sync::Arc;
use std::sync::Mutex;
use tower_lsp::lsp_types::{
    Diagnostic, DocumentHighlight, DocumentSymbol, Position, Range, SymbolKind,
};

#[derive(Default)]
pub struct Environment {
    pub document_symbols: Vec<DocumentSymbol>,
    pub diagnostics: Vec<Diagnostic>,

    /// Uses / imports in the current file
    pub uses: Vec<SymbolImport>,

    pub scope: Arc<Mutex<Scope>>,
}

impl Environment {
    pub fn cache_symbols(&mut self, ast: &[Node]) {
        let scope = Arc::new(Mutex::new(Scope::default()));
        ast.iter()
            .for_each(|node| collect_symbols(&node, scope.clone()).unwrap());

        self.document_symbols = scope.lock().unwrap().definitions.clone();
    }

    pub fn cache_diagnostics(&mut self, errors: &[Error]) {
        self.diagnostics = errors
            .iter()
            .map(Diagnostic::from)
            .collect::<Vec<Diagnostic>>()
    }

    pub fn cache_uses(&mut self, ast: &[Node]) {
        self.uses = ast
            .iter()
            .map(|node| collect_uses(&node, &Vec::new()))
            .collect::<Vec<Vec<SymbolImport>>>()
            .concat()
    }

    pub fn document_highlights(&self, position: &Position) -> Option<Vec<DocumentHighlight>> {
        if let Some(symbol) = symbol_under_cursor(&self.document_symbols, position) {
            let mut container = Vec::new();
            usages_of_symbol(&symbol, &self.document_symbols, &mut container);

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

        None
    }

    pub fn hover(&self, position: &Position) -> Option<&DocumentSymbol> {
        symbol_under_cursor(&self.document_symbols, position)
    }

    pub fn fqdn(&self, name: &str) -> String {
        for s in self
            .document_symbols
            .iter()
            .filter(|s| s.kind == SymbolKind::Namespace)
        {
            return format!("{}\\{}", s.name, name);
        }

        return name.to_owned();
    }
}

fn symbol_under_cursor<'a>(
    symbols: &'a [DocumentSymbol],
    position: &Position,
) -> Option<&'a DocumentSymbol> {
    for symbol in symbols {
        if in_range(position, &symbol.selection_range) {
            return Some(symbol);
        } else if in_range(position, &symbol.range) {
            if let Some(children) = symbol.children.as_ref() {
                return symbol_under_cursor(&children, position);
            } else {
                return Some(symbol);
            }
        }
    }

    return None;
}

fn usages_of_symbol<'a>(
    symbol: &DocumentSymbol,
    symbols: &'a [DocumentSymbol],
    container: &mut Vec<&'a DocumentSymbol>,
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
