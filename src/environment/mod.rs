use crate::environment::symbol::{PhpSymbolKind, Symbol};
use indextree::{Arena, NodeId};
use tower_lsp::lsp_types::{DocumentHighlight, Location, Position, Range, Url};

pub mod import;
pub mod scope;
pub mod symbol;
pub mod traverser;
pub mod visitor;

/// Return all references to the symbol under the cursor at `position`
pub fn document_highlights(
    _position: &Position,
    _arena: &Arena<Symbol>,
    _file: &NodeId,
) -> Option<Vec<DocumentHighlight>> {
    //let all_symbols: Vec<Symbol> = symbol.get_definitions();

    //if let Some(_symbol) = symbol_under_cursor(&all_symbols, position) {}

    None
}

/// Find the definition or reference under the cursor
pub fn symbol_location(arena: &Arena<Symbol>, symbol_node: &NodeId) -> Option<Location> {
    let range = arena[*symbol_node].get().selection_range;

    let mut symbol_node = *symbol_node;

    while let Some(parent) = arena[symbol_node].parent() {
        let symbol = arena[parent].get();

        if symbol.kind == PhpSymbolKind::File {
            return Some(Location {
                uri: Url::from_file_path(symbol.name.clone()).unwrap(),
                range,
            });
        }

        symbol_node = parent;
    }

    return None;
}

/// Checks if a given `position` is within a given `range`.
pub fn in_range(position: &Position, range: &Range) -> bool {
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
