use crate::environment::symbol::{Symbol, PhpSymbolKind};
use indextree::{Arena, NodeId};
use std::collections::HashMap;
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

/// Handle hover action triggered by the language server
/// Use `global_symbols` to resolve references to symbols in other files to a location
/// Use `root_symbols` to resolve a location to a symbol
pub fn hover(
    arena: &Arena<Symbol>,
    symbol_node: &NodeId,
    position: &Position,
    global_symbols: &HashMap<String, NodeId>,
) -> Option<String> {
    if let Some(symbol_node) = symbol_under_cursor(arena, symbol_node, position) {
        let symbol = arena[symbol_node].get();

        if let Some(resolved) = symbol.resolve(arena, &symbol_node, global_symbols, Vec::new()) {
            Some(format!(
                "{} resolved",
                arena[resolved].get().hover_text(arena, &resolved)
            ))
        } else {
            Some(symbol.hover_text(arena, &symbol_node))
        }
    } else {
        Some(String::from("Nothing found :("))
    }
}

/// Find the definition or reference under the cursor
pub fn symbol_under_cursor(
    arena: &Arena<Symbol>,
    symbol_node: &NodeId,
    position: &Position,
) -> Option<NodeId> {
    for child_symbol in symbol_node.children(arena) {
        let symbol = arena[child_symbol].get();

        // Either in this symbol or in one of its children
        if in_range(position, &symbol.range) {
            // Is it in one of the children of child_symbol?
            if let Some(child_symbol) = symbol_under_cursor(arena, &child_symbol, position) {
                return Some(child_symbol);
            }

            // If not it must be child_symbol itself
            if in_range(position, &symbol.selection_range) {
                return Some(child_symbol);
            }
        }
    }

    // Should not really happen as the most outer symbol encloses the entire document
    return None;
}

/// Find the definition or reference under the cursor
pub fn symbol_location(
    arena: &Arena<Symbol>,
    symbol_node: &NodeId
) -> Option<Location> {
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
