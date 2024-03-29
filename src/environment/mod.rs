use crate::environment::symbol::{PhpSymbolKind, Symbol};
use crate::parser::node::NodeRange;
use indextree::{Arena, NodeId};
use tower_lsp::lsp_types::{DiagnosticSeverity, Location, Position, Range, Url};

pub mod fs;
pub mod import;
pub mod scope;
pub mod symbol;
pub mod traverser;
pub mod visitor;

/// Find the definition or reference under the cursor
pub(crate) fn symbol_location(arena: &Arena<Symbol>, symbol_node: &NodeId) -> Option<Location> {
    let range = arena[*symbol_node].get().selection_range;

    let mut symbol_node = *symbol_node;

    while let Some(parent) = arena[symbol_node].parent() {
        let symbol = arena[parent].get();

        if symbol.kind == PhpSymbolKind::File {
            return Some(Location {
                uri: Url::from_file_path(symbol.name()).unwrap(),
                range,
            });
        }

        symbol_node = parent;
    }

    None
}

/// Convert a node range into a Range understood by tower lsp
pub fn get_range(coords: NodeRange) -> Range {
    Range {
        start: Position {
            line: coords.start_line,
            character: coords.start_col,
        },
        end: Position {
            line: coords.end_line,
            character: coords.end_col,
        },
    }
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

#[derive(Clone)]
pub struct Notification {
    pub file: String,
    pub message: String,
    pub range: NodeRange,
    pub severity: DiagnosticSeverity,
}

impl Notification {
    pub fn error(file: String, message: String, range: NodeRange) -> Self {
        Notification {
            file,
            message,
            range,
            severity: DiagnosticSeverity::ERROR,
        }
    }

    pub fn warning(file: String, message: String, range: NodeRange) -> Self {
        Notification {
            file,
            message,
            range,
            severity: DiagnosticSeverity::WARNING,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::get_range;
    use tower_lsp::lsp_types::{Position, Range};

    #[test]
    fn test_converts_ranges() {
        let expected = Range {
            start: Position {
                line: 1,
                character: 100,
            },
            end: Position {
                line: 2,
                character: 200,
            },
        };
        let range = get_range(((100, 1), (200, 2)).into());
        assert_eq!(expected, range);
    }
}
