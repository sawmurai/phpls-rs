use crate::environment::symbol::{PhpSymbolKind, Symbol};
use crate::parser::node::NodeRange;
use indextree::{Arena, NodeId};
use tower_lsp::lsp_types::{DiagnosticSeverity, DocumentHighlight, Location, Position, Range, Url};

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
    let start = coords.0;
    let end = coords.1;

    Range {
        start: Position {
            line: u64::from(start.0),
            character: u64::from(start.1),
        },
        end: Position {
            line: u64::from(end.0),
            character: u64::from(end.1),
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
            severity: DiagnosticSeverity::Error,
        }
    }

    pub fn warning(file: String, message: String, range: NodeRange) -> Self {
        Notification {
            file,
            message,
            range,
            severity: DiagnosticSeverity::Warning,
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
        let range = get_range(((1, 100), (2, 200)));
        assert_eq!(expected, range);
    }
}
