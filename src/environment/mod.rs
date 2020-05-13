use crate::environment::scope::Scope;
use crate::environment::symbol::Symbol;
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use tower_lsp::lsp_types::{DocumentHighlight, Location, Position, Range, SymbolKind, Url};

pub mod import;
pub mod scope;
pub mod symbol;

/// Return all references to the symbol under the cursor at `position`
pub fn document_highlights(position: &Position, scope: &Scope) -> Option<Vec<DocumentHighlight>> {
    let all_symbols: Vec<Symbol> = scope.get_definitions();

    //if let Some(_symbol) = symbol_under_cursor(&all_symbols, position) {}

    None
}

/// Handle hover action triggered by the language server
/// Use `global_symbols` to resolve references to symbols in other files to a location
/// Use `root_scopes` to resolve a location to a scope
pub fn hover(
    uri: &Url,
    arena: &Arena<Scope>,
    scope_node: &NodeId,
    position: &Position,
    global_symbols: &HashMap<String, Location>,
    root_scopes: &HashMap<String, NodeId>,
) -> Option<String> {
    if let Some(scope_node) = scope_under_cursor(arena, scope_node, position) {
        let scope = arena[scope_node].get();

        // The easiest case is if the user hovered over a definition
        if let Some(symbol) = scope.symbol_at_position(position) {
            return Some(format!("Definition of {:?}", symbol));
        }

        // Otherwise he might have hovered over a reference?
        if let Some(reference) = scope.reference_at_position(position) {
            // Resolve the location of the references definition
            if let Some(location) =
                scope.resolve_reference(uri, reference, arena, scope_node, global_symbols)
            {
                // Get the root node of the definition of the reference
                if let Some(root_node) =
                    root_scopes.get(location.uri.to_file_path().unwrap().to_str().unwrap())
                {
                    // As position pick the first character and find the enclosing scope
                    let position = location.range.start;
                    if let Some(defining_scope) = scope_under_cursor(arena, root_node, &position) {
                        // Get the defining symbol
                        if let Some(symbol) =
                            arena[defining_scope].get().symbol_at_position(&position)
                        {
                            return Some(format!("Reference to {:?}", symbol));
                        } else {
                            eprintln!("No defining symbol in target scope {:?}", location);
                        }

                        // Try to search in the symbols parent because the range of classes etc. includes
                        // their own scope
                        if let Some(parent) = arena[defining_scope].parent() {
                            if let Some(symbol) = arena[parent].get().symbol_at_position(&position)
                            {
                                return Some(format!("Reference to {:?}", symbol));
                            } else {
                                eprintln!(
                                    "No defining symbol in target scopes parent {:?}",
                                    location
                                );
                            }
                        }
                    } else {
                        eprintln!("Target scope not found");
                    }
                }
            }

            Some(format!("Unresolvable reference to {:?}", reference))
        } else {
            None
        }
    } else {
        None
    }
}

/// Find the definition or reference under the cursor
pub fn scope_under_cursor(
    arena: &Arena<Scope>,
    scope_node: &NodeId,
    position: &Position,
) -> Option<NodeId> {
    for child_scope in scope_node.children(arena) {
        let scope = arena[child_scope].get();

        // Either in this scope or in one of its children
        if in_range(position, &scope.range) {
            // Is it in one of the children of child_scope?
            if let Some(child_scope) = scope_under_cursor(arena, &child_scope, position) {
                return Some(child_scope);
            }

            // If not it must be child_scope itself
            return Some(child_scope);
        }
    }

    // Should not really happen as the most outer scope encloses the entire document
    return None;
}

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
