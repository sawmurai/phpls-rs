use super::{Backend, BackendState};
use crate::environment::{
    fs as EnvFs, symbol::PhpSymbolKind, visitor::name_resolver::NameResolver,
};
use indextree::NodeId;
use lsp_types::{
    request::{GotoImplementationParams, GotoImplementationResponse},
    GotoDefinitionResponse, Location, Url,
};
use tower_lsp::jsonrpc::Result;

pub(crate) fn goto_implementation(
    state: &mut BackendState,
    params: GotoImplementationParams,
) -> Result<Option<GotoImplementationResponse>> {
    let position = &params.text_document_position_params.position;
    let file = EnvFs::normalize_path(
        &params
            .text_document_position_params
            .text_document
            .uri
            .to_file_path()
            .unwrap(),
    );
    let mut results = Vec::new();

    if let Some((nuc, sym_name)) = Backend::symbol_under_cursor(&state, position, &file) {
        let kind_of_suc = state.arena[nuc].get().kind;

        if kind_of_suc == PhpSymbolKind::Method {
            let parent_node = if let Some(parent_node) = state.arena[nuc].parent() {
                let parent_symbol = state.arena[parent_node].get();
                if parent_symbol.kind == PhpSymbolKind::Interface {
                    parent_node
                } else {
                    return Ok(None);
                }
            } else {
                return Ok(None);
            };

            let method_name = sym_name.to_lowercase();

            // We are searching for the implementations of a particular method
            return Ok(Some(GotoDefinitionResponse::Array(
                implementing_interfaces(parent_node, state)
                    .iter()
                    .filter_map(|(file, node)| {
                        node.children(&state.arena).find_map(|child_of_interface| {
                            let meth = state.arena[child_of_interface].get();

                            if meth.normalized_name().eq(&method_name) {
                                Some(Location {
                                    uri: Url::from_file_path(file).unwrap(),
                                    range: meth.selection_range,
                                })
                            } else {
                                None
                            }
                        })
                    })
                    .collect(),
            )));
        }

        if kind_of_suc == PhpSymbolKind::Interface {
            results.extend(
                implementing_interfaces(nuc, state)
                    .iter()
                    .map(|(file, node)| Location {
                        uri: Url::from_file_path(file).unwrap(),
                        range: state.arena[*node].get().selection_range,
                    }),
            )
        }
    }

    Ok(Some(GotoDefinitionResponse::Array(results)))
}

fn implementing_interfaces(interface: NodeId, state: &mut BackendState) -> Vec<(String, NodeId)> {
    let mut results = Vec::new();
    state.global_symbols.iter().find(|(_, node)| {
        let potential_symbol = state.arena[**node].get();
        let symbol_name = potential_symbol.normalized_name();

        if potential_symbol.kind != PhpSymbolKind::Class {
            return false;
        }

        potential_symbol
            .data_types
            .iter()
            .filter_map(|reference| {
                if let Some(tip) = reference.type_ref.tip() {
                    // Skip self reference
                    if !tip.to_lowercase().eq(&symbol_name) {
                        return Some(reference.type_ref.clone());
                    }
                }

                None
            })
            .find(|type_ref| {
                // There is always an enclosing file so we can safely unwrap
                let enclosing_file = node
                    .ancestors(&state.arena)
                    .find_map(|a| {
                        let ancestor = state.arena[a].get();

                        if ancestor.kind == PhpSymbolKind::File {
                            Some(a)
                        } else {
                            None
                        }
                    })
                    .unwrap();

                let mut resolver = NameResolver::new(&state.global_symbols, enclosing_file);

                if let Some(resolved) =
                    resolver.resolve_type_ref(&type_ref, &state.arena, &enclosing_file, false)
                {
                    if resolved == interface {
                        results.push((state.arena[enclosing_file].get().name.clone(), **node));
                        return true;
                    }

                    // Check if the type extends the interface we are searching for
                    if state.arena[resolved].get().is_child_of(
                        resolved,
                        &mut resolver,
                        &state.arena,
                        interface,
                    ) {
                        results.push((state.arena[enclosing_file].get().name.clone(), **node));

                        return true;
                    }
                }

                false
            });

        false
    });

    results
}
