use super::BackendState;
use lsp_types::{Location, SymbolInformation, SymbolTag, Url, WorkspaceSymbolParams};
use tower_lsp::jsonrpc::Result;

pub(crate) fn symbol(
    state: &BackendState,
    params: WorkspaceSymbolParams,
) -> Result<Option<Vec<SymbolInformation>>> {
    if params.query.is_empty() {
        return Ok(None);
    }

    let query = params.query.to_lowercase();

    let mut symbols = Vec::new();

    for (file_name, node) in state.files.iter() {
        for symbol in node.descendants(&state.arena) {
            let symbol = state.arena[symbol].get();

            if symbol.normalized_name().starts_with(&query) {
                if let Some(kind) = symbol.kind.get_symbol_kind() {
                    let tags = if symbol.deprecated.is_some() {
                        Some(vec![SymbolTag::Deprecated])
                    } else {
                        None
                    };
                    symbols.push(SymbolInformation {
                        name: symbol.name().to_owned(),
                        tags,
                        kind,
                        location: Location {
                            uri: Url::from_file_path(&file_name).unwrap(),
                            range: symbol.range,
                        },
                        container_name: None,
                        deprecated: None,
                    })
                }
            }
        }
    }

    Ok(Some(symbols))
}
