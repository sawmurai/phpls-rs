use super::BackendState;
use crate::environment::fs as EnvFs;
use lsp_types::{DocumentSymbolParams, DocumentSymbolResponse};
use tower_lsp::jsonrpc::Result;

pub(crate) fn document_symbol(
    state: &BackendState,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    let file_path = EnvFs::normalize_path(&params.text_document.uri.to_file_path().unwrap());

    let node_id = if let Some(node_id) = state.files.get(&file_path) {
        node_id.clone()
    } else {
        return Ok(None);
    };

    Ok(Some(DocumentSymbolResponse::Nested(
        node_id
            .children(&state.arena)
            .filter_map(|s| state.arena[s].get().to_doc_sym(&state.arena, &s))
            .collect(),
    )))
}
