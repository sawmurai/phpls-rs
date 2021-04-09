use super::BackendState;
use crate::environment::{self, fs as EnvFs};
use lsp_types::{DocumentHighlight, DocumentHighlightParams};
use tower_lsp::jsonrpc::Result;

pub(crate) fn document_highlight(
    state: &BackendState,
    params: DocumentHighlightParams,
) -> Result<Option<Vec<DocumentHighlight>>> {
    let file = EnvFs::normalize_path(
        &params
            .text_document_position_params
            .text_document
            .uri
            .to_file_path()
            .unwrap(),
    );

    if let Some(node_id) = state.files.get(&file) {
        return Ok(environment::document_highlights(
            &params.text_document_position_params.position,
            &state.arena,
            node_id,
        ));
    }

    Ok(None)
}
