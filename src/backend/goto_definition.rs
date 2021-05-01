use super::BackendState;
use crate::environment::{self, fs as EnvFs, get_range, in_range};
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse};
use tower_lsp::jsonrpc::Result;

pub(crate) fn goto_definition(
    state: &BackendState,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let uri = params.text_document_position_params.text_document.uri;
    let file = EnvFs::normalize_path(&uri.to_file_path().unwrap());

    let position = &params.text_document_position_params.position;

    if let Some(references) = state.symbol_references.get(&file) {
        for (node, ranges) in references {
            if ranges.iter().any(|r| in_range(position, &get_range(*r))) {
                if let Some(location) = environment::symbol_location(&state.arena, node) {
                    return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                }
            }
        }
    }

    Ok(None)
}
