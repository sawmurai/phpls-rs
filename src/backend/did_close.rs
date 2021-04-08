use super::BackendState;
use crate::environment::fs as EnvFs;
use lsp_types::DidCloseTextDocumentParams;

pub(crate) fn did_close(state: &mut BackendState, params: DidCloseTextDocumentParams) {
    let p = EnvFs::normalize_path(&params.text_document.uri.to_file_path().unwrap());
    state.latest_version_of_file.remove(&p);
    state.opened_files.remove(&p);
    state.symbol_references.remove(&p);
}
