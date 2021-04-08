use lsp_types::DidOpenTextDocumentParams;

use super::{Backend, BackendState};

pub(crate) fn did_open(state: &mut BackendState, params: &DidOpenTextDocumentParams) {
    let file_path = params.text_document.uri.to_file_path().unwrap();

    let source = std::fs::read_to_string(file_path).unwrap();

    Backend::refresh_file(state, params.text_document.uri.clone(), &source);
}
