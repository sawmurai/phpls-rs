use super::{Backend, BackendState};
use crate::environment::fs as EnvFs;
use lsp_types::DidChangeTextDocumentParams;

pub(crate) fn did_change(state: &mut BackendState, params: &DidChangeTextDocumentParams) {
    let uri = params.text_document.uri.clone();
    let file_path = uri.to_file_path().unwrap();
    let path = EnvFs::normalize_path(&file_path);

    if let Some(changes) = params.content_changes.first() {
        state
            .latest_version_of_file
            .insert(path, changes.text.clone());

        Backend::refresh_file(state, uri, &changes.text);
    }
}
