use lsp_types::{Diagnostic, DidChangeWatchedFilesParams};

use super::{Backend, BackendState};
use crate::environment::fs as EnvFs;

pub(crate) fn did_change_watched_files(
    state: &mut BackendState,
    params: DidChangeWatchedFilesParams,
) {
    for change in params.changes {
        let file_path = change.uri.to_file_path().unwrap();

        let path = EnvFs::normalize_path(&file_path);

        // If the file is currently opened we don't have to refresh
        if state.opened_files.contains_key(&path) {
            return;
        }

        let content = std::fs::read_to_string(file_path).unwrap();

        if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
            let reindex_result = Backend::collect_symbols(&path, &ast, &range, state);

            let diagnostics = state
                .diagnostics
                .entry(path.to_string())
                .or_insert_with(Vec::new);
            diagnostics.clear();
            diagnostics.extend(errors.iter().map(Diagnostic::from));
        }
    }
}
