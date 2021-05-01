use super::{Backend, BackendState};
use crate::environment::fs as EnvFs;
use lsp_types::{Diagnostic, DidOpenTextDocumentParams};

pub(crate) fn did_open(state: &mut BackendState, params: &DidOpenTextDocumentParams) {
    let file_path = params.text_document.uri.to_file_path().unwrap();

    let path = EnvFs::normalize_path(&file_path);

    if !state.opened_files.contains_key(&path) {
        let source = std::fs::read_to_string(file_path).unwrap();
        state
            .latest_version_of_file
            .insert(path.clone(), source.clone());

        if let Ok((ast, range, errors)) = Backend::source_to_ast(&source) {
            let diags = errors.iter().map(Diagnostic::from).collect();
            state.diagnostics.insert(path.to_owned(), diags);
            state.opened_files.insert(path.to_string(), (ast, range));
        } else {
            return;
        }
    }

    let (ast, _) = if let Some((ast, range)) = state.opened_files.get(&path) {
        (ast.clone(), range)
    } else {
        return;
    };

    if let Err(_) = Backend::collect_references(&path, &ast, state, None) {
        return;
    }
}
