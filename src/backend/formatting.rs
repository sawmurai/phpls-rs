use super::BackendState;
use crate::formatter::format;
use crate::{environment::fs as EnvFs, formatter::FormatterOptions};
use lsp_types::{DocumentFormattingParams, TextEdit};
use tower_lsp::jsonrpc::Result;

pub(crate) fn formatting(
    state: &BackendState,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri;
    let file_path = uri.to_file_path().unwrap();
    let path = EnvFs::normalize_path(&file_path);

    if let Some((ast, range)) = state.opened_files.get(&path) {
        let formatted = format(
            ast,
            0,
            0,
            &FormatterOptions {
                indent: 2,
                max_line_length: 120,
            },
        );

        return Ok(Some(vec![TextEdit {
            new_text: formatted,
            range: range.to_owned(),
        }]));
    }

    Ok(None)
}
