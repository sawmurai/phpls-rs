use super::BackendState;
use crate::formatter::format_file;
use crate::parser::scanner::Scanner;
use crate::parser::Parser;
use crate::{
    environment::{fs as EnvFs, get_range},
    formatter::FormatterOptions,
};
use lsp_types::{DocumentFormattingParams, TextEdit};
use tower_lsp::jsonrpc::Result;

pub(crate) fn formatting(
    state: &BackendState,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri;
    let file_path = uri.to_file_path().unwrap();
    let path = EnvFs::normalize_path(&file_path);

    if let Some(source) = state.latest_version_of_file.get(&path) {
        let mut scanner = Scanner::new(source);
        scanner.scan().unwrap();

        let range = scanner.document_range();
        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();

        // Reformatting a half broken source is a very bad idea, only format if its
        // parsed without errors.
        if !errors.is_empty() {
            return Ok(None);
        }

        let formatted = format!(
            "<?php\n{}",
            format_file(
                &ast,
                0,
                0,
                &FormatterOptions {
                    indent: 4,
                    max_line_length: 120,
                },
            )
        );

        return Ok(Some(vec![TextEdit {
            new_text: formatted,
            range: get_range(range),
        }]));
    }

    Ok(None)
}
