use crate::expression::{collect_symbols, Node};
use crate::parser::Error;
use tower_lsp::lsp_types::{Diagnostic, DocumentSymbol};

#[derive(Debug, Default)]
pub struct Environment {
    pub document_symbols: Vec<DocumentSymbol>,
    pub diagnostics: Vec<Diagnostic>,
}

impl Environment {
    pub fn cache_symbols(&mut self, ast: &Vec<Node>) {
        self.document_symbols = ast
            .iter()
            .map(|node| collect_symbols(&node))
            .collect::<Vec<Vec<DocumentSymbol>>>()
            .concat()
    }

    pub fn cache_diagnostics(&mut self, errors: &Vec<Error>) {
        self.diagnostics = errors
            .iter()
            .map(|error| Diagnostic::from(error))
            .collect::<Vec<Diagnostic>>()
    }
}
