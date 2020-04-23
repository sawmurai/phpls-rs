use crate::expression::{collect_symbols, Node};
use tower_lsp::lsp_types::DocumentSymbol;

#[derive(Debug, Default)]
pub struct Environment {
    pub document_symbols: Vec<DocumentSymbol>,
}

impl Environment {
    pub fn cache_symbols(&mut self, ast: &Vec<Node>) {
        self.document_symbols = ast
            .iter()
            .map(|node| collect_symbols(&node))
            .collect::<Vec<Vec<DocumentSymbol>>>()
            .concat()
    }
}
