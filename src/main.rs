#![allow(clippy::must_use_candidate)]

use crate::backend::Backend;
use indextree::Arena;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::{LspService, Server};

pub mod backend;
pub mod environment;
pub mod node;
pub mod parser;
pub mod scanner;
pub mod token;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let backend = Backend {
        arena: Arc::new(Mutex::new(Arena::new())),
        files: Arc::new(Mutex::new(HashMap::new())),
        global_symbols: Arc::new(Mutex::new(HashMap::new())),
        diagnostics: Arc::new(Mutex::new(HashMap::new())),
        symbol_references: Arc::new(Mutex::new(HashMap::new())),
    };

    let (service, messages) = LspService::new(backend);
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
