#![allow(clippy::must_use_candidate)]

use crate::backend::Backend;
use tower_lsp::{LspService, Server};
extern crate glob;

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

    let backend = Backend::new();

    let (service, messages) = LspService::new(backend);
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
