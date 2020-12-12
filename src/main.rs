#![allow(clippy::must_use_candidate)]

use crate::backend::Backend;
use tower_lsp::{LspService, Server};

pub mod backend;
pub mod environment;
pub mod parser;

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| Backend::new(client));
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
