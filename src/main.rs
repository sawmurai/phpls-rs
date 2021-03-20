#![allow(clippy::must_use_candidate)]

extern crate clap;

use crate::backend::Backend;
use clap::{App, Arg, SubCommand};
use tower_lsp::{LspService, Server};

pub mod backend;
pub mod environment;
pub mod formatter;
pub mod parser;
pub mod suggester;

#[tokio::main]
async fn main() {
    let matches = App::new("PHPLS-RS")
        .version("0.1")
        .author("Fabian Becker <fabian.becker@b-it-d.de>")
        .about("PHP language server written in Rust")
        .arg(
            Arg::with_name("stubs")
                .long("stubs")
                .value_name("Stubs library")
                .help("Path to the phpstorm stubs")
                .required(true)
                .takes_value(true),
        )
        .arg(
            Arg::with_name("ignore-patterns")
                .long("ignore-patterns")
                .value_name("Path ending to ignore")
                .help("List of endings of file-paths to ignore during indexing")
                .multiple(true)
                .takes_value(true),
        )
        .get_matches();

    let ignore_patterns = matches
        .values_of("ignore-patterns")
        .unwrap_or(clap::Values::default())
        .map(|s| s.to_owned())
        .collect();

    let stubs = matches.value_of("stubs").unwrap().to_owned();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) =
        LspService::new(|client| Backend::new(client, stubs, ignore_patterns));
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
