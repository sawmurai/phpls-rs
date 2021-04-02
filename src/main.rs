#![allow(clippy::must_use_candidate)]

extern crate clap;
extern crate crossbeam_channel as channel;
extern crate ignore;

use crate::backend::Backend;
use clap::{App, Arg};
use std::path::PathBuf;
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
            Arg::with_name("file")
                .long("file")
                .short("f")
                .value_name("Parse single file")
                .help("Only parse a single file instead of launching a server")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("dir")
                .long("dir")
                .short("d")
                .value_name("Parse files in directory")
                .help("Only parse files in directory instead of launching a server")
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

    let ignore_patterns: Vec<String> = matches
        .values_of("ignore-patterns")
        .unwrap_or(clap::Values::default())
        .map(|s| s.to_owned())
        .collect();

    if let Some(file) = matches.value_of("file") {
        match Backend::source_to_ast(file) {
            Ok((_, _, _)) => println!("Parsed ok"),
            Err(e) => eprintln!("Error: {}", e),
        }

        return;
    }
    if let Some(dir) = matches.value_of("dir") {
        let ip: Vec<PathBuf> = ignore_patterns.iter().map(PathBuf::from).collect();
        match environment::fs::reindex_folder(&PathBuf::from(dir), &ip) {
            Ok(paths) => {
                paths
                    .iter()
                    .map(environment::fs::normalize_path)
                    .for_each(|file| match Backend::source_to_ast(&file) {
                        Err(e) => eprintln!("Error: {}", e),
                        _ => (),
                    });
            }
            Err(_) => (),
        }

        return;
    }

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
