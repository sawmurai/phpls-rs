#![allow(clippy::must_use_candidate)]

use crate::environment::scope::{collect_symbols, Scope, ScopeType};
use crate::parser::Parser;
use crate::scanner::Scanner;
use async_recursion::async_recursion;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::{self};
use tokio::sync::mpsc;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

pub mod environment;
pub mod node;
pub mod parser;
pub mod scanner;
pub mod token;

type EnvReceiver = mpsc::Receiver<(String, environment::Environment)>;
type Registry = Mutex<HashMap<String, environment::Environment>>;

#[derive(Default)]
struct Backend {
    /// Global scope that is parent to all files in the project
    global_scope: Arc<Mutex<Scope>>,

    registry: Registry,
    /// key: FQDN of symbol, value: Filename of importing fil
    usages: Mutex<HashMap<String, Vec<String>>>,

    /// key: FQDN of symbol, value: Location of definition
    definitions: Mutex<HashMap<String, Location>>,
}

impl Backend {
    async fn init_workspace(&self, url: &Url) -> io::Result<()> {
        if let Ok(root_path) = url.to_file_path() {
            self.reindex_folder(&root_path).await?;
        }

        Ok(())
    }

    #[async_recursion]
    async fn reindex_folder(&self, dir: &PathBuf) -> io::Result<()> {
        if dir.is_dir() {
            for entry in fs::read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_dir() {
                    self.reindex_folder(&path).await?;
                } else if let Some(ext) = path.extension() {
                    if ext == "php" {
                        let p = path.to_str().unwrap().to_string();
                        let content = match fs::read_to_string(path) {
                            Ok(content) => content,
                            Err(error) => {
                                eprintln!("{}", error);
                                continue;
                            }
                        };

                        self.reindex(&p, &content).await?;
                    }
                }
            }
        }
        Ok(())
    }

    async fn reindex(&self, path: &str, content: &str) -> io::Result<()> {
        let mut scanner = Scanner::new(&content);

        if let Err(msg) = scanner.scan() {
            eprintln!("Could not read file {}: {}", &path, &msg);

            return Ok(());
        }

        if let Ok((ast, errors)) = Parser::ast(scanner.tokens.clone()) {
            let mut env = environment::Environment::default();

            let scope = Scope::within(path, self.global_scope.clone(), ScopeType::File).await;

            for node in ast {
                if let Err(e) = collect_symbols(&node, scope.clone()).await {
                    eprintln!("Error collecting symbols in file {}: {}", path, e);
                }
            }

            env.cache_diagnostics(&errors);
            //env.cache_symbols(&ast);
            //env.cache_uses(&ast);

            // Have this in a block to free the lock as soon as possible
            {
                let mut usages = self.usages.lock().await;

                env.uses.iter().for_each(|symbol_import| {
                    usages
                        .entry(
                            symbol_import
                                .path
                                .iter()
                                .map(|t| t.clone().label.unwrap_or_else(|| "\\".to_owned()))
                                .collect::<Vec<String>>()
                                .join(""),
                        )
                        .or_insert_with(|| Vec::new())
                        .push(path.to_owned())
                });
            }

            {
                let mut definitions = self.definitions.lock().await;

                env.document_symbols
                    .iter()
                    .filter(|symbol| {
                        symbol.kind == SymbolKind::Class
                            || symbol.kind == SymbolKind::Interface
                            || symbol.kind == SymbolKind::Function
                            || symbol.kind == SymbolKind::Constant
                    })
                    .for_each(|symbol| {
                        // TODO: Add methods and properties of classes
                        definitions.insert(
                            env.fqdn(&symbol.name),
                            Location {
                                uri: Url::from_file_path(path).unwrap(),
                                range: symbol.range,
                            },
                        );
                    });
            }
            self.registry.lock().await.insert(path.to_owned(), env);
        } else {
            eprintln!("shiw");
        }

        Ok(())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        _client: &Client,
        params: InitializeParams,
    ) -> Result<InitializeResult> {
        if let Some(url) = params.root_uri {
            if let Ok(()) = self.init_workspace(&url).await {
                eprintln!("Indexed root");
            }
        }

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Full,
                )),
                hover_provider: Some(true),
                /*completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: None,
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),*/
                references_provider: Some(true),
                document_highlight_provider: Some(true),
                document_symbol_provider: Some(true),
                workspace_symbol_provider: Some(true),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceCapability {
                    workspace_folders: Some(WorkspaceFolderCapability {
                        supported: Some(true),
                        change_notifications: Some(
                            WorkspaceFolderCapabilityChangeNotifications::Bool(true),
                        ),
                    }),
                }),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, client: &Client, _params: InitializedParams) {
        for (file, env) in self.registry.lock().await.iter() {
            if env.diagnostics.is_empty() {
                continue;
            }

            client.publish_diagnostics(
                Url::from_file_path(file).unwrap(),
                env.diagnostics.clone(),
                None,
            );
        }
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let registry = self.registry.lock().await;
        let mut symbols = Vec::new();

        for (file, env) in registry.iter() {
            for symbol in env.document_symbols.iter() {
                if params.query != "" && symbol.name.starts_with(&params.query) {
                    symbols.push(SymbolInformation {
                        name: symbol.name.clone(),
                        deprecated: symbol.deprecated,
                        kind: symbol.kind,
                        location: Location {
                            uri: Url::from_file_path(file).unwrap(),
                            range: symbol.range,
                        },
                        container_name: None,
                    })
                }
            }
        }

        Ok(Some(symbols))
    }

    // TODO:
    // * Add range to scope to quickly jump into the correct scope,
    // ** Alternatively find a way to jump from a symbol to a scope
    // * Read symbols from scope
    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let path = params.text_document.uri.path();
        let global_scope = self.global_scope.lock().await;

        if let Some(scope) = global_scope.children.get(path) {
            //let top_level_symbols = scope.upgrade().unwrap();
            let top_level_symbols = scope.lock().await;

            if let Some(definitions) = top_level_symbols.get_definitions() {
                return Ok(Some(DocumentSymbolResponse::Nested(
                    definitions
                        .iter()
                        .map(|s| DocumentSymbol::from(s))
                        .collect(),
                )));
            } else {
                return Ok(None);
            }
        }

        Ok(None)
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let file = params
            .text_document_position_params
            .text_document
            .uri
            .path();

        if let Some(env) = self.registry.lock().await.get(file) {
            return Ok(env.document_highlights(&params.text_document_position_params.position));
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let file = params.text_document_position.text_document.uri.path();

        if let Some(env) = self.registry.lock().await.get(file) {
            if let Some(symbol) = env.hover(&params.text_document_position.position) {
                eprintln!("Searching or {}", env.fqdn(&symbol.name));
                if let Some(usages) = self.usages.lock().await.get(&env.fqdn(&symbol.name)) {
                    return Ok(Some(
                        usages
                            .iter()
                            .map(|usage| Location {
                                uri: Url::from_file_path(&usage).unwrap(),
                                range: Range {
                                    start: Position {
                                        line: 0,
                                        character: 0,
                                    },
                                    end: Position {
                                        line: 0,
                                        character: 0,
                                    },
                                },
                            })
                            .collect::<Vec<Location>>(),
                    ));
                } else {
                    eprintln!("No Usages found");
                }
            } else {
                eprintln!("Symbol not found");
            }
        } else {
            eprintln!("File not found");
        }

        Ok(None)
    }

    async fn did_change(&self, client: &Client, params: DidChangeTextDocumentParams) {
        let path = params.text_document.uri.path();

        if let Err(e) = self.reindex(path, &params.content_changes[0].text).await {
            client.log_message(MessageType::Error, e);

            return;
        }

        if let Some(env) = self.registry.lock().await.get(path) {
            client.publish_diagnostics(
                params.text_document.uri,
                env.diagnostics.clone(),
                params.text_document.version,
            );
        }
    }

    async fn did_open(&self, client: &Client, params: DidOpenTextDocumentParams) {
        let path = params.text_document.uri.path();
        let content = tokio::fs::read_to_string(path).await.unwrap();

        if let Err(e) = self.reindex(path, &content).await {
            client.log_message(MessageType::Error, format!("Failed to index {}", e));

            return;
        }

        if let Some(env) = self.registry.lock().await.get(path) {
            client.publish_diagnostics(
                params.text_document.uri,
                env.diagnostics.clone(),
                Some(params.text_document.version),
            );
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let file = params
            .text_document_position_params
            .text_document
            .uri
            .path();

        if let Some(env) = self.registry.lock().await.get(file) {
            return Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(format!(
                    "{:?}",
                    env.hover(&params.text_document_position_params.position)
                ))),
                range: None,
            }));
        }

        Ok(None)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, _params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }
}

pub async fn launch_registry_writer(registry: Registry, mut rx: EnvReceiver) {
    while let Some((path, env)) = rx.recv().await {
        registry.lock().await.insert(path, env);
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let backend = Backend::default();

    let (service, messages) = LspService::new(backend);
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
