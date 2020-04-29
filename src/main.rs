#![allow(clippy::must_use_candidate)]

use crate::parser::Parser;
use crate::scanner::Scanner;
use std::collections::HashMap;
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
type Registry = Arc<Mutex<HashMap<String, environment::Environment>>>;

#[derive(Debug, Default)]
struct Backend {
    registry: Registry,
    opened_files: Arc<Mutex<HashMap<String, Vec<token::Token>>>>,
}

impl Backend {
    pub async fn reindex(&self, path: &str, content: &str) -> io::Result<()> {
        let mut scanner = Scanner::new(&content);

        if let Err(msg) = scanner.scan() {
            eprintln!("Could not read file {}: {}", &path, &msg);

            return Ok(());
        }

        if let Ok((ast, errors)) = Parser::ast(scanner.tokens.clone()) {
            let mut env = environment::Environment::default();

            env.cache_diagnostics(&errors);
            env.cache_symbols(&ast);
            //env.finish_namespace();

            self.registry.lock().await.insert(path.to_owned(), env);
        } else {
            eprintln!("shiw");
        }

        self.opened_files
            .lock()
            .await
            .insert(path.to_owned(), scanner.tokens);

        Ok(())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    fn initialize(&self, client: &Client, params: InitializeParams) -> Result<InitializeResult> {
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

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let path = params.text_document.uri.path();
        {
            let registry = self.registry.lock().await;

            if let Some(env) = registry.get(path) {
                return Ok(Some(DocumentSymbolResponse::Nested(
                    env.document_symbols.clone(),
                )));
            }
        }
        // Well, nothing cached yet. Index and return result
        let content = tokio::fs::read_to_string(path).await.unwrap();
        if let Ok(()) = self.reindex(path, &content).await {
            let registry = self.registry.lock().await;
            if let Some(env) = registry.get(path) {
                return Ok(Some(DocumentSymbolResponse::Nested(
                    env.document_symbols.clone(),
                )));
            }
        }

        Ok(None)
    }

    async fn document_highlight(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let file = params.text_document.uri.path();

        if let Some(env) = self.registry.lock().await.get(file) {
            return Ok(env.document_highlights(&params.position));
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

    async fn hover(&self, params: TextDocumentPositionParams) -> Result<Option<Hover>> {
        let file = params.text_document.uri.path();

        if let Some(env) = self.registry.lock().await.get(file) {
            return Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::String(format!(
                    "{:?}",
                    env.hover(&params.position)
                ))),
                range: None,
            }));
        }

        Ok(None)
    }

    async fn initialized(&self, client: &Client, _: InitializedParams) {
        client.log_message(MessageType::Info, "Initialized");
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
