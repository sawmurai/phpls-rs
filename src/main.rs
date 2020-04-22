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
pub mod expression;
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

        if let Ok((ast, _)) = Parser::ast(scanner.tokens.clone()) {
            let mut env = environment::Environment::new(path);

            environment::index::walk_tree(&mut env, ast);
            //env.finish_namespace();

            self.registry.lock().await.insert(path.to_owned(), env);
            eprintln!("updated");
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
        //if let Some(root_uri) = params.root_uri {
        client.log_message(MessageType::Info, format!("Indexing {:?}", params.root_uri));
        //}

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Full,
                )),
                //hover_provider: Some(true),
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
                //document_highlight_provider: Some(true),
                document_symbol_provider: Some(true),
                //workspace_symbol_provider: Some(true),
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
        let registry = self.registry.lock().await;

        if let Some(env) = registry.get(params.text_document.uri.path()) {
            Ok(Some(DocumentSymbolResponse::Nested(env.document_symbols())))
        } else {
            Ok(None)
        }
    }

    async fn did_change(&self, client: &Client, params: DidChangeTextDocumentParams) {
        if let Err(e) = self
            .reindex(
                params.text_document.uri.path(),
                &params.content_changes[0].text,
            )
            .await
        {
            client.log_message(MessageType::Error, e);
        }
    }

    async fn did_open(&self, client: &Client, params: DidOpenTextDocumentParams) {
        let path = params.text_document.uri.path();
        let content = tokio::fs::read_to_string(path).await.unwrap();

        if let Ok(()) = self.reindex(path, &content).await {
            client.log_message(
                MessageType::Info,
                format!("Indexed {}", params.text_document.uri),
            )
        } else {
            client.log_message(
                MessageType::Error,
                format!("Failed to index {}", params.text_document.uri),
            )
        }
    }

    async fn hover(&self, params: TextDocumentPositionParams) -> Result<Option<Hover>> {
        let file = params.text_document.uri.path();
        let Position { line, character } = params.position;

        if let Some(_env) = self.registry.lock().await.get(file) {
            let of = self.opened_files.lock().await;
            if let Some(token) = of
                .get(file)
                .unwrap()
                .iter()
                .find(|token| token.is_on(line as u16, character as u16))
            {
                return Ok(Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(format!(
                        "Type: {:?}, Usages: {}",
                        token.t, 0
                    ))),
                    range: None,
                }));
            } else {
                return Ok(None);
            }
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
