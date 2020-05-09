#![allow(clippy::must_use_candidate)]

use crate::environment::scope::{collect_symbols, Scope, ScopeType};
use crate::environment::symbol::{document_symbol, Symbol};
use crate::node::Node;
use crate::parser::Parser;
use crate::scanner::Scanner;
use async_recursion::async_recursion;
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::{self};
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

pub mod environment;
pub mod node;
pub mod parser;
pub mod scanner;
pub mod token;

struct Backend {
    /// Storage arena for all scopes
    arena: Arc<Mutex<Arena<Scope>>>,

    /// Global entry point
    global_scope: Arc<Mutex<NodeId>>,

    /// NodeIds of entry points to files
    root_scopes: Arc<Mutex<HashMap<String, NodeId>>>,

    /// Global symbol table. Key is the full name of the symbol, including the namespace
    /// Value is the NodeId of the symbols parent
    global_symbols: Arc<Mutex<HashMap<String, Location>>>,

    /// Global list of all diagnostics
    diagnostics: Arc<Mutex<HashMap<String, Vec<Diagnostic>>>>,
}

impl Backend {
    async fn init_workspace(&self, url: &Url) -> io::Result<()> {
        if let Ok(root_path) = url.to_file_path() {
            self.reindex_folder(&root_path).await?;
        }

        let arena = self.arena.lock().await;
        let mut diagnostics = self.diagnostics.lock().await;
        let global_symbols = self.global_symbols.lock().await;

        for (file, node_id) in self.root_scopes.lock().await.iter() {
            if file.ends_with("DeserializationServiceProvider.php") {
                for scope in node_id.descendants(&arena) {
                    for missing in
                        arena[scope]
                            .get()
                            .get_unresolvable(&url, &arena, scope, &global_symbols)
                    {
                        diagnostics
                            .get_mut(file)
                            .unwrap()
                            .push(Diagnostic::from(missing));
                    }
                }
            }
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
                    //} && !path.ends_with("vendor") {
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
        let uri = Url::from_file_path(path).unwrap();
        let mut scanner = Scanner::new(&content);

        if let Err(msg) = scanner.scan() {
            eprintln!("Could not read file {}: {}", &path, &msg);

            return Ok(());
        }

        if let Ok((ast, errors)) = Parser::ast(scanner.tokens.clone()) {
            let mut arena = self.arena.lock().await;
            let scope = arena.new_node(Scope::new(ScopeType::File, scanner.document_range()));
            let global_scope = self.global_scope.lock().await;

            global_scope.append(scope, &mut arena);

            let mut iter = ast.iter();
            while let Some(node) = iter.next() {
                match node {
                    // Once we detect a namespace statement we consider all following definitions to be
                    // within the namespaces scope
                    Node::NamespaceStatement { .. } => {
                        // Get the namespace symbol
                        let symbol = document_symbol(&mut arena, &scope, &node).unwrap();

                        // Create a new scope for the namespace and use the entire file range
                        let range = arena[scope].get().range;
                        let child = arena.new_node(Scope {
                            scope_type: ScopeType::Namespace,
                            range,
                            ..Default::default()
                        });

                        // Append the new namespace under the file scope
                        scope.append(child, &mut arena);

                        // Read all further nodes into the namespace scope
                        while let Some(node) = iter.next() {
                            collect_symbols(&mut arena, &child, &node).unwrap();
                        }

                        // Create a new symbol that also includes the children
                        let symbol = Symbol {
                            children: Some(arena[child].get().get_definitions()),
                            range,
                            ..symbol
                        };
                        arena[scope].get_mut().definition(symbol);

                        continue;
                    }
                    _ => {}
                }

                if let Err(e) = collect_symbols(&mut arena, &scope, &node) {
                    eprintln!("Error collecting symbols in file {}: {}", path, e);
                }
            }

            let mut global_symbols = self.global_symbols.lock().await;

            // Register globally available symbols
            arena[scope]
                .get()
                .symbols
                .iter()
                .filter(|(_, symbol)| {
                    symbol.kind == SymbolKind::Namespace && symbol.children.is_some()
                })
                .for_each(|(name, symbol)| {
                    symbol
                        .children
                        .as_ref()
                        .unwrap()
                        .iter()
                        .filter(|child| {
                            child.kind == SymbolKind::Class || child.kind == SymbolKind::Interface
                        })
                        .for_each(|child| {
                            global_symbols.insert(
                                format!("{}\\{}", name.clone(), child.name),
                                Location {
                                    uri: uri.clone(),
                                    range: child.range,
                                },
                            );
                        });
                });

            self.root_scopes.lock().await.insert(path.to_owned(), scope);
            self.diagnostics.lock().await.insert(
                path.to_owned(),
                errors
                    .iter()
                    .map(Diagnostic::from)
                    .collect::<Vec<Diagnostic>>(),
            );
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
        for (file, diagnostics) in self.diagnostics.lock().await.iter() {
            if diagnostics.is_empty() {
                continue;
            }

            client.publish_diagnostics(
                Url::from_file_path(file).unwrap(),
                diagnostics.clone(),
                None,
            );
        }
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let mut symbols = Vec::new();
        let arena = self.arena.lock().await;

        for (file, node_id) in self.root_scopes.lock().await.iter() {
            let scope = arena[*node_id].get();

            for symbol in scope.get_definitions() {
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
        let arena = self.arena.lock().await;

        if let Some(node_id) = self.root_scopes.lock().await.get(path) {
            let scope = arena[*node_id].get();

            //let top_level_symbols = scope.upgrade().unwrap();
            let definitions = scope.get_definitions();
            if !definitions.is_empty() {
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
        let arena = self.arena.lock().await;

        if let Some(node_id) = self.root_scopes.lock().await.get(file) {
            return Ok(environment::document_highlights(
                &params.text_document_position_params.position,
                &arena[*node_id].get(),
            ));
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let file = params.text_document_position.text_document.uri.path();

        if let Some(_arena) = self.root_scopes.lock().await.get(file) {
            /* if let Some(_symbol) =
                environment::hover(&params.text_document_position.position, &file_scope)
            {
            } else {
                eprintln!("Symbol not found");
            }*/
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

        if let Some(diagnostics) = self.diagnostics.lock().await.get(path) {
            if diagnostics.is_empty() {
                return;
            }

            client.publish_diagnostics(
                params.text_document.uri,
                diagnostics.clone(),
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

        if let Some(diagnostics) = self.diagnostics.lock().await.get(path) {
            if diagnostics.is_empty() {
                return;
            }

            client.publish_diagnostics(
                params.text_document.uri,
                diagnostics.clone(),
                Some(params.text_document.version),
            );
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let file = uri.path();
        let arena = self.arena.lock().await;
        let root_scopes = self.root_scopes.lock().await;

        if let Some(node_id) = root_scopes.get(file) {
            let global_symbols = self.global_symbols.lock().await;

            if let Some(string) = environment::hover(
                &uri,
                &arena,
                node_id,
                &params.text_document_position_params.position,
                &global_symbols,
                &root_scopes,
            ) {
                return Ok(Some(Hover {
                    contents: HoverContents::Scalar(MarkedString::String(string)),
                    range: None,
                }));
            }
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

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let mut arena = Arena::new();
    let global_scope = arena.new_node(Scope::default());

    let backend = Backend {
        arena: Arc::new(Mutex::new(arena)),
        global_scope: Arc::new(Mutex::new(global_scope)),
        global_symbols: Arc::new(Mutex::new(HashMap::new())),
        root_scopes: Arc::new(Mutex::new(HashMap::new())),
        diagnostics: Arc::new(Mutex::new(HashMap::new())),
    };

    let (service, messages) = LspService::new(backend);
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
