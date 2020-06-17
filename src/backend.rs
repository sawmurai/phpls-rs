use crate::environment;
use crate::environment::scope::collect_symbols;
use crate::environment::symbol::{PhpSymbolKind, Symbol};
use crate::node::{get_range, Node};
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
use tower_lsp::{Client, LanguageServer};

/// Represents the backend of the language server.
pub struct Backend {
    /// Storage arena for all symbols
    pub arena: Arc<Mutex<Arena<Symbol>>>,

    /// NodeIds of entry points to files
    pub root_symbols: Arc<Mutex<HashMap<String, NodeId>>>,

    /// FQDN to NodeId
    pub global_symbols: Arc<Mutex<HashMap<String, NodeId>>>,

    /// Global list of all diagnostics
    pub diagnostics: Arc<Mutex<HashMap<String, Vec<Diagnostic>>>>,
}

impl Backend {
    pub fn new() -> Self {
        Backend {
            arena: Arc::new(Mutex::new(Arena::new())),
            root_symbols: Arc::new(Mutex::new(HashMap::new())),
            global_symbols: Arc::new(Mutex::new(HashMap::new())),
            diagnostics: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    async fn init_workspace(&self, url: &Url) -> io::Result<()> {
        // Index stdlib
        //self.reindex_folder(&PathBuf::from(
        //    "/home/sawmurai/develop/rust/phpls-rs/fixtures/phpstorm-stubs",
        //))
        //.await?;

        if let Ok(root_path) = url.to_file_path() {
            self.reindex_folder(&root_path).await?;
        }

        let arena = self.arena.lock().await;
        let mut diagnostics = self.diagnostics.lock().await;
        let root_symbols = self.root_symbols.lock().await;

        let mut global_table: HashMap<String, NodeId> = HashMap::new();
        for (_file, node_id) in root_symbols.iter() {
            for symbol_id in node_id.children(&arena) {
                let symbol = arena[symbol_id].get();

                if symbol.kind == PhpSymbolKind::Namespace {
                    let base_name = symbol.name.clone();

                    for symbol_id in symbol_id.children(&arena) {
                        let symbol = arena[symbol_id].get();

                        if symbol.kind != PhpSymbolKind::Unknown {
                            global_table
                                .insert(format!("{}\\{}", base_name, symbol.name), symbol_id);
                        }
                    }
                } else {
                    if symbol.kind != PhpSymbolKind::Unknown {
                        global_table.insert(symbol.name.clone(), symbol_id);
                    }
                }
            }
        }

        for (file, node_id) in root_symbols.iter() {
            for symbol_node in node_id.descendants(&arena) {
                let symbol = arena[symbol_node].get();

                if symbol.kind == PhpSymbolKind::Unknown
                    && symbol
                        .resolve(&arena, &symbol_node, &global_table)
                        .is_none()
                {
                    diagnostics
                        .get_mut(file)
                        .unwrap()
                        .push(Diagnostic::from(symbol));
                }
            }
        }

        *self.global_symbols.lock().await = global_table;

        Ok(())
    }

    #[async_recursion]
    async fn reindex_folder(&self, dir: &PathBuf) -> io::Result<()> {
        if dir.is_dir() {
            for entry in fs::read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_dir() {
                    // && !path.ends_with("vendor") {
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
            //eprintln!("{:#?}", ast);
            let mut arena = self.arena.lock().await;
            let range = get_range(scanner.document_range());

            let enclosing_file = arena.new_node(Symbol {
                name: path.to_owned(),
                kind: PhpSymbolKind::File,
                range,
                selection_range: range,
                detail: None,
                deprecated: None,
                inherits_from: Vec::new(),
                parent: None,
                references: None,
                references_by: Vec::new(),
                data_types: Vec::new(),
                is_static: false,
                imports: None,
            });

            // By default, put all symbols in the file scope
            let mut current_parent = enclosing_file;

            let mut iter = ast.iter();
            while let Some(node) = iter.next() {
                match node {
                    // Once we detect a namespace statement we consider all following definitions to be
                    // within the namespaces symbol
                    Node::NamespaceStatement { type_ref, .. } => {
                        let (name, selection_range) = match &**type_ref {
                            Node::TypeRef(tokens) => (
                                tokens
                                    .iter()
                                    .map(|n| n.clone().label.unwrap_or_else(|| "\\".to_owned()))
                                    .collect::<Vec<String>>()
                                    .join(""),
                                // Range from first part of name until the last one
                                get_range((
                                    tokens.first().unwrap().range().0,
                                    tokens.last().unwrap().range().1,
                                )),
                            ),
                            _ => panic!("This should not happen"),
                        };

                        current_parent = arena.new_node(Symbol {
                            name,
                            kind: PhpSymbolKind::Namespace,
                            range,
                            selection_range,
                            detail: None,
                            deprecated: None,
                            inherits_from: Vec::new(),
                            parent: None,
                            references: None,
                            references_by: Vec::new(),
                            data_types: Vec::new(),
                            is_static: false,
                            imports: None,
                        });

                        enclosing_file.append(current_parent, &mut arena);
                    }
                    _ => {
                        if let Err(e) = collect_symbols(&mut arena, &current_parent, &node) {
                            eprintln!("Mööp!: {} {}", path, e);
                        }
                    }
                }
            }

            self.root_symbols
                .lock()
                .await
                .insert(path.to_owned(), enclosing_file);
            self.diagnostics.lock().await.insert(
                path.to_owned(),
                errors
                    .iter()
                    .map(Diagnostic::from)
                    .collect::<Vec<Diagnostic>>(),
            );
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
                definition_provider: Some(true),
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
        let diagnostics = self.diagnostics.lock().await;

        for (file, diagnostics) in diagnostics.iter() {
            if diagnostics.is_empty() {
                continue;
            }

            client.publish_diagnostics(
                Url::from_file_path(file).unwrap(),
                diagnostics.clone(),
                None,
            );
        }

        //diagnostics.clear();
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let mut symbols = Vec::new();
        let arena = self.arena.lock().await;

        for (file_name, node) in self.root_symbols.lock().await.iter() {
            for symbol in node.children(&arena) {
                let symbol = arena[symbol].get();

                if params.query != "" && symbol.name.starts_with(&params.query) {
                    if let Some(kind) = symbol.kind.to_symbol_kind() {
                        symbols.push(SymbolInformation {
                            name: symbol.name.clone(),
                            deprecated: symbol.deprecated,
                            kind,
                            location: Location {
                                uri: Url::from_file_path(&file_name).unwrap(),
                                range: symbol.range,
                            },
                            container_name: None,
                        })
                    }
                }
            }
        }

        Ok(Some(symbols))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let path = params.text_document.uri.path();
        let arena = self.arena.lock().await;

        if let Some(node_id) = self.root_symbols.lock().await.get(path) {
            return Ok(Some(DocumentSymbolResponse::Nested(
                node_id
                    .children(&arena)
                    .map(|s| arena[s].get().to_doc_sym(&arena, &s))
                    .filter(|s| s.is_some())
                    .map(|s| s.unwrap())
                    .collect(),
            )));
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

        if let Some(node_id) = self.root_symbols.lock().await.get(file) {
            return Ok(environment::document_highlights(
                &params.text_document_position_params.position,
                &arena,
                node_id,
            ));
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let file = params.text_document_position.text_document.uri.path();

        if let Some(_arena) = self.root_symbols.lock().await.get(file) {
        } else {
            eprintln!("File not found");
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let file = uri.path();
        let arena = self.arena.lock().await;
        let root_symbols = self.root_symbols.lock().await;
        let global_symbols = self.global_symbols.lock().await;

        if let Some(node_id) = root_symbols.get(file) {
            if let Some(location) = environment::definition_of_symbol_under_cursor(
                &arena,
                node_id,
                &params.text_document_position_params.position,
                &global_symbols,
            ) {
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
        }

        Ok(None)
    }

    async fn did_change(&self, client: &Client, params: DidChangeTextDocumentParams) {
        let path = params.text_document.uri.path();

        if let Err(e) = self.reindex(path, &params.content_changes[0].text).await {
            client.log_message(MessageType::Error, e);

            return;
        }

        let root_symbols = self.root_symbols.lock().await;
        let global_table = self.global_symbols.lock().await;
        let arena = self.arena.lock().await;

        if let Some(node_id) = root_symbols.get(path) {
            let mut diagnostics = Vec::new();

            for symbol_node in node_id.descendants(&arena) {
                let symbol = arena[symbol_node].get();

                if symbol.kind == PhpSymbolKind::Unknown
                    && symbol
                        .resolve(&arena, &symbol_node, &global_table)
                        .is_none()
                {
                    diagnostics.push(Diagnostic::from(symbol));
                }
            }

            client.publish_diagnostics(
                params.text_document.uri,
                diagnostics,
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

        let diagnostics = self.diagnostics.lock().await;

        if let Some(diagnostics) = diagnostics.get(path) {
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
        let root_symbols = self.root_symbols.lock().await;
        let global_symbols = self.global_symbols.lock().await;

        if let Some(node_id) = root_symbols.get(file) {
            if let Some(string) = environment::hover(
                &arena,
                node_id,
                &params.text_document_position_params.position,
                &global_symbols,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_initialized_workspace() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/projects/valid", base_dir.display());
        let file = format!("{}/fixtures/projects/valid/base.php", base_dir.display());

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        assert_eq!(true, diagnostics.get(&file).unwrap().is_empty());
    }

    #[tokio::test]
    async fn test_knows_stdlib() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/projects/stdlib", base_dir.display());
        let file = format!("{}/fixtures/projects/stdlib/index.php", base_dir.display());

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        assert_eq!(true, diagnostics.get(&file).unwrap().is_empty());
    }

    #[tokio::test]
    async fn test_handles_fluent_interfaces() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/small", base_dir.display());
        let file = format!("{}/fixtures/small/fluent-interface.php", base_dir.display());

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        let diagnostics = diagnostics.get(&file).unwrap();
        assert_eq!(true, diagnostics.is_empty());
    }

    #[tokio::test]
    async fn test_detects_class_within_grouping() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/small", base_dir.display());
        let file = format!("{}/fixtures/small/grouping.php", base_dir.display());

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        let diagnostics = diagnostics.get(&file).unwrap();
        assert_eq!(true, diagnostics.is_empty());
    }

    #[tokio::test]
    async fn test_detects_class_of_assigned_variable() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/small/assignment", base_dir.display());
        let file = format!(
            "{}/fixtures/small/assignment/assigned-object.php",
            base_dir.display()
        );

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        let diagnostics = diagnostics.get(&file).unwrap();
        eprintln!("{:#?}", diagnostics);
        assert_eq!(true, diagnostics.is_empty());
    }

    #[tokio::test]
    async fn test_resolves_method_of_static_parent() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/small/parent", base_dir.display());
        let file = format!("{}/fixtures/small/parent/parent.php", base_dir.display());

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        let diagnostics = diagnostics.get(&file).unwrap();
        eprintln!("{:#?}", diagnostics);
        assert_eq!(true, diagnostics.is_empty());
    }
}
