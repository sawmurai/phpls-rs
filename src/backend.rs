use crate::environment;
use crate::environment::in_range;
use crate::environment::symbol::{PhpSymbolKind, Symbol};
use crate::environment::traverser::traverse;
use crate::environment::visitor::name_resolver::Reference;
use crate::environment::visitor::name_resolver::{NameResolveVisitor, NameResolver};
use crate::environment::visitor::workspace_symbol::WorkspaceSymbolVisitor;
use crate::node::get_range;
use crate::node::Node as AstNode;
use crate::parser::Error as ParserError;
use crate::parser::Parser;
use crate::scanner::Scanner;
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::{self};
use tokio::sync::Mutex;
use tokio::task;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

type AstMutex = Arc<Mutex<HashMap<String, (Vec<AstNode>, Range)>>>;
type ArenaMutex = Arc<Mutex<Arena<Symbol>>>;
type NodeMapMutex = Arc<Mutex<HashMap<String, NodeId>>>;
type DiagnosticsMutex = Arc<Mutex<HashMap<String, Vec<Diagnostic>>>>;
type ReferenceMapMutex = Arc<Mutex<HashMap<String, Vec<Reference>>>>;

/// Represents the backend of the language server.
pub struct Backend {
    /// Storage arena for all symbols
    pub arena: ArenaMutex,

    /// NodeIds of entry points to files
    pub files: NodeMapMutex,

    /// FQDN to NodeId
    pub global_symbols: NodeMapMutex,

    /// Global list of all diagnostics
    pub diagnostics: DiagnosticsMutex,

    /// References to symbols. Key is the filename
    pub symbol_references: ReferenceMapMutex,

    /// List of currently opened files and their AST
    pub opened_files: AstMutex,
}

impl Backend {
    pub fn new() -> Self {
        Backend {
            arena: Arc::new(Mutex::new(Arena::new())),
            files: Arc::new(Mutex::new(HashMap::new())),
            global_symbols: Arc::new(Mutex::new(HashMap::new())),
            diagnostics: Arc::new(Mutex::new(HashMap::new())),
            symbol_references: Arc::new(Mutex::new(HashMap::new())),
            opened_files: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    async fn init_workspace(&self, url: &Url) -> io::Result<()> {
        // Index stdlib
        let mut files = self.reindex_folder(&PathBuf::from(
            "/home/sawmurai/develop/rust/phpls-rs/fixtures/phpstorm-stubs",
        ))?;

        let mut joins = Vec::new();
        if let Ok(root_path) = url.to_file_path() {
            files.extend(self.reindex_folder(&root_path)?);

            for path in files {
                let content = match fs::read_to_string(&path) {
                    Ok(content) => content,
                    Err(error) => {
                        eprintln!("{}", error);

                        continue;
                    }
                };

                let arena = self.arena.clone();
                let global_symbols = self.global_symbols.clone();
                let files = self.files.clone();
                let references = self.symbol_references.clone();
                let diagnostics = self.diagnostics.clone();

                joins.push(task::spawn(async move {
                    if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
                        let reindex_result = Backend::reindex(
                            &path,
                            &ast,
                            &range,
                            false,
                            true,
                            arena,
                            global_symbols,
                            references,
                            files,
                            diagnostics.clone(),
                        )
                        .await;

                        match reindex_result {
                            Ok(()) => (),
                            Err(err) => {
                                eprintln!("{}", err);
                            }
                        }

                        let diags = errors.iter().map(|e| Diagnostic::from(e)).collect();
                        diagnostics.lock().await.insert(path, diags);
                    } else {
                        // TODO: Publish errors as diagnostics
                        eprintln!("Could not index {} due to syntax errors", path);
                    }
                }));
            }
        } else {
            eprintln!("Error converting url to file path");
        }

        for j in joins {
            match j.await {
                Ok(()) => (),
                Err(err) => {
                    eprintln!("{}", err);
                }
            }
        }

        let arena = self.arena.lock().await;
        let files = self.files.lock().await;

        let mut global_table: HashMap<String, NodeId> = HashMap::new();
        for (_file, node_id) in files.iter() {
            let mut current_namespace = String::new();

            for symbol_id in node_id.children(&arena) {
                let symbol = arena[symbol_id].get();

                if symbol.kind == PhpSymbolKind::Namespace {
                    current_namespace = symbol.name.clone();
                } else if symbol.kind.register_global() {
                    global_table
                        .insert(format!("{}\\{}", current_namespace, symbol.name), symbol_id);
                }
            }
        }

        *self.global_symbols.lock().await = global_table;

        Ok(())
    }

    // TODO: Filter out vendor tests
    fn reindex_folder(&self, dir: &PathBuf) -> io::Result<Vec<String>> {
        let mut files = Vec::new();

        if dir.is_dir() {
            let entries = match fs::read_dir(dir) {
                Ok(entries) => entries,
                Err(e) => {
                    eprintln!("Error reading folder {:?}: {}", dir, e);

                    return Ok(files);
                }
            };

            for entry in entries {
                let entry = match entry {
                    Ok(entry) => entry,
                    Err(e) => {
                        eprintln!("- Error reading folder {:?}: {}", dir, e);

                        return Ok(files);
                    }
                };
                let path = entry.path();
                if path.is_dir() {
                    // && !path.ends_with("vendor") {
                    files.extend(self.reindex_folder(&path)?);
                } else if let Some(ext) = path.extension() {
                    if ext == "php" {
                        let p = path.to_str().unwrap().to_string();
                        files.push(p);
                    }
                }
            }
        }
        Ok(files)
    }

    /// Index a source string to an ast
    fn source_to_ast(
        source: &str,
    ) -> std::result::Result<(Vec<AstNode>, Range, Vec<ParserError>), String> {
        let mut scanner = Scanner::new(&source);

        if let Err(msg) = scanner.scan() {
            eprintln!();

            return Err(format!("Could not read file: {}", &msg));
        }

        let range = get_range(scanner.document_range());

        if let Ok((ast, errors)) = Parser::ast(scanner.tokens) {
            Ok((ast, range, errors))
        } else {
            Err(String::from("dd"))
        }
    }

    async fn reindex(
        path: &str,
        ast: &Vec<AstNode>,
        range: &Range,
        resolve: bool,
        collect: bool,
        arena: ArenaMutex,
        global_symbols: NodeMapMutex,
        symbol_references: ReferenceMapMutex,
        files: NodeMapMutex,
        diagnostics: DiagnosticsMutex,
    ) -> io::Result<()> {
        //eprintln!("{:#?}", ast);
        let mut arena = arena.lock().await;

        let enclosing_file = if collect {
            let enclosing_file = arena.new_node(Symbol {
                kind: PhpSymbolKind::File,
                name: path.to_owned(),
                range: range.clone(),
                selection_range: range.clone(),
                ..Symbol::default()
            });

            let mut visitor = WorkspaceSymbolVisitor::new();
            let mut iter = ast.iter();
            while let Some(node) = iter.next() {
                traverse(node, &mut visitor, &mut arena, enclosing_file);
            }

            let mut global_symbols = global_symbols.lock().await;
            let mut current_namespace = String::new();

            // Deregister old children
            if let Some(old_enclosing) = files.lock().await.insert(path.to_owned(), enclosing_file)
            {
                for symbol_id in old_enclosing.children(&arena) {
                    let symbol = arena[symbol_id].get();

                    if symbol.kind == PhpSymbolKind::Namespace {
                        current_namespace = symbol.name.clone();
                    } else if symbol.kind.register_global() {
                        global_symbols.remove(&format!("{}\\{}", current_namespace, symbol.name));
                    }
                }

                old_enclosing.remove(&mut arena);
            }

            // and register new children
            let mut current_namespace = String::new();

            for symbol_id in enclosing_file.children(&arena) {
                let symbol = arena[symbol_id].get();

                if symbol.kind == PhpSymbolKind::Namespace {
                    current_namespace = symbol.name.clone();
                } else if symbol.kind.register_global() {
                    global_symbols
                        .insert(format!("{}\\{}", current_namespace, symbol.name), symbol_id);
                }
            }

            enclosing_file
        } else if let Some(enclosing_file) = files.lock().await.get(path) {
            *enclosing_file
        } else {
            // TODO: refactor to return an actual error
            return Ok(());
        };

        if resolve {
            let global_symbols = global_symbols.lock().await;
            let mut resolver = NameResolver::new(&global_symbols, enclosing_file);

            let mut visitor = NameResolveVisitor::new(&mut resolver);
            let mut iter = ast.iter();
            while let Some(node) = iter.next() {
                traverse(node, &mut visitor, &mut arena, enclosing_file);
            }

            symbol_references
                .lock()
                .await
                .insert(path.to_owned(), visitor.references());
        }
        /*
                diagnostics.lock().await.insert(
                    path.to_owned(),
                    errors
                        .iter()
                        .map(Diagnostic::from)
                        .collect::<Vec<Diagnostic>>(),
                );
        */
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
            eprintln!("{:?}", url);
            match self.init_workspace(&url).await {
                Ok(()) => {
                    eprintln!("Indexed root");
                }
                Err(e) => {
                    eprintln!("Something broke: {}", e);
                }
            };
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
        let mut diagnostics = self.diagnostics.lock().await;

        for (file, diagnostics) in diagnostics.iter() {
            if
            /*file.contains("/vendor/")
            || file.contains("/phpstorm-stubs/")
            || */
            diagnostics.is_empty() {
                continue;
            }

            client.publish_diagnostics(
                Url::from_file_path(file).unwrap(),
                diagnostics.clone(),
                None,
            );
        }

        diagnostics.clear();
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        if params.query == "" {
            return Ok(None);
        }

        let query = params.query.to_lowercase();

        let mut symbols = Vec::new();
        let arena = self.arena.lock().await;

        for (file_name, node) in self.files.lock().await.iter() {
            for symbol in node.descendants(&arena) {
                let symbol = arena[symbol].get();

                if symbol.name.to_lowercase().starts_with(&query) {
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

        if let Some(node_id) = self.files.lock().await.get(path) {
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

        if let Some(node_id) = self.files.lock().await.get(file) {
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

        if let Some(_arena) = self.files.lock().await.get(file) {
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

        let local_references = self.symbol_references.lock().await;

        let arena = self.arena.lock().await;

        let position = &params.text_document_position_params.position;

        if let Some(references) = local_references.get(file) {
            for reference in references {
                if in_range(position, &get_range(reference.range)) {
                    if let Some(location) = environment::symbol_location(&arena, &reference.node) {
                        return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                    }
                }
            }
        }

        Ok(None)
    }

    async fn did_save(&self, client: &Client, params: DidSaveTextDocumentParams) {
        let path = params.text_document.uri.path();

        let arena = self.arena.clone();
        let global_symbols = self.global_symbols.clone();
        let files = self.files.clone();
        let references = self.symbol_references.clone();
        let diagnostics = self.diagnostics.clone();
        let content = tokio::fs::read_to_string(path).await.unwrap();

        if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
            let reindex_result = Backend::reindex(
                path,
                &ast,
                &range,
                false,
                true,
                arena.clone(),
                global_symbols.clone(),
                references.clone(),
                files.clone(),
                diagnostics.clone(),
            )
            .await;

            let diags = errors.iter().map(|e| Diagnostic::from(e)).collect();
            diagnostics.lock().await.insert(path.to_string(), diags);

            self.opened_files
                .lock()
                .await
                .insert(path.to_string(), (ast, range));

            if let Err(e) = reindex_result {
                client.log_message(MessageType::Error, e);

                return;
            }
        }

        // Now reindex all the other current files to update references
        for (path, (ast, range)) in self.opened_files.lock().await.iter() {
            match Backend::reindex(
                path,
                &ast,
                &range,
                true,
                false,
                arena.clone(),
                global_symbols.clone(),
                references.clone(),
                files.clone(),
                diagnostics.clone(),
            )
            .await
            {
                Err(err) => eprintln!("Error updating opened file after save of another: {}", err),
                _ => (),
            }
        }

        if let Some(diagnostics) = self.diagnostics.lock().await.get(path) {
            // TODO: Add resolver here and collect unresolvable for diagnostics
            if
            /*file.contains("/vendor/")
            || file.contains("/phpstorm-stubs/")
            || */
            diagnostics.is_empty() {
                return;
            }

            client.publish_diagnostics(params.text_document.uri, diagnostics.clone(), None);
        }
    }

    async fn did_close(&self, _client: &Client, params: DidCloseTextDocumentParams) {
        self.opened_files
            .lock()
            .await
            .remove(params.text_document.uri.path());
        self.symbol_references
            .lock()
            .await
            .remove(params.text_document.uri.path());
    }

    async fn did_open(&self, client: &Client, params: DidOpenTextDocumentParams) {
        let path = params.text_document.uri.path();
        let arena = self.arena.clone();
        let global_symbols = self.global_symbols.clone();
        let files = self.files.clone();
        let references = self.symbol_references.clone();
        let diagnostics = self.diagnostics.clone();
        let mut opened_files = self.opened_files.lock().await;

        if !opened_files.contains_key(path) {
            client.log_message(MessageType::Info, "Need to freshly index");

            let source = tokio::fs::read_to_string(path).await.unwrap();
            if let Ok((ast, range, errors)) = Backend::source_to_ast(&source) {
                let diags = errors.iter().map(|e| Diagnostic::from(e)).collect();
                diagnostics.lock().await.insert(path.to_owned(), diags);
                opened_files.insert(path.to_string(), (ast, range));
            } else {
                client.log_message(MessageType::Error, "Error indexing");

                return;
            }
        }

        let (ast, range) = if let Some((ast, range)) = opened_files.get(path) {
            client.log_message(MessageType::Info, "Opened from cache");
            (ast, range)
        } else {
            client.log_message(MessageType::Error, "Error storing reindexed");

            return;
        };

        if let Err(e) = Backend::reindex(
            path,
            ast,
            range,
            true,
            false,
            arena,
            global_symbols,
            references,
            files,
            diagnostics,
        )
        .await
        {
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
        let file = params
            .text_document_position_params
            .text_document
            .uri
            .path();
        let local_references = self.symbol_references.lock().await;
        let arena = self.arena.lock().await;
        let position = &params.text_document_position_params.position;

        if let Some(references) = local_references.get(file) {
            for reference in references {
                if in_range(position, &get_range(reference.range)) {
                    let symbol = arena[reference.node].get();

                    return Ok(Some(Hover {
                        range: Some(get_range(reference.range)),
                        contents: HoverContents::Scalar(MarkedString::String(
                            symbol.hover_text(&arena, &reference.node),
                        )),
                    }));
                }
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

    #[tokio::test]
    async fn test_resolves_named_constructor() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/small/oop", base_dir.display());
        let file = format!(
            "{}/fixtures/small/oop/named_constructor.php",
            base_dir.display()
        );

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        let diagnostics = diagnostics.get(&file).unwrap();
        assert_eq!(true, diagnostics.is_empty());
    }

    #[tokio::test]
    async fn test_resolves_inherited_members() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/small/oop", base_dir.display());
        let file = format!("{}/fixtures/small/oop/inheritance.php", base_dir.display());

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        let diagnostics = diagnostics.get(&file).unwrap();

        println!("{:?}", diagnostics);
        assert_eq!(true, diagnostics.is_empty());
    }

    #[tokio::test]
    async fn test_handles_unresolvable() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/projects/invalid", base_dir.display());
        let file = format!(
            "{}/fixtures/projects/invalid/unresolvable.php",
            base_dir.display()
        );

        let backend = Backend::new();
        let uri = Url::from_file_path(root).unwrap();
        backend.init_workspace(&uri).await.unwrap();

        let diagnostics = backend.diagnostics.lock().await;
        let diagnostics = diagnostics.get(&file).unwrap();
        assert_eq!(false, diagnostics.is_empty());
    }
}
