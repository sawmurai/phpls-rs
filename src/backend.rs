use crate::environment;
use crate::environment::get_range;
use crate::environment::in_range;
use crate::environment::symbol::{PhpSymbolKind, Symbol};
use crate::environment::traverser::traverse;
use crate::environment::visitor::name_resolver::Reference;
use crate::environment::visitor::name_resolver::{NameResolveVisitor, NameResolver};
use crate::environment::visitor::workspace_symbol::WorkspaceSymbolVisitor;
use crate::parser::node::Node as AstNode;
use crate::parser::scanner::Scanner;
use crate::parser::token::{Token, TokenType};
use crate::parser::Error as ParserError;
use crate::parser::Parser;
use crate::suggester;
use crate::suggester::suggest_variables;
use indextree::{Arena, NodeId};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::{self};
use tokio::sync::Mutex;
use tokio::task;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentHighlight, DocumentHighlightParams, DocumentSymbolParams,
    DocumentSymbolResponse, ExecuteCommandOptions, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverContents, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, Location, MarkedString, MessageType, Position, Range, ReferenceParams,
    ServerCapabilities, SymbolInformation, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    WorkspaceCapability, WorkspaceFolderCapability, WorkspaceFolderCapabilityChangeNotifications,
    WorkspaceSymbolParams,
};
use tower_lsp::{jsonrpc::Result, lsp_types::DidChangeTextDocumentParams};
use tower_lsp::{Client, LanguageServer};

type AstMutex = Arc<Mutex<HashMap<String, (Vec<AstNode>, Range)>>>;
type ArenaMutex = Arc<Mutex<Arena<Symbol>>>;
pub(crate) type NodeMapMutex = Arc<Mutex<HashMap<String, NodeId>>>;
type DiagnosticsMutex = Arc<Mutex<HashMap<String, Vec<Diagnostic>>>>;
type ReferenceMapMutex = Arc<Mutex<HashMap<String, Vec<Reference>>>>;
type InFlightFileMutex = Arc<Mutex<HashMap<String, String>>>;

/// Represents the backend of the language server.
pub struct Backend {
    /// LSP client
    client: Client,

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

    /// Map of latest edits on files
    pub latest_version_of_file: InFlightFileMutex,

    /// Path to stdlib
    stdlib: String,
}

impl From<&ParserError> for Diagnostic {
    fn from(e: &ParserError) -> Diagnostic {
        match e {
            ParserError::WrongTokenError { token, expected } => Diagnostic {
                range: get_range(token.range()),
                message: format!("Wrong token {:?}, expected one of {:?}", token.t, expected),
                ..Diagnostic::default()
            },
            ParserError::UnexpectedTokenError { token, .. } => Diagnostic {
                range: get_range(token.range()),
                message: format!("Unexpected token {:?}", token.t),
                ..Diagnostic::default()
            },
            ParserError::IllegalOffsetType { expr, .. } => Diagnostic {
                range: get_range(expr.range()),
                message: "Illegal offset type".to_owned(),
                ..Diagnostic::default()
            },
            ParserError::RValueInWriteContext { token, .. } => Diagnostic {
                range: get_range(token.range()),
                message: "Can not use expression in write context".to_owned(),
                ..Diagnostic::default()
            },
            ParserError::Eof => Diagnostic {
                range: get_range(((0, 0), (0, 0))),
                message: "Unexpected end of file".to_owned(),
                ..Diagnostic::default()
            },
        }
    }
}

impl From<&Token> for Symbol {
    fn from(token: &Token) -> Symbol {
        let start = token.start();
        let end = token.end();

        let range = Range {
            start: Position {
                line: u64::from(start.0),
                character: u64::from(start.1),
            },
            end: Position {
                line: u64::from(end.0),
                character: u64::from(end.1),
            },
        };

        let kind = match token.t {
            TokenType::Variable => PhpSymbolKind::Variable,
            _ => PhpSymbolKind::Unknown,
        };

        Symbol {
            name: token.clone().label.unwrap_or_else(|| "Unknown".to_owned()),
            kind,
            range,
            selection_range: range,
            ..Symbol::default()
        }
    }
}

fn normalize_path(path: &PathBuf) -> String {
    path.to_str().unwrap().to_owned()
}

impl Backend {
    pub fn new(client: Client, stdlib: String) -> Self {
        Backend {
            client,
            arena: Arc::new(Mutex::new(Arena::new())),
            files: Arc::new(Mutex::new(HashMap::new())),
            global_symbols: Arc::new(Mutex::new(HashMap::new())),
            diagnostics: Arc::new(Mutex::new(HashMap::new())),
            symbol_references: Arc::new(Mutex::new(HashMap::new())),
            opened_files: Arc::new(Mutex::new(HashMap::new())),
            latest_version_of_file: Arc::new(Mutex::new(HashMap::new())),
            stdlib,
        }
    }

    async fn init_workspace(&self, url: &Url) -> io::Result<()> {
        // Index stdlib
        let mut files = self.reindex_folder(&PathBuf::from(&self.stdlib))?;

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
                    let p = normalize_path(&path);

                    if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
                        let reindex_result = Backend::reindex(
                            &p,
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

                        let diags = errors.iter().map(Diagnostic::from).collect();
                        diagnostics.lock().await.insert(p, diags);
                    } else {
                        // TODO: Publish errors as diagnostics
                        eprintln!("Could not index {} due to syntax errors", p);
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
    fn reindex_folder(&self, dir: &PathBuf) -> io::Result<Vec<PathBuf>> {
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
                        files.push(path);
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
        ast: &[AstNode],
        range: &Range,
        resolve: bool,
        collect: bool,
        arena: ArenaMutex,
        global_symbols: NodeMapMutex,
        symbol_references: ReferenceMapMutex,
        files: NodeMapMutex,
        diagnostics: DiagnosticsMutex,
    ) -> io::Result<()> {
        let mut arena = arena.lock().await;

        let enclosing_file = if collect {
            let enclosing_file = arena.new_node(Symbol {
                kind: PhpSymbolKind::File,
                name: path.to_owned(),
                range: *range,
                selection_range: *range,
                ..Symbol::default()
            });

            let mut visitor = WorkspaceSymbolVisitor {};
            let iter = ast.iter();
            for node in iter {
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
            let iter = ast.iter();
            for node in iter {
                traverse(node, &mut visitor, &mut arena, enclosing_file);
            }

            symbol_references
                .lock()
                .await
                .insert(path.to_owned(), visitor.references());

            let mut diagnostics = diagnostics.lock().await;

            for notification in visitor.diagnostics().iter() {
                diagnostics
                    .entry(notification.0.clone())
                    .or_insert_with(Vec::new)
                    .push(Diagnostic {
                        range: get_range(notification.1),
                        message: notification.2.clone(),
                        ..Diagnostic::default()
                    })
            }
        }

        Ok(())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Some(url) = params.root_uri {
            match self.init_workspace(&url).await {
                Ok(()) => {
                    eprintln!("Indexed root");
                }
                Err(e) => {
                    eprintln!("Something broke: {}", e);
                }
            };
        }

        let mut trigger_characters = ('a'..'z')
            .into_iter()
            .map(|c| String::from(c))
            .collect::<Vec<String>>();
        trigger_characters.push(String::from("$"));
        trigger_characters.push(String::from("_"));
        trigger_characters.push(String::from("\\"));

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Full,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(trigger_characters),
                    work_done_progress_options: Default::default(),
                }),
                /*signature_help_provider: Some(SignatureHelpOptions {
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

    async fn initialized(&self, _params: InitializedParams) {
        let mut diagnostics = self.diagnostics.lock().await;

        for (file, diagnostics) in diagnostics.iter() {
            if
            /*file.contains("/vendor/")
            || file.contains("/phpstorm-stubs/")
            || */
            diagnostics.is_empty() {
                continue;
            }

            eprintln!("{:?}", Url::from_file_path(file).unwrap());

            self.client
                .publish_diagnostics(
                    Url::from_file_path(file).unwrap(),
                    diagnostics.clone(),
                    None,
                )
                .await;
        }

        diagnostics.clear();
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        if params.query.is_empty() {
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
        let file_path = normalize_path(&params.text_document.uri.to_file_path().unwrap());
        let arena = self.arena.lock().await;

        if let Some(node_id) = self.files.lock().await.get(&file_path) {
            return Ok(Some(DocumentSymbolResponse::Nested(
                node_id
                    .children(&arena)
                    .map(|s| arena[s].get().to_doc_sym(&arena, &s))
                    .filter(std::option::Option::is_some)
                    .map(std::option::Option::unwrap)
                    .collect(),
            )));
        }

        Ok(None)
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let file = normalize_path(
            &params
                .text_document_position_params
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
        );
        let arena = self.arena.lock().await;

        if let Some(node_id) = self.files.lock().await.get(&file) {
            return Ok(environment::document_highlights(
                &params.text_document_position_params.position,
                &arena,
                node_id,
            ));
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let file = normalize_path(
            &params
                .text_document_position
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
        );

        if let Some(_arena) = self.files.lock().await.get(&file) {
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
        let file = normalize_path(&uri.to_file_path().unwrap());

        let local_references = self.symbol_references.lock().await;

        let arena = self.arena.lock().await;

        let position = &params.text_document_position_params.position;

        if let Some(references) = local_references.get(&file) {
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

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        for change in params.changes {
            let file_path = change.uri.to_file_path().unwrap();
            let path = normalize_path(&file_path);

            let arena = self.arena.clone();
            let global_symbols = self.global_symbols.clone();
            let files = self.files.clone();
            let references = self.symbol_references.clone();
            let diagnostics = self.diagnostics.clone();
            let content = fs::read_to_string(file_path).unwrap();

            if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
                let reindex_result = Backend::reindex(
                    &path,
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

                let mut diagnostics = diagnostics.lock().await;

                let diagnostics = diagnostics.entry(path.to_string()).or_insert_with(Vec::new);
                diagnostics.clear();
                diagnostics.extend(errors.iter().map(Diagnostic::from));

                if let Err(e) = reindex_result {
                    self.client.log_message(MessageType::Error, e).await;

                    return;
                }
            }
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let _ = params;
        let file_path = params.text_document.uri.to_file_path().unwrap();
        let path = normalize_path(&file_path);

        if let Some(changes) = params.content_changes.first() {
            self.latest_version_of_file
                .lock()
                .await
                .insert(path, changes.text.clone());
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let file_path = params.text_document.uri.to_file_path().unwrap();
        let path = normalize_path(&file_path);

        let arena = self.arena.clone();
        let global_symbols = self.global_symbols.clone();
        let files = self.files.clone();
        let references = self.symbol_references.clone();
        let diagnostics = self.diagnostics.clone();
        let content = fs::read_to_string(file_path).unwrap();

        if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
            let reindex_result = Backend::reindex(
                &path,
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

            let mut diagnostics = diagnostics.lock().await;

            let diagnostics = diagnostics.entry(path.to_string()).or_insert_with(Vec::new);
            diagnostics.clear();
            diagnostics.extend(errors.iter().map(Diagnostic::from));

            self.opened_files
                .lock()
                .await
                .insert(path.to_string(), (ast.to_owned(), range));

            if let Err(e) = reindex_result {
                self.client.log_message(MessageType::Error, e).await;

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

        if let Some(diagnostics) = self.diagnostics.lock().await.get(&path) {
            self.client
                .publish_diagnostics(params.text_document.uri, diagnostics.clone(), None)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let p = normalize_path(&params.text_document.uri.to_file_path().unwrap());
        self.opened_files.lock().await.remove(&p);
        self.symbol_references.lock().await.remove(&p);
        self.latest_version_of_file.lock().await.remove(&p);
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let file_path = params.text_document.uri.to_file_path().unwrap();
        let path = normalize_path(&file_path);
        let arena = self.arena.clone();
        let global_symbols = self.global_symbols.clone();
        let files = self.files.clone();
        let references = self.symbol_references.clone();
        let diagnostics = self.diagnostics.clone();
        let mut opened_files = self.opened_files.lock().await;

        if !opened_files.contains_key(&path) {
            self.client
                .log_message(MessageType::Info, "Need to freshly index")
                .await;

            let source = fs::read_to_string(file_path).unwrap();
            self.latest_version_of_file
                .lock()
                .await
                .insert(path.clone(), source.clone());

            if let Ok((ast, range, errors)) = Backend::source_to_ast(&source) {
                let diags = errors.iter().map(Diagnostic::from).collect();
                diagnostics.lock().await.insert(path.to_owned(), diags);
                opened_files.insert(path.to_string(), (ast.to_owned(), range));
            } else {
                self.client
                    .log_message(MessageType::Error, "Error indexing")
                    .await;

                return;
            }
        }

        let (ast, range) = if let Some((ast, range)) = opened_files.get(&path) {
            self.client
                .log_message(MessageType::Info, "Opened from cache")
                .await;
            (ast, range)
        } else {
            self.client
                .log_message(MessageType::Error, "Error storing reindexed")
                .await;

            return;
        };

        if let Err(e) = Backend::reindex(
            &path,
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
            self.client
                .log_message(MessageType::Error, format!("Failed to index {}", e))
                .await;

            return;
        }

        let diagnostics = self.diagnostics.lock().await;

        if let Some(diagnostics) = diagnostics.get(&path) {
            self.client
                .publish_diagnostics(
                    params.text_document.uri,
                    diagnostics.clone(),
                    Some(params.text_document.version),
                )
                .await;
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let file = normalize_path(
            &params
                .text_document_position_params
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
        );
        let local_references = self.symbol_references.lock().await;
        let arena = self.arena.lock().await;
        let position = &params.text_document_position_params.position;

        if let Some(references) = local_references.get(&file) {
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

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let global_symbols = self.global_symbols.lock().await;

        let opened_file = normalize_path(
            &params
                .text_document_position
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
        );
        let file_ast = Backend::source_to_ast(
            self.latest_version_of_file
                .lock()
                .await
                .get(&opened_file)
                .unwrap_or(&String::new()),
        );

        let trigger = if let Some(context) = params.context {
            if let Some(tc) = context.trigger_character {
                tc.chars().nth(0)
            } else {
                None
            }
        } else {
            None
        };

        let mut pos = params.text_document_position.position;
        pos.character -= 1;

        if let Ok(file_ast) = file_ast {
            let ast = file_ast.0;

            let (node, ancestors) = if let Some((node, ancestors)) = ast
                .iter()
                .filter_map(|n| suggester::find(n, &pos, Vec::new()))
                .nth(0)
            {
                (node, ancestors)
            } else {
                return Ok(None);
            };

            eprintln!("{:?}", node);

            let mut suggestions = Vec::new();

            match trigger {
                Some('$') => suggestions.extend(suggest_variables(ancestors)),
                None => {
                    suggestions.extend(suggest_variables(ancestors));
                    suggestions.extend(
                        global_symbols
                            .keys()
                            .map(|s| s.clone())
                            .filter(|name| name.starts_with(&node.name()))
                            .collect::<Vec<String>>(),
                    )
                }
                _ => suggestions.extend(
                    global_symbols
                        .keys()
                        .into_iter()
                        .map(|s| s.clone())
                        .filter(|name| name.starts_with(&node.name()))
                        .collect::<Vec<String>>(),
                ),
            };

            Ok(Some(CompletionResponse::Array(
                suggestions
                    .drain(0..)
                    .map(|s| CompletionItem::new_simple(s, "Some detail".to_string()))
                    .collect(),
            )))
        } else {
            Ok(None)
        }
    }
}

/*

Disabled for now since Backend expects a client instance due to the update of tower_lsp

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
*/
