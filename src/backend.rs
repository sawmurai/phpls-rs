use crate::environment;
use crate::environment::fs as EnvFs;
use crate::environment::get_range;
use crate::environment::in_range;
use crate::environment::symbol::{PhpSymbolKind, Symbol};
use crate::environment::traverser::traverse;
use crate::environment::visitor::name_resolver::{NameResolveVisitor, NameResolver};
use crate::environment::visitor::workspace_symbol::WorkspaceSymbolVisitor;
use crate::parser::node::Node as AstNode;
use crate::parser::node::NodeRange;
use crate::parser::scanner::Scanner;
use crate::parser::token::{Token, TokenType};
use crate::parser::Error as ParserError;
use crate::parser::Parser;
use crate::suggester;
use ignore::{types::TypesBuilder, WalkBuilder};
use indextree::{Arena, NodeId};
use std::path::PathBuf;
use std::sync::Arc;
use std::{collections::HashMap, ffi::OsString};
use tokio::runtime::Handle;
use tokio::sync::Mutex;
use tokio::task;
use tokio::{io, task::JoinHandle};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentHighlight, DocumentHighlightParams, DocumentSymbolParams, DocumentSymbolResponse,
    ExecuteCommandOptions, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams,
    Location, MarkupContent, MarkupKind, MessageType, Position, Range, ReferenceParams,
    RenameParams, RenameProviderCapability, ServerCapabilities, SymbolInformation,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url, WorkspaceCapability,
    WorkspaceEdit, WorkspaceFolderCapability, WorkspaceFolderCapabilityChangeNotifications,
    WorkspaceSymbolParams,
};
use tower_lsp::{jsonrpc::Result, lsp_types::DidChangeTextDocumentParams};
use tower_lsp::{Client, LanguageServer};

extern crate crossbeam_channel as channel;
extern crate ignore;
extern crate walkdir;

pub(crate) type FileReferenceMap = HashMap<NodeId, Vec<NodeRange>>;
pub(crate) type ReferenceMap = HashMap<String, FileReferenceMap>;
pub(crate) type ReferenceMapMutex = Arc<Mutex<ReferenceMap>>;
pub(crate) type ParseResult = (String, Vec<AstNode>, Range, Vec<ParserError>);

#[derive(Default)]
pub struct BackendState {
    /// Storage arena for all symbols
    pub arena: Arena<Symbol>,

    /// NodeIds of entry points to files
    pub files: HashMap<String, NodeId>,

    /// FQDN to NodeId
    pub global_symbols: HashMap<String, NodeId>,

    /// Global list of all diagnostics
    pub diagnostics: HashMap<String, Vec<Diagnostic>>,

    /// References to symbols implemented as a HashMap of HashMaps. The
    /// other HashMap has the filename as key, the inner HashMap has the
    /// NodeId of the symbols whose references are stored, as key
    pub symbol_references: ReferenceMap,

    /// List of currently opened files and their AST
    pub opened_files: HashMap<String, (Vec<AstNode>, Range)>,

    /// Map of latest edits on files
    pub latest_version_of_file: HashMap<String, String>,
}

/// Represents the backend of the language server.
pub struct Backend {
    /// LSP client
    client: Client,

    /// The global state of the server
    state: Arc<Mutex<BackendState>>,

    /// Path to stubs
    stubs: String,

    /// Patterns of folders to be ignored during indexing
    ignore_patterns: Vec<PathBuf>,
}

impl From<&ParserError> for Diagnostic {
    fn from(e: &ParserError) -> Diagnostic {
        match e {
            ParserError::MissingIdentifier { token } => Diagnostic {
                range: get_range(token.range()),
                message: "Missing identifier".to_string(),
                ..Diagnostic::default()
            },
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

impl Backend {
    pub fn new(client: Client, stubs: String, ignore_patterns: Vec<String>) -> Self {
        Backend {
            client,
            state: Arc::new(Mutex::new(BackendState::default())),
            stubs,
            ignore_patterns: ignore_patterns.iter().map(PathBuf::from).collect(),
        }
    }

    /// Returns the nodeid and name of the symbol under the cursor
    async fn symbol_under_cursor(
        &self,
        position: &Position,
        file: &str,
    ) -> Option<(NodeId, String)> {
        let state = self.state.lock().await;

        let file = if let Some(node) = state.files.get(file) {
            node.clone()
        } else {
            return None;
        };

        let suc = state.arena[file]
            .get()
            .symbol_at(&position, file, &state.arena);

        Some((suc, state.arena[suc].get().name.clone()))
    }

    async fn references_of_symbol_under_cursor(&self, nuc: &str) -> Option<ReferenceMapMutex> {
        let all_files = self
            .state
            .lock()
            .await
            .files
            .iter()
            .filter_map(|(k, _)| {
                if k.contains("/vendor/") {
                    return None;
                } else {
                    Some(k.clone())
                }
            })
            .collect::<Vec<String>>();
        let symbol_references = Arc::new(Mutex::new(ReferenceMap::new()));

        let mut joins = Vec::new();
        for p in all_files {
            let state = self.state.clone();
            let symbol_references = symbol_references.clone();
            let nuc = nuc.to_owned();

            joins.push(task::spawn(async move {
                let content = match tokio::fs::read_to_string(&p).await {
                    Ok(content) => content,
                    Err(error) => {
                        eprintln!("{}", error);
                        return;
                    }
                };

                // Full text search ....
                if !content.contains(&nuc) {
                    return;
                }

                let mut state = state.lock().await;

                if let Ok((ast, _, _)) = Backend::source_to_ast(&content) {
                    let mut symbol_references = symbol_references.lock().await;
                    let reindex_result = Backend::collect_references(
                        &p,
                        &ast,
                        &mut state,
                        Some(&mut symbol_references),
                    );
                    match reindex_result {
                        Ok(()) => (),
                        Err(err) => {
                            eprintln!("{}", err);
                        }
                    }
                } else {
                    // TODO: Publish errors as diagnostics
                    eprintln!("Could not index {} due to syntax errors", p);
                }
            }));
        }

        for j in joins {
            match j.await {
                Ok(()) => (),
                Err(err) => {
                    eprintln!("{}", err);
                }
            }
        }

        Some(symbol_references)
    }

    async fn init_workspace(&self, url: &Url) -> io::Result<()> {
        let root_path = url.to_file_path().unwrap();
        let (tx, rx) = channel::bounded::<ParseResult>(1000);

        let state = self.state.clone();

        let handle = Handle::current();
        let mt = std::thread::spawn(move || {
            return handle.spawn(async move {
                for (p, ast, range, errors) in rx {
                    let mut state = state.lock().await;

                    let reindex_result = Backend::collect_symbols(&p, &ast, &range, &mut state);

                    match reindex_result {
                        Ok(()) => (),
                        Err(err) => {
                            eprintln!("{}", err);
                        }
                    }

                    let diags = errors.iter().map(Diagnostic::from).collect();
                    state.diagnostics.insert(p, diags);
                }
            });
        });

        let mut type_builder = TypesBuilder::new();
        type_builder.add_def("php:*.php").unwrap();
        let types = type_builder.select("php").build().unwrap();

        let walker = WalkBuilder::new(PathBuf::from(&root_path))
            .standard_filters(false)
            .add(PathBuf::from(&self.stubs))
            //  .git_ignore(false)
            //  .git_exclude(false)
            .types(types)
            .threads(6)
            .build_parallel();

        walker.run(|| {
            let tx = tx.clone();
            Box::new(move |result| {
                use ignore::WalkState::Continue;

                match result {
                    Ok(dent) => {
                        let path = dent.into_path();
                        let content = match std::fs::read_to_string(&path) {
                            Ok(content) => content,
                            Err(error) => {
                                //eprintln!("Error reading file {}", error);

                                return Continue;
                            }
                        };

                        if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
                            match tx.send((
                                EnvFs::normalize_path(&PathBuf::from(path)),
                                ast,
                                range,
                                errors,
                            )) {
                                Err(e) => eprintln!("{:?}", e),
                                _ => (),
                            };
                        }
                    }
                    Err(e) => {
                        eprintln!("Error indexing path {:?}", e);
                        return Continue;
                    }
                }

                Continue
            })
        });

        drop(tx);

        mt.join().unwrap().await?;

        // This is in a block to release the locks asap
        {
            let mut state = self.state.lock().await;
            eprintln!("Indexed {} files ", state.files.len());
            let mut global_table: HashMap<String, NodeId> = HashMap::new();
            for (_file, node_id) in state.files.iter() {
                let mut current_namespace = String::new();

                for symbol_id in node_id.children(&state.arena) {
                    let symbol = state.arena[symbol_id].get();

                    if symbol.kind == PhpSymbolKind::Namespace {
                        current_namespace = symbol.normalized_name();
                    } else if symbol.kind.register_global() {
                        global_table.insert(
                            format!("{}\\{}", current_namespace, symbol.normalized_name()),
                            symbol_id,
                        );
                    }
                }
            }

            state.global_symbols = global_table;
        }

        eprintln!("Done doing the stuff");

        Ok(())
    }

    /// Index a source string to an ast
    pub fn source_to_ast(
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

    pub(crate) fn collect_symbols(
        path: &str,
        ast: &[AstNode],
        range: &Range,
        state: &mut BackendState,
    ) -> io::Result<()> {
        let enclosing_file = state.arena.new_node(Symbol {
            kind: PhpSymbolKind::File,
            name: path.to_owned(),
            range: *range,
            selection_range: *range,
            ..Symbol::default()
        });

        let mut visitor = WorkspaceSymbolVisitor::new();
        for node in ast {
            traverse(node, &mut visitor, &mut state.arena, enclosing_file);
        }

        let mut current_namespace = String::new();

        // Deregister old children from the global symbol table and the references
        if let Some(old_enclosing) = state.files.insert(path.to_owned(), enclosing_file) {
            // Since only the top level symbols are in the global_symbols its okay
            // to shallowy travers the tree
            for symbol_id in old_enclosing.children(&state.arena) {
                let symbol = state.arena[symbol_id].get();

                if symbol.kind == PhpSymbolKind::Namespace {
                    current_namespace = symbol.normalized_name();
                } else if symbol.kind.register_global() {
                    state.global_symbols.remove(&format!(
                        "{}\\{}",
                        current_namespace,
                        symbol.normalized_name()
                    ));
                }
            }

            // But references are a different story
            for symbol_id in old_enclosing.descendants(&state.arena) {
                for (_file, references_of_file) in state.symbol_references.iter_mut() {
                    references_of_file.remove(&symbol_id);
                }
            }

            old_enclosing.remove_subtree(&mut state.arena);
        }

        // and register new children
        let mut current_namespace = String::new();

        for symbol_id in enclosing_file.children(&state.arena) {
            let symbol = state.arena[symbol_id].get();

            if symbol.kind == PhpSymbolKind::Namespace {
                current_namespace = symbol.normalized_name();
            } else if symbol.kind.register_global() {
                state.global_symbols.insert(
                    format!("{}\\{}", current_namespace, symbol.normalized_name()),
                    symbol_id,
                );
            }
        }

        Ok(())
    }

    /// Collect all references withing a given ast
    /// Provide an optional fourth parameter to write the references into a separate
    /// HashMap. This is used for example when filtering for specific references in
    /// a smaller subset of references later on (find references for symbol under cursor)
    pub(crate) fn collect_references(
        path: &str,
        ast: &[AstNode],
        state: &mut BackendState,
        container: Option<&mut ReferenceMap>,
    ) -> io::Result<()> {
        let enclosing_file = if let Some(file) = state.files.get(path) {
            file.clone()
        } else {
            eprintln!("Dafuq! {}", path);

            return Ok(());
        };

        let mut resolver = NameResolver::new(&state.global_symbols, enclosing_file);

        let mut visitor = NameResolveVisitor::new(&mut resolver, enclosing_file);
        let iter = ast.iter();
        for node in iter {
            //eprintln!("[reindex] calling traverse()");
            traverse(node, &mut visitor, &mut state.arena, enclosing_file);
            //eprintln!("[reindex] calling traverse() ended");
        }

        for notification in visitor.diagnostics().iter() {
            state
                .diagnostics
                .entry(notification.file.clone())
                .or_insert_with(Vec::new)
                .push(Diagnostic {
                    range: get_range(notification.range),
                    message: notification.message.clone(),
                    severity: Some(notification.severity),
                    ..Diagnostic::default()
                })
        }

        // Extract all references of the current file only
        let mut map = HashMap::new();
        visitor
            .references()
            .get(&enclosing_file)
            .iter()
            .for_each(|refs| {
                refs.iter().for_each(|reference| {
                    map.entry(reference.node)
                        .or_insert_with(Vec::new)
                        .push(reference.range);
                });
            });

        if let Some(container) = container {
            container.insert(path.to_owned(), map);
        } else {
            state.symbol_references.insert(path.to_owned(), map);
        }

        Ok(())
    }

    async fn refresh_file(&self, uri: Url, src: &str) {
        let file_path = uri.to_file_path().unwrap();
        let path = EnvFs::normalize_path(&file_path);

        let mut state = self.state.lock().await;

        if let Ok((ast, range, errors)) = Backend::source_to_ast(&src) {
            let reindex_result = Backend::collect_symbols(&path, &ast, &range, &mut state);

            let diagnostics = state
                .diagnostics
                .entry(path.to_string())
                .or_insert_with(Vec::new);
            diagnostics.clear();
            diagnostics.extend(errors.iter().map(Diagnostic::from));

            state
                .opened_files
                .insert(path.to_string(), (ast.to_owned(), range));

            if let Err(e) = reindex_result {
                self.client.log_message(MessageType::Error, e).await;

                return;
            }
        }

        // Now reindex all the other currently opened files to update references
        let tasks = state.opened_files.clone();
        for (path, (ast, _)) in tasks {
            match Backend::collect_references(&path, &ast, &mut state, None) {
                Err(err) => eprintln!("Error updating opened file after save of another: {}", err),
                _ => (),
            }
        }

        if let Some(diagnostics) = state.diagnostics.get(&path) {
            self.client
                .publish_diagnostics(uri, diagnostics.clone(), None)
                .await;
        }
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
        trigger_characters.push(String::from(":"));
        trigger_characters.push(String::from(">"));

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Full,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(trigger_characters),
                    work_done_progress_options: Default::default(),
                }),
                /*signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: None,
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),*/
                references_provider: Some(true),
                rename_provider: Some(RenameProviderCapability::Simple(true)),
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
        let mut state = self.state.lock().await;

        for (file, diagnostics) in state.diagnostics.iter() {
            if
            /*file.contains("/vendor/")
            || file.contains("/phpstorm-stubs/")
            || */
            diagnostics.is_empty() {
                continue;
            }

            self.client
                .publish_diagnostics(
                    Url::from_file_path(file).unwrap(),
                    diagnostics.clone(),
                    None,
                )
                .await;
        }

        state.diagnostics.clear();
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        if params.query.is_empty() {
            return Ok(None);
        }

        let state = self.state.lock().await;

        let query = params.query.to_lowercase();

        let mut symbols = Vec::new();

        for (file_name, node) in state.files.iter() {
            for symbol in node.descendants(&state.arena) {
                let symbol = state.arena[symbol].get();

                if symbol.normalized_name().starts_with(&query) {
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
        let file_path = EnvFs::normalize_path(&params.text_document.uri.to_file_path().unwrap());

        let state = self.state.lock().await;

        let node_id = if let Some(node_id) = state.files.get(&file_path) {
            node_id.clone()
        } else {
            return Ok(None);
        };

        Ok(Some(DocumentSymbolResponse::Nested(
            node_id
                .children(&state.arena)
                .filter_map(|s| state.arena[s].get().to_doc_sym(&state.arena, &s))
                .collect(),
        )))
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let file = EnvFs::normalize_path(
            &params
                .text_document_position_params
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
        );

        let state = self.state.lock().await;

        if let Some(node_id) = state.files.get(&file) {
            return Ok(environment::document_highlights(
                &params.text_document_position_params.position,
                &state.arena,
                node_id,
            ));
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let position = &params.text_document_position.position;
        let file = EnvFs::normalize_path(
            &params
                .text_document_position
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
        );
        let (suc, nuc) = if let Some((suc, nuc)) = self.symbol_under_cursor(position, &file).await {
            (suc, nuc)
        } else {
            return Ok(None);
        };

        eprintln!("Finding refs for {:?} {}", suc, nuc);

        let symbol_references =
            if let Some(symbol_references) = self.references_of_symbol_under_cursor(&nuc).await {
                symbol_references
            } else {
                return Ok(None);
            };

        let locations = symbol_references
            .lock()
            .await
            .iter()
            .filter_map(|(file, refs)| {
                // Find all refs that point to our symbol, across all files
                if let Some(ranges) = refs.get(&suc) {
                    Some(
                        ranges
                            .iter()
                            .map(|range| Location {
                                uri: Url::from_file_path(file).unwrap(),
                                range: get_range(*range),
                            })
                            .collect::<Vec<Location>>(),
                    )
                } else {
                    None
                }
            })
            .fold(Vec::new(), |cur, mut tot: Vec<Location>| {
                tot.extend(cur);

                tot
            });

        return Ok(Some(locations));
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let position = &params.text_document_position.position;
        let file = EnvFs::normalize_path(
            &params
                .text_document_position
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
        );
        let (suc, nuc) = if let Some((suc, nuc)) = self.symbol_under_cursor(position, &file).await {
            (suc, nuc)
        } else {
            return Ok(None);
        };

        let symbol_references =
            if let Some(symbol_references) = self.references_of_symbol_under_cursor(&nuc).await {
                symbol_references
            } else {
                return Ok(None);
            };

        let symbol_range = self.state.lock().await.arena[suc].get().selection_range;
        let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        changes.insert(
            Url::from_file_path(file).unwrap(),
            vec![TextEdit {
                range: symbol_range,
                new_text: params.new_name.clone(),
            }],
        );
        symbol_references
            .lock()
            .await
            .iter()
            .for_each(|(file, refs)| {
                let edits: Vec<TextEdit> = refs
                    .iter()
                    .filter_map(|(node, ranges)| {
                        if node == &suc {
                            return Some(ranges.iter().map(|range| TextEdit {
                                new_text: params.new_name.clone(),
                                range: get_range(*range),
                            }));
                        }

                        return None;
                    })
                    .flatten()
                    .collect();
                // Find all refs that point to our symbol, across all files
                changes
                    .entry(Url::from_file_path(file).unwrap())
                    .or_insert_with(Vec::new)
                    .extend(edits);
            });

        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let file = EnvFs::normalize_path(&uri.to_file_path().unwrap());
        let state = self.state.lock().await;

        let position = &params.text_document_position_params.position;

        if let Some(references) = state.symbol_references.get(&file) {
            for (node, ranges) in references {
                if ranges
                    .iter()
                    .find(|r| in_range(position, &get_range(**r)))
                    .is_some()
                {
                    if let Some(location) = environment::symbol_location(&state.arena, node) {
                        return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                    }
                }
            }
        }

        Ok(None)
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let mut state = self.state.lock().await;

        for change in params.changes {
            let file_path = change.uri.to_file_path().unwrap();
            if self
                .ignore_patterns
                .iter()
                .any(|ip| file_path.ends_with(ip))
            {
                continue;
            }

            let path = EnvFs::normalize_path(&file_path);

            // If the file is currently opened we don't have to refresh
            if state.opened_files.contains_key(&path) {
                return;
            }

            let content = tokio::fs::read_to_string(file_path).await.unwrap();

            if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
                let reindex_result = Backend::collect_symbols(&path, &ast, &range, &mut state);

                let diagnostics = state
                    .diagnostics
                    .entry(path.to_string())
                    .or_insert_with(Vec::new);
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
        eprintln!("[did_change] start");
        let uri = params.text_document.uri;
        let file_path = uri.to_file_path().unwrap();
        let path = EnvFs::normalize_path(&file_path);

        if let Some(changes) = params.content_changes.first() {
            self.state
                .lock()
                .await
                .latest_version_of_file
                .insert(path, changes.text.clone());

            self.refresh_file(uri, &changes.text).await;
        }
        eprintln!("[did_change] end");
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let p = EnvFs::normalize_path(&params.text_document.uri.to_file_path().unwrap());
        let mut state = self.state.lock().await;
        state.latest_version_of_file.remove(&p);
        state.opened_files.remove(&p);
        state.symbol_references.remove(&p);
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let file_path = params.text_document.uri.to_file_path().unwrap();
        let path = EnvFs::normalize_path(&file_path);
        let mut state = self.state.lock().await;

        if !state.opened_files.contains_key(&path) {
            eprintln!("[did_open] indexing file");
            self.client
                .log_message(MessageType::Info, "Need to freshly index")
                .await;

            let source = tokio::fs::read_to_string(file_path).await.unwrap();
            state
                .latest_version_of_file
                .insert(path.clone(), source.clone());

            if let Ok((ast, range, errors)) = Backend::source_to_ast(&source) {
                let diags = errors.iter().map(Diagnostic::from).collect();
                state.diagnostics.insert(path.to_owned(), diags);
                state
                    .opened_files
                    .insert(path.to_string(), (ast.to_owned(), range));
            } else {
                self.client
                    .log_message(MessageType::Error, "Error indexing")
                    .await;

                return;
            }
        }

        eprintln!("[did_open] file stored in opened files");

        let (ast, range) = if let Some((ast, range)) = state.opened_files.get(&path) {
            self.client
                .log_message(MessageType::Info, "Opened from cache")
                .await;
            (ast.clone(), range)
        } else {
            self.client
                .log_message(MessageType::Error, "Error storing reindexed")
                .await;

            return;
        };

        eprintln!("[did_open] start resolving symbols");
        if let Err(e) = Backend::collect_references(&path, &ast, &mut state, None) {
            self.client
                .log_message(MessageType::Error, format!("Failed to index {}", e))
                .await;

            return;
        }
        eprintln!("[did_open] done resolving symbols");

        if let Some(diagnostics) = state.diagnostics.get(&path) {
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
        let file = EnvFs::normalize_path(
            &params
                .text_document_position_params
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
        );
        let position = &params.text_document_position_params.position;

        let symbol = self.symbol_under_cursor(position, &file).await;
        let state = self.state.lock().await;
        if let Some((node, _)) = symbol {
            let symbol = state.arena[node].get();

            if in_range(position, &symbol.selection_range) {
                return Ok(Some(Hover {
                    range: Some(symbol.range),
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: symbol.hover_text(node, &state.arena),
                    }),
                }));
            }
        }

        if let Some(references) = state.symbol_references.get(&file) {
            for (node, ranges) in references {
                if let Some(range) = ranges.iter().find(|r| in_range(position, &get_range(**r))) {
                    let symbol = state.arena[*node].get();

                    return Ok(Some(Hover {
                        range: Some(get_range(*range)),
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: symbol.hover_text(*node, &state.arena),
                        }),
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
        let state = self.state.lock().await;

        let pos = params.text_document_position.position;

        let opened_file = EnvFs::normalize_path(
            &params
                .text_document_position
                .text_document
                .uri
                .to_file_path()
                .unwrap(),
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

        let file_ast = state.opened_files.get(&opened_file);
        if let Some((file_ast, _range)) = file_ast {
            let ast = file_ast;

            let current_file_symbol =
                if let Some(current_file_symbol) = state.files.get(&opened_file) {
                    current_file_symbol
                } else {
                    return Ok(None);
                };
            let current_file = state.arena[*current_file_symbol].get();

            let symbol_under_cursor =
                current_file.symbol_at(&pos, *current_file_symbol, &state.arena);

            if let Some(references) = state.symbol_references.get(&opened_file) {
                let mut suggestions = suggester::get_suggestions_at(
                    trigger,
                    pos,
                    symbol_under_cursor,
                    ast,
                    &state.arena,
                    &state.global_symbols,
                    references,
                );

                let mut completions: Vec<CompletionItem> =
                    suggestions.1.iter().map(|t| t.into()).collect();

                completions.extend(
                    suggestions
                        .0
                        .drain(..)
                        .map(|s| {
                            let symbol = state.arena[s].get();

                            // If the symbol is a class we try to add a namespace as a text edit
                            if symbol.kind == PhpSymbolKind::Class {
                                let ns = if let Some(ns) = symbol.namespace.as_ref() {
                                    ns
                                } else {
                                    return symbol.completion_item(s, &state.arena);
                                };

                                let fqdn = format!("{}\\{}", ns, symbol.name);
                                let to_import = format!("use {};\n", fqdn);
                                // Check if the current file already has that import. if yes we are good
                                let line = if let Some(imports) = current_file.imports.as_ref() {
                                    if imports
                                        .iter()
                                        .find(|import| import.full_name() == fqdn)
                                        .is_some()
                                    {
                                        return symbol.completion_item(s, &state.arena);
                                    } else {
                                        // add use to the end of the imports
                                        if let Some(first_import) = imports.first() {
                                            first_import.path.first().unwrap().line
                                        } else {
                                            3
                                        }
                                    }
                                } else {
                                    // add use right after the namespace or the opening <?php
                                    3
                                };

                                // if not, we add it as a text edit

                                return CompletionItem {
                                    additional_text_edits: Some(vec![TextEdit {
                                        range: get_range(((line, 0), (line, 0))),
                                        new_text: to_import,
                                    }]),
                                    ..symbol.completion_item(s, &state.arena)
                                };
                            }

                            symbol.completion_item(s, &state.arena)
                        })
                        .collect::<Vec<CompletionItem>>(),
                );

                return Ok(Some(CompletionResponse::Array(completions)));
            }
        }

        Ok(None)
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
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
    async fn test_knows_stubs() {
        let base_dir = std::env::current_dir().unwrap();
        let root = format!("{}/fixtures/projects/stubs", base_dir.display());
        let file = format!("{}/fixtures/projects/stubs/index.php", base_dir.display());

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
