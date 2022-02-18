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
use ignore::{types::TypesBuilder, WalkBuilder};
use indextree::{Arena, NodeId};
use lsp_types::request::GotoImplementationParams;
use lsp_types::request::GotoImplementationResponse;
use lsp_types::DefinitionOptions;
use lsp_types::DocumentHighlightOptions;
use lsp_types::DocumentSymbolOptions;
use lsp_types::ImplementationProviderCapability;
use lsp_types::OneOf;
use lsp_types::ReferencesOptions;
use lsp_types::RenameOptions;
use lsp_types::WorkspaceFoldersServerCapabilities;
use lsp_types::WorkspaceServerCapabilities;
use lsp_types::WorkspaceSymbolOptions;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io;
use tokio::runtime::Handle;
use tokio::sync::Mutex;
use tokio::task;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionOptions, CompletionParams, CompletionResponse, Diagnostic,
    DidChangeWatchedFilesParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentFormattingParams, DocumentHighlight, DocumentHighlightParams, DocumentSymbolParams,
    DocumentSymbolResponse, ExecuteCommandOptions, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, HoverProviderCapability, InitializeParams, InitializeResult,
    InitializedParams, Location, Position, Range, ReferenceParams, RenameParams,
    ServerCapabilities, SymbolInformation, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextEdit, Url, WorkspaceEdit, WorkspaceSymbolParams,
};
use tower_lsp::{jsonrpc::Result, lsp_types::DidChangeTextDocumentParams};
use tower_lsp::{Client, LanguageServer};

extern crate crossbeam_channel as channel;
extern crate ignore;
extern crate walkdir;

mod completion;
mod did_change;
mod did_change_watched_files;
mod did_close;
mod did_open;
mod document_highlight;
mod document_symbol;
mod formatting;
mod goto_definition;
mod goto_implementation;
mod hover;
mod symbol;

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
                line: start.0,
                character: start.1,
            },
            end: Position {
                line: end.0,
                character: end.1,
            },
        };

        let kind = match token.t {
            TokenType::Variable => PhpSymbolKind::Variable,
            _ => PhpSymbolKind::Unknown,
        };

        Symbol {
            name: token.to_string(),
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
    fn symbol_under_cursor(
        state: &BackendState,
        position: &Position,
        file: &str,
    ) -> Option<(NodeId, String)> {
        let file = if let Some(node) = state.files.get(file) {
            *node
        } else {
            return None;
        };

        let suc = state.arena[file]
            .get()
            .symbol_at(&position, file, &state.arena);

        Some((suc, state.arena[suc].get().name().to_owned()))
    }

    fn referenced_symbol_under_cursor<'a>(
        position: &Position,
        references: &'a HashMap<NodeId, Vec<NodeRange>>,
    ) -> Option<&'a NodeId> {
        for (node, ranges) in references {
            if ranges.iter().any(|r| in_range(position, &get_range(*r))) {
                return Some(node);
            }
        }

        None
    }

    async fn references_of_symbol_under_cursor(&self, nuc: &str) -> Option<ReferenceMapMutex> {
        let all_files = self
            .state
            .lock()
            .await
            .files
            .iter()
            .map(|(k, _)| k.clone())
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
            handle.spawn(async move {
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
            })
        });

        let mut type_builder = TypesBuilder::new();
        type_builder.add_def("php:*.php").unwrap();
        let types = type_builder.select("php").build().unwrap();

        let walker = WalkBuilder::new(PathBuf::from(&root_path))
            .standard_filters(false)
            .add(PathBuf::from(&self.stubs))
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
                            Err(_) => {
                                //eprintln!("Error reading file {}", error);

                                return Continue;
                            }
                        };

                        if let Ok((ast, range, errors)) = Backend::source_to_ast(&content) {
                            if let Err(e) =
                                tx.send((EnvFs::normalize_path(&path), ast, range, errors))
                            {
                                eprintln!("{:?}", e);
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
                for symbol_id in node_id.children(&state.arena) {
                    let symbol = state.arena[symbol_id].get();

                    if symbol.kind.register_global() {
                        global_table.insert(symbol.fqdn().to_lowercase(), symbol_id);
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

        // Deregister old children from the global symbol table and the references
        if let Some(old_enclosing) = state.files.insert(path.to_owned(), enclosing_file) {
            // Since only the top level symbols are in the global_symbols its okay
            // to shallowy travers the tree
            for symbol_id in old_enclosing.children(&state.arena) {
                let symbol = state.arena[symbol_id].get();

                state.global_symbols.remove(&symbol.fqdn().to_lowercase());
            }

            // But references are a different story
            for symbol_id in old_enclosing.descendants(&state.arena) {
                for (_file, references_of_file) in state.symbol_references.iter_mut() {
                    references_of_file.remove(&symbol_id);
                }
            }

            old_enclosing.remove_subtree(&mut state.arena);
        }

        for symbol_id in enclosing_file.children(&state.arena) {
            let symbol = state.arena[symbol_id].get();

            if symbol.kind.register_global() {
                state
                    .global_symbols
                    .insert(symbol.fqdn().to_lowercase(), symbol_id);
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
            *file
        } else {
            eprintln!("Dafuq! {}", path);

            return Ok(());
        };

        // Purge all existing variables as they will be created during resolution. This avoids ending up with duplicate
        // variables
        let variables = enclosing_file
            .descendants(&state.arena)
            .filter(|descendant| state.arena[*descendant].get().kind == PhpSymbolKind::Variable)
            .collect::<Vec<NodeId>>();

        variables.iter().for_each(|variable| {
            variable.remove(&mut state.arena);
        });

        let mut resolver = NameResolver::new(&state.global_symbols, enclosing_file);

        let mut visitor = NameResolveVisitor::new(&mut resolver, enclosing_file);
        let iter = ast.iter();
        for node in iter {
            traverse(node, &mut visitor, &mut state.arena, enclosing_file);
        }

        for notification in visitor.diagnostics() {
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

    pub(crate) fn refresh_file(state: &mut BackendState, uri: Url, src: &str) {
        let file_path = uri.to_file_path().unwrap();
        let path = EnvFs::normalize_path(&file_path);

        if let Ok((ast, range, errors)) = Backend::source_to_ast(&src) {
            let reindex_result = Backend::collect_symbols(&path, &ast, &range, state);

            let diagnostics = state
                .diagnostics
                .entry(path.to_string())
                .or_insert_with(Vec::new);
            diagnostics.clear();
            diagnostics.extend(errors.iter().map(Diagnostic::from));

            state.opened_files.insert(path, (ast, range));

            if reindex_result.is_err() {
                return;
            }
        }

        // Now reindex all the other currently opened files to update references
        let tasks = state.opened_files.clone();
        for (path, (ast, _)) in tasks {
            if let Err(e) = Backend::collect_references(&path, &ast, state, None) {
                eprintln!("Error updating opened file after save of another: {}", e);
            }
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
            .map(String::from)
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
                    all_commit_characters: None,
                    resolve_provider: Some(true),
                    trigger_characters: Some(trigger_characters),
                    work_done_progress_options: Default::default(),
                }),
                /*signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: None,
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),*/
                implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
                references_provider: Some(OneOf::Right(ReferencesOptions {
                    work_done_progress_options: Default::default(),
                })),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(false),
                    work_done_progress_options: Default::default(),
                })),
                definition_provider: Some(OneOf::Right(DefinitionOptions {
                    work_done_progress_options: Default::default(),
                })),
                document_highlight_provider: Some(OneOf::Right(DocumentHighlightOptions {
                    work_done_progress_options: Default::default(),
                })),
                document_symbol_provider: Some(OneOf::Right(DocumentSymbolOptions {
                    work_done_progress_options: Default::default(),
                    label: None,
                })),
                workspace_symbol_provider: Some(OneOf::Right(WorkspaceSymbolOptions {
                    work_done_progress_options: Default::default(),
                })),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                document_formatting_provider: Some(OneOf::Left(false)),
                workspace: Some(WorkspaceServerCapabilities {
                    file_operations: None,
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
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
        let state = self.state.lock().await;

        symbol::symbol(&state, params)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let state = self.state.lock().await;

        document_symbol::document_symbol(&state, params)
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let state = self.state.lock().await;

        document_highlight::document_highlight(&state, params)
    }

    async fn goto_implementation(
        &self,
        params: GotoImplementationParams,
    ) -> Result<Option<GotoImplementationResponse>> {
        let mut state = self.state.lock().await;

        goto_implementation::goto_implementation(&mut state, params)
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

        let (suc, nuc) = {
            let state = self.state.lock().await;
            if let Some((suc, nuc)) = Backend::symbol_under_cursor(&state, position, &file) {
                (suc, nuc)
            } else {
                return Ok(None);
            }
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
                refs.get(&suc).map(|ranges| {
                    ranges
                        .iter()
                        .map(|range| Location {
                            uri: Url::from_file_path(file).unwrap(),
                            range: get_range(*range),
                        })
                        .collect::<Vec<Location>>()
                })
            })
            .fold(Vec::new(), |cur, mut tot: Vec<Location>| {
                tot.extend(cur);

                tot
            });

        Ok(Some(locations))
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
        let nuc = {
            let state = self.state.lock().await;
            if let Some(references) = &state.symbol_references.get(&file) {
                if let Some(nuc) = Backend::referenced_symbol_under_cursor(position, &references) {
                    nuc.clone()
                } else {
                    return Ok(None);
                }
            } else {
                return Ok(None);
            }
        };

        let name = self.state.lock().await.arena[nuc].get().name.clone();

        let symbol_references =
            if let Some(symbol_references) = self.references_of_symbol_under_cursor(&name).await {
                symbol_references
            } else {
                return Ok(None);
            };

        let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

        symbol_references
            .lock()
            .await
            .iter()
            .for_each(|(file, refs)| {
                let edits: Vec<TextEdit> = refs
                    .iter()
                    .filter_map(|(node, ranges)| {
                        if node == &nuc {
                            return Some(ranges.iter().map(|range| TextEdit {
                                new_text: params.new_name.clone(),
                                range: get_range(*range),
                            }));
                        }

                        None
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
            change_annotations: None,
        }))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let state = self.state.lock().await;

        goto_definition::goto_definition(&state, params)
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let mut state = self.state.lock().await;

        did_change_watched_files::did_change_watched_files(&mut state, params);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut state = self.state.lock().await;
        let file_path = params.text_document.uri.to_file_path().unwrap();
        let path = EnvFs::normalize_path(&file_path);

        did_change::did_change(&mut state, &params);

        if let Some(diagnostics) = state.diagnostics.get(&path) {
            self.client
                .publish_diagnostics(params.text_document.uri, diagnostics.clone(), None)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut state = self.state.lock().await;
        did_close::did_close(&mut state, params);
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let mut state = self.state.lock().await;

        did_open::did_open(&mut state, &params);

        let file_path = params.text_document.uri.to_file_path().unwrap();
        let path = EnvFs::normalize_path(&file_path);
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
        let state = self.state.lock().await;
        hover::hover(&state, params)
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let state = self.state.lock().await;
        completion::completion(&state, params)
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let state = self.state.lock().await;
        formatting::formatting(&state, params)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    pub(crate) fn populate_state(state: &mut BackendState, sources: &[(&str, &str)]) {
        for (file_name, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let dr = scanner.document_range();
            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_symbols(*file_name, &pr.0, &get_range(dr), state).unwrap();
        }

        for (file_name, source) in sources.iter() {
            let mut scanner = Scanner::new(*source);
            scanner.scan().unwrap();

            let pr = Parser::ast(scanner.tokens).unwrap();

            Backend::collect_references(*file_name, &pr.0, state, None).unwrap();
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
