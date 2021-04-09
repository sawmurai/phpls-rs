use super::BackendState;
use crate::environment::{fs as EnvFs, get_range, in_range};
use lsp_types::{DocumentHighlight, DocumentHighlightParams, Position};
use tower_lsp::jsonrpc::Result;

/// Gateway function that accepts the LSP parameters and calls the exec method
pub(crate) fn document_highlight(
    state: &BackendState,
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
    let position = &params.text_document_position_params.position;

    exec(state, position, &file)
}

#[inline]
fn exec(
    state: &BackendState,
    position: &Position,
    file: &str,
) -> Result<Option<Vec<DocumentHighlight>>> {
    // Check all references in the current file
    if let Some(references) = state.symbol_references.get(file) {
        for (node, ranges) in references {
            // Does this symbol has a reference at the location we are looking?
            if ranges
                .iter()
                .find(|r| in_range(position, &get_range(**r)))
                .is_some()
            {
                // If it does, return with all of its references
                if let Some(ranges) = references.get(&node) {
                    return Ok(Some(
                        ranges
                            .iter()
                            .map(|pos| DocumentHighlight {
                                kind: None,
                                range: get_range(*pos),
                            })
                            .collect(),
                    ));
                }
            }
        }
    }

    Ok(None)
}

#[cfg(test)]
mod tests {
    use crate::backend::tests::populate_state;

    use super::*;
    use lsp_types::{DocumentHighlight, DocumentHighlightParams, Position, Range};

    #[test]
    fn returns_none_if_no_symbol_at_position() {
        let sources = [("index.php", "<?php $var = 2; echo $var;")];

        let mut state = BackendState::default();
        populate_state(&mut state, &sources);

        let pos = Position {
            line: 0,
            character: 18,
        };

        assert_eq!(Ok(None), exec(&state, &pos, "index.php"));
    }

    #[test]
    fn returns_all_references_to_symbol_at_position() {
        let sources = [("index.php", "<?php $var = 2; echo $var;")];

        let mut state = BackendState::default();
        populate_state(&mut state, &sources);

        let pos = Position {
            line: 0,
            character: 24,
        };

        assert_eq!(
            Ok(Some(vec![
                DocumentHighlight {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 6
                        },
                        end: Position {
                            line: 0,
                            character: 10
                        }
                    },
                    kind: None
                },
                DocumentHighlight {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 21
                        },
                        end: Position {
                            line: 0,
                            character: 25
                        }
                    },
                    kind: None
                }
            ])),
            exec(&state, &pos, "index.php")
        );
    }

    #[test]
    fn returns_all_references_to_symbol_references_at_position() {
        let sources = [("index.php", "<?php $var = 2; echo $var;")];

        let mut state = BackendState::default();
        populate_state(&mut state, &sources);

        let pos = Position {
            line: 0,
            character: 8,
        };

        assert_eq!(
            Ok(Some(vec![
                DocumentHighlight {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 6
                        },
                        end: Position {
                            line: 0,
                            character: 10
                        }
                    },
                    kind: None
                },
                DocumentHighlight {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 21
                        },
                        end: Position {
                            line: 0,
                            character: 25
                        }
                    },
                    kind: None
                }
            ])),
            exec(&state, &pos, "index.php")
        );
    }
}
