use crate::environment::{fs as EnvFs, get_range, in_range};
use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind};
use tower_lsp::jsonrpc::Result;

use super::{Backend, BackendState};

pub(crate) fn hover(state: &BackendState, params: HoverParams) -> Result<Option<Hover>> {
    let file = EnvFs::normalize_path(
        &params
            .text_document_position_params
            .text_document
            .uri
            .to_file_path()
            .unwrap(),
    );
    let position = &params.text_document_position_params.position;

    let symbol = Backend::symbol_under_cursor(&state, position, &file);
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
