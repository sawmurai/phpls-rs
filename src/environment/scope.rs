use super::get_range;
use crate::parser::token::{to_fqdn, Token};
use indextree::NodeId;
use tower_lsp::lsp_types::Range;

#[derive(Clone, Debug)]
pub struct Reference {
    /// The type_ref if applicable
    pub type_ref: Option<Vec<Token>>,

    /// Selection range of the usage
    pub range: Range,

    pub node: Option<NodeId>,
}

impl std::fmt::Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(type_ref) = self.type_ref.as_ref() {
            f.write_str(&to_fqdn(type_ref))?;
        }

        Ok(())
    }
}

impl Reference {
    /// Reference to an identifier, for example a function or a member
    pub fn node(token: &[Token], node: NodeId) -> Self {
        let range = get_range((
            token.first().unwrap().range().0,
            token.last().unwrap().range().1,
        ));

        Self {
            type_ref: None,
            range: range,
            node: Some(node),
        }
    }

    /// Reference to a type
    pub fn type_ref(type_ref: Vec<Token>) -> Self {
        let range = get_range((
            type_ref.first().unwrap().range().0,
            type_ref.last().unwrap().range().1,
        ));

        Self {
            type_ref: Some(type_ref),
            range,
            node: None,
        }
    }
}
