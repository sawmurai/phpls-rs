use super::get_range;
use crate::token::Token;
use indextree::NodeId;
use tower_lsp::lsp_types::Range;

#[derive(Clone, Debug)]
pub struct Reference {
    /// Used for variables
    pub token: Option<Token>,

    /// The type_ref if applicable
    pub type_ref: Option<Vec<Token>>,

    /// Selection range of the usage
    pub range: Range,

    pub node: Option<NodeId>,
}

impl std::fmt::Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(token) = self.token.as_ref() {
            f.write_str(&token.to_string())?;
        }

        if let Some(type_ref) = self.type_ref.as_ref() {
            let mut name = Vec::with_capacity(type_ref.len());

            for token in type_ref.iter() {
                name.push(token.to_string());
            }

            f.write_str(&name.join("\\"))?;
        }

        Ok(())
    }
}

impl Reference {
    /// Reference to an identifier, for example a function or a member
    pub fn node(token: &Token, node: NodeId) -> Self {
        Self {
            token: Some(token.clone()),
            type_ref: None,
            range: get_range(token.range()),
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
            token: None,
            type_ref: Some(type_ref),
            range,
            node: None,
        }
    }
}
