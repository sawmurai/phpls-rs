use crate::node::get_range;
use crate::token::{Token, TokenType};
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

impl Reference {
    pub fn is_builtin(&self) -> bool {
        if let Some(type_ref) = self.type_ref.as_ref() {
            if type_ref.len() == 1 {
                match type_ref[0].t {
                    TokenType::TypeString
                    | TokenType::TypeArray
                    | TokenType::TypeBool
                    | TokenType::TypeSelf
                    | TokenType::TypeInt => return true,
                    _ => return false,
                }
            }
        }

        return false;
    }

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
