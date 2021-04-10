use super::get_range;
use crate::parser::node::TypeRef;
use indextree::NodeId;
use tower_lsp::lsp_types::Range;

#[derive(Clone, Debug)]
pub struct Reference {
    /// The type_ref if applicable
    pub type_ref: TypeRef,

    /// Selection range of the usage
    pub range: Range,

    pub node: Option<NodeId>,
}

impl std::fmt::Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.type_ref.to_fqdn())?;

        Ok(())
    }
}

impl Reference {
    /// Reference to an identifier, for example a function or a member
    pub fn node(type_ref: TypeRef, node: NodeId) -> Self {
        let range = get_range(type_ref.range());

        Self {
            type_ref,
            range,
            node: Some(node),
        }
    }

    /// Reference to a type
    pub fn type_ref(type_ref: TypeRef) -> Self {
        let range = get_range(type_ref.range());

        Self {
            type_ref,
            range,
            node: None,
        }
    }
}
