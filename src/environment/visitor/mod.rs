use super::Symbol;
use crate::parser::node::Node as AstNode;
use indextree::{Arena, NodeId};

macro_rules! ref_from_doc {
    ($source:ident, $dest:ident, $part:ident ) => {
        if let Some(doc_comment) = $source {
            if let AstNode::DocComment { $part, .. } = doc_comment.as_ref() {
                for rt in $part {
                    $dest.extend(
                        get_type_refs(rt)
                            .iter()
                            .map(|tr| Reference::type_ref(tr.clone()))
                            .collect::<Vec<Reference>>(),
                    );
                }
            }
        }
    };
}

pub mod name_resolver;
pub mod workspace_symbol;

pub enum NextAction {
    /// Do not continue processing the children
    Abort,

    /// Do process the children and consider NodeId the next parent
    ProcessChildren(NodeId),
}

pub trait Visitor {
    fn before(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId);
    fn visit(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId) -> NextAction;
    fn after(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId);
}
