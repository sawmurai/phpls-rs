use super::Symbol;
use crate::parser::node::Node as AstNode;
use indextree::{Arena, NodeId};

pub mod name_resolver;
pub mod workspace_symbol;

pub enum NextAction {
    Abort,
    ProcessChildren(NodeId),
}

pub trait Visitor {
    fn before(&mut self, node: &AstNode);
    fn visit(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId) -> NextAction;
    fn after(&mut self, node: &AstNode);
}
