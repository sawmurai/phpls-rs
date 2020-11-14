use crate::node::Node as AstNode;
use super::Symbol;
use indextree::{Arena, NodeId};

pub mod workspace_symbol;
pub mod name_resolver;

pub enum NextAction {
    Abort,
    ProcessChildren
}

pub trait Visitor {
    fn before(&mut self, node: &AstNode);
    fn visit(&mut self, node: &AstNode, arena: &mut Arena<Symbol>, parent: NodeId) -> NextAction;
    fn after(&mut self, node: &AstNode);
}