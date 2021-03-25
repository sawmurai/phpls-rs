use super::visitor::NextAction;
use super::visitor::Visitor;
use super::Symbol;
use crate::parser::node::Node as AstNode;
use indextree::{Arena, NodeId};

pub fn traverse<T>(node: &AstNode, visitor: &mut T, arena: &mut Arena<Symbol>, parent: NodeId)
where
    T: Visitor,
{
    visitor.before(node, arena, parent);

    match visitor.visit(node, arena, parent) {
        NextAction::Abort => (),
        NextAction::ProcessChildren(next_parent) => {
            // If node is a namespace somehow the classes and interface etc. are not added to it but still to the file
            // Probably because they are not children in the ast node of namespace
            for child in node.children() {
                //eprintln!("[environment::traverser::traverse] calling traverse()");
                traverse(child, visitor, arena, next_parent);
                //eprintln!("[environment::traverser::traverse] calling traverse() ended");
            }
        }
    }

    visitor.after(node, arena, parent);
}
