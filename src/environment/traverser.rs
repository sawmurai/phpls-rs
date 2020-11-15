use crate::node::Node as AstNode;
use super::visitor::{Visitor, ScopedVisitor};
use super::Symbol;
use super::visitor::NextAction;
use indextree::{Arena, NodeId};

pub fn traverse<T>(node: &AstNode, visitor: &mut T, arena: &mut Arena<Symbol>, parent: NodeId) where T: Visitor {
    visitor.before(node);

    match visitor.visit(node, arena, parent) {
        NextAction::Abort => return,
        NextAction::ProcessChildren => {
            // If the visitor added a symbol we consider it the new parent
            let parent = if let Some(freshly_added) = arena[parent].last_child() {
                freshly_added
            } else {
                parent
            };

            for child in node.children() {
                traverse(child, visitor, arena, parent);
            }
        }
    }

    visitor.after(node);
}

pub fn traverse_scoped<T>(node: &AstNode, visitor: &mut T, arena: &mut Arena<Symbol>) where T: ScopedVisitor {
    visitor.before(node);

    match visitor.visit(node, arena) {
        NextAction::Abort => return,
        NextAction::ProcessChildren => {
            for child in node.children() {
                traverse_scoped(child, visitor, arena);
            }
        }
    }

    visitor.after(node);
}
