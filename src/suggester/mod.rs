use tower_lsp::lsp_types::Position;

use crate::{environment::get_range, environment::in_range, parser::node::Node};

pub fn find<'a>(
    n: &'a Node,
    position: &Position,
    mut ancestors: Vec<&'a Node>,
) -> Option<(&'a Node, Vec<&'a Node>)> {
    ancestors.push(n);
    eprintln!(">> {}", n.name());

    for c in n.children() {
        if in_range(position, &get_range(c.range())) {
            return find(c, position, ancestors);
        }
    }

    if in_range(position, &get_range(n.range())) {
        return Some((n, ancestors));
    }

    None
}

pub fn suggest_variables(ancestors: Vec<&Node>) -> Vec<String> {
    let mut suggestions = Vec::new();

    let in_class = ancestors
        .iter()
        .find(|n| match n {
            Node::ClassStatement { .. } => true,
            _ => false,
        })
        .is_some();
    if in_class {
        suggestions.push(String::from("$this"));
    }

    let mut root = ancestors.first().unwrap();

    for ancestor in ancestors.iter().rev() {
        if ancestor.scope_boundary() {
            root = ancestor;

            break;
        }
    }

    for descedant in root.descendants() {
        match descedant {
            Node::Variable(name) => suggestions.push(name.to_string()),
            _ => (),
        }
    }

    suggestions.sort();
    suggestions.dedup();

    suggestions
}

#[cfg(test)]
mod tests {
    use super::super::backend::NodeMapMutex;
    use super::super::parser::scanner::Scanner;
    use super::super::parser::Parser;

    #[test]
    fn test_suggest_global_symbol_for_start_of_new_token() {
        assert!(false)
    }
}
