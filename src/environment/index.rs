use crate::environment::Environment;
use crate::expression::{Node, NodeRange};
use crate::token::Token;
use std::collections::HashMap;

use tower_lsp::lsp_types::{Position, Range};

#[derive(Debug, Default)]
pub struct Index {
    /// HashMap where the key is the name of the symbol and the value is a
    /// vector of all the tokens of its usage within the scope. Symbols, which are defined
    /// in a parent scope will be inserted into to the parent scope's symbol HashMap, not in both
    pub(crate) symbols: HashMap<String, Vec<Token>>,
}

pub(crate) fn walk_tree<'a>(env: &'a mut Environment, ast: Vec<Node>) {
    for node in ast {
        walk_node(env, node);
    }
}

/// Walk the generated syntax tree and resolve symbols and definitions, enriching the passed
/// environment on the way.
fn walk_node<'a>(env: &'a mut Environment, node: Node) {
    // See if this fucks up performance
    let range = node.range();

    match node {
        Node::AliasedVariable { expr, .. } => walk_node(env, *expr),
        Node::AlternativeBlock { statements, .. } => walk_tree(env, statements),
        Node::ArrowFunction {
            arguments, body, ..
        } => {
            if let Some(arguments) = arguments {
                walk_tree(env, arguments);
            }

            walk_node(env, *body);
        }
        Node::Array { elements, .. } => walk_tree(env, elements),
        Node::ArrayElement { key, value, .. } => {
            if let Some(key) = key {
                walk_node(env, *key);
            }

            walk_node(env, *value);
        }
        Node::Block { statements, .. } => {
            walk_tree(env, statements);
        }
        Node::Binary { left, right, .. } => {
            walk_node(env, *left);
            walk_node(env, *right);
        }
        Node::Call {
            callee, parameters, ..
        } => {
            walk_node(env, *callee);
            walk_tree(env, parameters);
        }
        Node::Class { body, .. } => {
            walk_node(env, *body);
        }
        Node::ClassStatement { name, body, .. } => {
            let class_name = name.clone().label.unwrap().clone();

            if let Node::Block { statements, .. } = *body {
                env.start_class(&class_name, get_range(range));
                walk_tree(env, statements);
                env.finish_class();
            }
        }
        Node::ClassConstantDefinitionStatement { name, .. } => {
            env.register_constant(&name.label.unwrap(), get_range(range))
        }
        Node::DoWhileStatement {
            condition, body, ..
        } => {
            walk_node(env, *condition);
            walk_node(env, *body);
        }
        Node::ExpressionStatement { expression } => walk_node(env, *expression),
        Node::ForStatement {
            init,
            condition,
            step,
            body,
            ..
        } => {
            walk_tree(env, init);
            walk_tree(env, condition);
            walk_tree(env, step);
            walk_node(env, *body);
        }
        Node::ForEachStatement {
            collection,
            kv,
            body,
            ..
        } => {
            walk_node(env, *collection);
            walk_node(env, *kv);
            walk_node(env, *body);
        }
        Node::Function {
            arguments, uses, ..
        } => {
            if let Some(arguments) = arguments {
                walk_tree(env, arguments);
            }

            if let Some(uses) = uses {
                walk_tree(env, uses);
            }
        }
        Node::FunctionArgument { name, .. } => {
            env.register_variable(&name.label.unwrap(), get_range(range));
        }
        Node::Member { object, member, .. } => {
            walk_node(env, *object);
            walk_node(env, *member);
        }
        Node::MethodDefinitionStatement { name, function, .. } => {
            let method_name = name.label.unwrap();
            env.register_method(&method_name, get_range(range));

            match *function {
                Node::FunctionDefinitionStatement {
                    arguments, body, ..
                } => {
                    if let Some(arguments) = arguments {
                        walk_tree(env, arguments);
                    }

                    if let Some(body) = body {
                        walk_node(env, *body);
                    }
                }
                _ => {}
            };
        }
        //Node::PropertyDefinitionStatement { name, .. } => println!("${}", name.label.unwrap()),
        Node::NamedFunctionDefinitionStatement { function, .. } => {
            match *function {
                Node::FunctionDefinitionStatement {
                    arguments, body, ..
                } => {
                    if let Some(arguments) = arguments {
                        walk_tree(env, arguments);
                    }
                    if let Some(body) = body {
                        walk_node(env, *body);
                    }
                }
                _ => {}
            };
        }
        Node::NamespaceBlock {
            block,
            type_ref,
            token,
        } => {
            if let Some(Node::TypeRef(type_ref)) = *type_ref {
                env.start_namespace(type_ref, get_range(range));
            } else {
                env.start_namespace(vec![token], get_range(range));
            }

            walk_node(env, *block);

            env.finish_namespace();
        }
        Node::FunctionDefinitionStatement {
            arguments, body, ..
        } => {
            if let Some(arguments) = arguments {
                walk_tree(env, arguments);
            }

            if let Some(body) = body {
                walk_node(env, *body);
            }
        }
        Node::GlobalVariablesStatement { vars, .. } => walk_tree(env, vars),
        Node::IfBranch {
            condition, body, ..
        } => {
            walk_node(env, *condition);
            walk_node(env, *body);
        }
        Node::IfStatement {
            if_branch,
            elseif_branches,
            else_branch,
        } => {
            walk_node(env, *if_branch);
            walk_tree(env, elseif_branches);

            if let Some(else_branch) = else_branch {
                walk_node(env, *else_branch);
            }
        }
        Node::New { class, .. } => walk_node(env, *class),
        Node::OldArray { elements, .. } => walk_tree(env, elements),
        Node::PropertyDefinitionStatement { name, .. } => {
            env.register_property(&name.label.unwrap(), get_range(range));
        }
        Node::SingleStatementBlock(node) => walk_node(env, *node),
        Node::SwitchBody { branches, .. } => walk_tree(env, branches),
        Node::SwitchBranch { cases, body } => {
            for case in cases {
                if let Some(case) = case {
                    walk_node(env, case);
                }
            }

            walk_tree(env, body)
        }
        Node::SwitchCase { expr, body, .. } => {
            walk_node(env, *expr);
            walk_node(env, *body);
        }
        Node::StaticVariablesStatement { assignments, .. } => walk_tree(env, assignments),
        Node::StaticVariable { value, .. } => {
            if let Some(value) = value {
                walk_node(env, *value);
            }
        }

        Node::Ternary {
            check,
            true_arm,
            false_arm,
            ..
        } => {
            walk_node(env, *check);
            if let Some(true_arm) = true_arm {
                walk_node(env, *true_arm);
            }
            walk_node(env, *false_arm);
        }
        Node::TryCatch {
            try_block,
            catch_blocks,
            finally_block,
            ..
        } => {
            walk_node(env, *try_block);

            walk_tree(env, catch_blocks);

            if let Some(finally_block) = finally_block {
                walk_node(env, *finally_block);
            }
        }
        Node::Unary { expr, .. } => {
            walk_node(env, *expr);
        }
        Node::Variable(variable) => {
            env.register_variable(&variable.label.unwrap(), get_range(range))
        }
        Node::WhileStatement {
            condition, body, ..
        } => {
            walk_node(env, *condition);
            walk_node(env, *body);
        }
        _ => {
            //println!("{:#?}", n);
        }
    };
}

fn get_range(coords: NodeRange) -> Range {
    let start = coords.0;
    let end = coords.1;

    Range {
        start: Position {
            line: start.0 as u64,
            character: start.1 as u64,
        },
        end: Position {
            line: end.0 as u64,
            character: end.1 as u64,
        },
    }
}
