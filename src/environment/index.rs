use crate::environment::Environment;
use crate::expression::Node;
use crate::scanner::Scanner;
use crate::token::Token;
use std::collections::HashMap;

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
    match node {
        Node::AliasedVariable { expr, .. } => walk_node(env, *expr),
        Node::AlternativeBlock { statements, .. } => walk_tree(env, statements),
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
        Node::Class { token, body, .. } => {
            env.enter_scope(&format!("anonclass_{}_{}", token.line, token.col));
            walk_node(env, *body);
            env.finish_scope();
        }
        Node::ClassStatement { name, body, .. } => {
            if let Some(name) = name.label {
                env.enter_scope(&name);
            } else {
                env.enter_scope(&format!("anonclass_{}_{}", name.line, name.col));
            }
            walk_node(env, *body);

            env.finish_scope();
        }
        Node::ClassConstantDefinitionStatement { name, .. } => env.definition(&name),
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
            env.usage(name);
        }
        Node::Member { object, member, .. } => {
            walk_node(env, *object);
            walk_node(env, *member);
        }
        Node::MethodDefinitionStatement { name, function, .. } => {
            env.definition(&name);

            if let Some(name) = name.label {
                env.enter_scope(&name);
            } else {
                env.enter_scope("asd2");
            }

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

            env.finish_scope();
        }
        //Node::PropertyDefinitionStatement { name, .. } => println!("${}", name.label.unwrap()),
        Node::NamedFunctionDefinitionStatement { name, function, .. } => {
            if let Some(name) = name.label {
                env.enter_scope(&name);
            } else {
                env.enter_scope("asd");
            }

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

            env.finish_scope();
        }
        Node::FunctionDefinitionStatement {
            arguments,
            body,
            op,
            ..
        } => {
            env.enter_scope(&format!("anonfunc_{}_{}", op.line, op.col));
            if let Some(arguments) = arguments {
                walk_tree(env, arguments);
            }

            if let Some(body) = body {
                walk_node(env, *body);
            }
            env.finish_scope();
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
        Node::PropertyDefinitionStatement { name, .. } => env.definition(&name),
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
        Node::StaticVariable {
            variable, value, ..
        } => {
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
        Node::Variable(variable) => env.usage(variable),
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

/// Finds the token under the current cursor
pub(crate) fn symbol_under_cursor(content: &str, line: u16, col: u16) -> Option<Token> {
    let mut scanner = Scanner::new(&content);

    if let Ok(_) = scanner.scan() {
        for token in scanner.tokens {
            if token.is_on(line, col) {
                return Some(token);
            }
            if token.line >= line && token.col >= col {
                break;
            }
        }
    }

    None
}

pub(crate) fn auto_complete(ast: Vec<Node>) -> Vec<String> {
    // 1. Find symbol_under_cursor
    // 2. Get its scope
    // 3. Return its symbols
    vec!["ddd".to_string()]
}

pub(crate) fn definition(ast: Vec<Node>) -> Vec<String> {
    // 1. Find symbol_under_cursor
    // For vars:
    // 2. Get its scope
    // 3. Return its first usage
    // For other things
    // 3. Return its definition
    vec!["ddd".to_string()]
}

pub(crate) fn usages(ast: Vec<Node>) -> Vec<String> {
    // 1. Find symbol_under_cursor
    // For vars:
    // 2. Get its scope
    // 3. Return its usages
    // For other things
    // 3. Do a BFS through its scope (and their child scopes) to return all usages
    vec!["ddd".to_string()]
}
