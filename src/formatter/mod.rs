use crate::parser::node::Node;

macro_rules! push_if_some {
    ($token:expr, $parts:ident) => {
        if let Some(token) = $token {
            $parts.push(format!("{} ", token));
        }
    };
}

macro_rules! push_unpadded_if_some {
    ($token:expr, $parts:ident) => {
        if let Some(token) = $token {
            $parts.push(format!("{}", token));
        }
    };
}

macro_rules! optional_ident_list {
    ($prefix:expr, $postfix:expr, $list:ident, $line:expr, $col:expr, $options:expr) => {
        if let Some(list) = $list {
            format!(
                "{}{}{}",
                $prefix,
                list.iter()
                    .map(|n| format_node(n, $line, $col, $options))
                    .collect::<Vec<String>>()
                    .join(", "),
                $postfix
            )
        } else {
            String::from("")
        };
    };
}

pub struct FormatterOptions {
    pub max_line_length: usize,
    pub indent: usize,
}

/// Formats an array of nodes
///
/// # Arguments
///
/// * ast - The current part of the ast that will be formatted
/// * line - The current start line
/// * col - The current column / indentation level
/// * options - The formatter options
pub fn format(ast: &[Node], line: usize, col: usize, options: &FormatterOptions) -> String {
    let mut parts = Vec::new();

    for node in ast {
        match node {
            Node::Interface {
                name,
                token,
                extends,
                body,
                ..
            } => {
                parts.push(" ".repeat(col));
                parts.push(format!(
                    "{} {}{} {}",
                    token,
                    name,
                    optional_ident_list!(" extends ", "", extends, line, col, options),
                    format_node(body, line, col, options)
                ))
            }
            Node::ClassStatement {
                is_final,
                is_abstract,
                token,
                name,
                body,
                extends,
                implements,
                ..
            } => {
                parts.push(" ".repeat(col));
                push_if_some!(is_final, parts);
                push_if_some!(is_abstract, parts);

                parts.push(format!(
                    "{} {}{}{} {}",
                    token,
                    name,
                    optional_ident_list!(" extends ", "", extends, line, col, options),
                    optional_ident_list!(" implements ", "", implements, line, col, options),
                    format_node(body, line, col, options)
                ))
            }
            Node::ClassConstantDefinitionStatement {
                token,
                doc_comment,
                name,
                value,
                visibility,
            } => {
                parts.push(" ".repeat(col));
                push_if_some!(visibility, parts);

                parts.push(format!(
                    "{} {} = {};",
                    token,
                    name,
                    format_node(value, line, col, options)
                ))
            }
            Node::PropertyDefinitionStatement {
                name,
                visibility,
                is_abstract,
                is_static,
                value,
                ..
            } => {
                parts.push(" ".repeat(col));
                push_if_some!(is_abstract, parts);
                push_if_some!(visibility, parts);
                push_if_some!(is_static, parts);
                parts.push(name.to_string());

                if let Some(value) = value {
                    parts.push(format!(" = {}", format_node(value, line, col, options)));
                }

                parts.push(";".to_string());
            }
            Node::MethodDefinitionStatement {
                token,
                is_abstract,
                is_final,
                visibility,
                is_static,
                name,
                function,
                ..
            } => {
                parts.push(" ".repeat(col));

                push_if_some!(visibility, parts);
                push_if_some!(is_static, parts);
                push_if_some!(is_final, parts);
                push_if_some!(is_abstract, parts);

                parts.push(format!(
                    "{} {}{}",
                    token,
                    name,
                    format_node(function, line, col, options)
                ))
            }

            Node::UseTraitStatement {
                token,
                traits_usages,
            } => parts.push(format!(
                "{}{} {}",
                " ".repeat(col),
                token,
                format(traits_usages, line, col, options)
            )),

            Node::UseTraitAs {
                left,
                paa,
                visibility,
                as_name,
                as_token,
                member,
            } => {
                parts.push(" ".repeat(col));
                if let Some(left) = left {
                    parts.push(format_node(left, line, col, options));
                }

                push_unpadded_if_some!(paa, parts);
                parts.push(format_node(member, line, col, options));
                parts.push(format!(" {} ", as_token));
                if as_name.is_some() {
                    push_if_some!(visibility, parts);
                } else {
                    push_unpadded_if_some!(visibility, parts);
                }
                push_unpadded_if_some!(as_name, parts);

                parts.push(String::from(";\n"));
            }

            Node::UseTraitInsteadOf {
                insteadof,
                insteadof_list,
                left,
                member,
                paa,
            } => {
                parts.push(" ".repeat(col));
                if let Some(left) = left {
                    parts.push(format_node(left, line, col, options));
                }

                push_unpadded_if_some!(paa, parts);
                parts.push(format_node(member, line, col, options));
                parts.push(format!(" {} ", insteadof));
                parts.push(format(insteadof_list, line, col, options));
                parts.push(String::from(";\n"));
            }
            Node::UseTraitAlterationBlock {
                alteration_group_type_refs,
                oc,
                cc,
                alterations,
            } => parts.push(format!(
                "{} {}\n{}{}{}",
                alteration_group_type_refs
                    .iter()
                    .map(|n| format_node(n, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", "),
                oc,
                format(alterations, line, col + options.indent, options),
                " ".repeat(col),
                cc,
            )),

            Node::UseTrait { type_ref } => {
                parts.push(format!("{};\n", format_node(type_ref, line, col, options)))
            }

            Node::TypeRef(..) => parts.push(format_node(node, line, col, options)),

            Node::ExpressionStatement { expression } => {
                parts.push(format_node(expression, line, col, options));
                parts.push(";\n".to_string());
            }
            Node::InlineHtml { start, end } => {
                parts.push(start.to_string());
                push_unpadded_if_some!(end, parts);
            }
            _ => unimplemented!("{:?}", node),
        }
    }

    parts.join("")
}

/// Formats a single node
///
/// # Arguments
///
/// * node - The node that will be formatted
/// * line - The current start line
/// * col - The current column / indentation level
/// * options - The formatter options
fn format_node(node: &Node, line: usize, col: usize, options: &FormatterOptions) -> String {
    match node {
        Node::Block { oc, cc, statements } => {
            format!(
                "{}\n{}\n{}{}",
                oc,
                format(statements, line, col + options.indent, options),
                " ".repeat(col),
                cc
            )
        }
        Node::FunctionDefinitionStatement { cp, op, body, .. } => {
            // Interfaces and abstract methods do not have a body
            if let Some(body) = body {
                format!("{}{} {}", op, cp, format_node(body, line, col, options))
            } else {
                format!("{}{};", op, cp)
            }
        }
        Node::Binary { left, right, token } => {
            format!(
                "{}{} {} {}",
                " ".repeat(col),
                format_node(left, line, col, options),
                token,
                format_node(right, line, col, options)
            )
        }
        Node::New { token, class } => {
            format!("{} {}", token, format_node(class, line, col, options))
        }
        Node::Class {
            token,
            extends,
            body,
            arguments,
            implements,
        } => {
            format!(
                "{}{}{}{} {}",
                token,
                optional_ident_list!("(", ")", arguments, line, col, options),
                optional_ident_list!(" extends ", "", extends, line, col, options),
                optional_ident_list!(" implements ", "", implements, line, col, options),
                format_node(body, line, col, options)
            )
        }
        Node::Field {
            array,
            cb,
            index,
            ob,
        } => {
            if let Some(index) = index {
                format!(
                    "{}{}{}{}",
                    format_node(array, line, col, options),
                    ob,
                    format_node(index, line, col, options),
                    cb
                )
            } else {
                format!("{}{}{}", format_node(array, line, col, options), ob, cb)
            }
        }
        Node::Member {
            object,
            oc,
            member,
            cc,
            arrow,
        } => {
            if let Some(oc) = oc {
                format!(
                    "{}{}{}{}{}",
                    format_node(object, line, col, options),
                    arrow,
                    oc,
                    format_node(member, line, col, options),
                    cc.as_ref().unwrap()
                )
            } else {
                format!(
                    "{}{}{}",
                    format_node(object, line, col, options),
                    arrow,
                    format_node(member, line, col, options)
                )
            }
        }
        Node::StaticMember {
            object,
            member,
            pn,
            oc,
            cc,
        } => {
            if let Some(oc) = oc {
                format!(
                    "{}{}{}{}{}",
                    format_node(object, line, col, options),
                    pn,
                    oc,
                    format_node(member, line, col, options),
                    cc.as_ref().unwrap()
                )
            } else {
                format!(
                    "{}{}{}",
                    format_node(object, line, col, options),
                    pn,
                    format_node(member, line, col, options)
                )
            }
        }
        Node::Call {
            callee,
            cp,
            parameters,
            op,
        } => {
            format!(
                "{}{}{}{}",
                format_node(callee, line, col, options),
                op,
                parameters
                    .iter()
                    .map(|p| format_node(p, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", "),
                cp,
            )
        }
        Node::Identifier(token) | Node::Literal(token) | Node::Variable(token) => token.to_string(),
        Node::TypeRef(tokens) => tokens
            .iter()
            .map(|n| n.clone().to_string())
            .collect::<String>(),
        Node::Missing(..) => "<Missing>".to_string(),
        _ => unimplemented!("{:?}", node),
    }
}
