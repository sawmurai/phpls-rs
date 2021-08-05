use crate::parser::node::Node;
use std::cmp::min;

pub mod classes;
pub mod expressions;
pub mod loops;
pub mod v2;

macro_rules! push_if_some {
    ($token:expr, $parts:expr) => {
        if let Some(token) = $token {
            $parts.push(format!("{} ", token));
        }
    };
}

macro_rules! str_if_some {
    ($token:expr) => {
        if let Some(token) = $token {
            token.to_string()
        } else {
            String::from("")
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
    ($prefix:expr, $postfix:expr, $list:expr, $line:expr, $col:expr, $options:expr) => {
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

macro_rules! optional_ident {
    ($prefix:expr, $postfix:expr, $ident:expr, $line:expr, $col:expr, $options:expr) => {
        if let Some(ident) = $ident {
            format!(
                "{}{}{}",
                $prefix,
                format_node(&ident, $line, $col, $options),
                $postfix
            )
        } else {
            String::from("")
        };
    };
}

#[derive(Default)]
pub struct FormatterOptions {
    pub max_line_length: usize,
    pub indent: usize,
}

pub fn format_file(ast: &[Node], line: usize, col: usize, options: &FormatterOptions) -> String {
    let result = format(ast, line, col, options);

    format!("{}\n", result.trim_end())
}

/// Formats an array of nodes
///
/// # Arguments
///
/// * ast - The current part of the ast that will be formatted
/// * line - The current start line
/// * col - The current column / indentation level
/// * options - The formatter options
fn format(ast: &[Node], line: usize, col: usize, options: &FormatterOptions) -> String {
    let mut parts = Vec::new();

    let mut prev_line_end: u32 = 0;
    for node in ast {
        let range = node.range();
        // The gap between this nodes start and the prev nodes end. The prev_line_end init with 0
        // should already prevent a gap at the beginning of a block. Do we want this?
        if prev_line_end != 0 && range.0 .0 > prev_line_end {
            let gap = (range.0 .0 - prev_line_end - 1) as usize;

            parts.push("\n".repeat(min(gap, 1)));
        }

        prev_line_end = range.1 .0;

        match node {
            Node::NamedFunctionDefinitionStatement {
                name,
                function,
                token,
                attributes,
                ..
            } => {
                let indent = " ".repeat(col);

                parts.push(format(attributes, line, col, options));
                parts.push(format!(
                    "{}{} {}{}",
                    indent,
                    token,
                    name,
                    format_node(function, line, col, options)
                ));
            }
            Node::Interface {
                name,
                token,
                extends,
                body,
                ..
            } => {
                parts.push(" ".repeat(col));
                parts.push(format!(
                    "{} {}{} \n{}{}",
                    token,
                    name,
                    optional_ident_list!(" extends ", "", extends, line, col, options),
                    " ".repeat(col),
                    format_node(body, line, col, options)
                ))
            }
            Node::ClassStatement(stmt) => {
                parts.push(format(&stmt.attributes, line, col, options));
                parts.push(" ".repeat(col));
                push_if_some!(stmt.is_final.as_ref(), parts);
                push_if_some!(stmt.is_abstract.as_ref(), parts);

                parts.push(format!(
                    "{} {}{}{}\n{}{}",
                    stmt.token,
                    stmt.name,
                    optional_ident!(" extends ", "", stmt.extends.as_ref(), line, col, options),
                    optional_ident_list!(
                        " implements ",
                        "",
                        stmt.implements.as_ref(),
                        line,
                        col,
                        options
                    ),
                    " ".repeat(col),
                    format_node(&stmt.body, line, col, options)
                ))
            }
            Node::ClassConstantDefinitionStatement {
                token,
                consts,
                visibility,
                attributes,
                ..
            } => {
                parts.push(format(attributes, line, col, options));
                parts.push(" ".repeat(col));
                push_if_some!(visibility, parts);

                let mut printed_consts = Vec::new();
                let mut iter = consts.iter();
                printed_consts.push(format_node(iter.next().unwrap(), line, 0, options));

                let mut offset = token.to_string().len() + 1;
                if let Some(visibility) = visibility {
                    offset += visibility.to_string().len();
                }

                for c in iter {
                    printed_consts.push(format_node(c, line, col + offset, options));
                }

                parts.push(format!("{} ", token));
                parts.push(printed_consts.join(",\n "));
                parts.push(";\n".to_string());
            }
            Node::Property { name, value } => {
                parts.push(name.to_string());

                if let Some(value) = value {
                    parts.push(" = ".to_string());
                    parts.push(format_node(value, line, col, options));
                }
            }
            Node::PropertyDefinitionStatement {
                visibility,
                is_abstract,
                is_static,
                properties,
                attributes,
                ..
            } => {
                parts.push(format(attributes, line, col, options));
                parts.push(" ".repeat(col));
                push_if_some!(is_abstract, parts);
                push_if_some!(visibility, parts);
                push_if_some!(is_static, parts);
                parts.push(format(properties, line, col, options));

                parts.push(";\n".to_string());
            }
            Node::MethodDefinitionStatement {
                token,
                is_abstract,
                is_final,
                visibility,
                is_static,
                name,
                function,
                attributes,
                ..
            } => {
                parts.push(format(attributes, line, col, options));

                parts.push(" ".repeat(col));
                push_if_some!(visibility, parts);
                push_if_some!(is_static, parts);
                push_if_some!(is_final, parts);
                push_if_some!(is_abstract, parts);

                parts.push(format!(
                    "{} {}{}\n",
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
                "{} {}\n{}{}{}\n",
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
                parts.push(" ".repeat(col));
                parts.push(format_node(expression, line, col, options));
                parts.push(";\n".to_string());
            }
            Node::InlineHtml { start, end } => {
                parts.push(start.to_string());
                push_unpadded_if_some!(end, parts);
            }
            Node::ReturnStatement { token, expression } => {
                parts.push(" ".repeat(col));
                parts.push(token.to_string());
                if let Some(expression) = expression {
                    parts.push(" ".to_string());
                    parts.push(format_node(expression, line, col, options));
                }
                parts.push(";\n".to_string());
            }

            Node::UseFunctionStatement { imports, .. } => {
                parts.push(" ".repeat(col));
                parts.push("use function ".to_owned());
                parts.push(
                    imports
                        .iter()
                        .map(|n| format_node(n, line, col, options))
                        .collect::<Vec<String>>()
                        .join(", "),
                );
                parts.push(";\n".to_string());
            }
            Node::UseConstStatement { imports, .. } => {
                parts.push(" ".repeat(col));
                parts.push("use const ".to_owned());
                parts.push(
                    imports
                        .iter()
                        .map(|n| format_node(n, line, col, options))
                        .collect::<Vec<String>>()
                        .join(", "),
                );
                parts.push(";\n".to_string());
            }
            Node::UseStatement { token, imports } => {
                parts.push(" ".repeat(col));
                parts.push(token.to_string());
                parts.push(" ".to_string());
                parts.push(
                    imports
                        .iter()
                        .map(|n| format_node(n, line, col, options))
                        .collect::<Vec<String>>()
                        .join(", "),
                );
                parts.push(";\n".to_string());
            }
            Node::Attribute {
                ats,
                expressions,
                cb,
            } => {
                parts.push(" ".repeat(col));
                parts.push(format!(
                    "{}{}{}",
                    ats,
                    expressions
                        .iter()
                        .map(|n| format_node(n, col, line, options))
                        .collect::<Vec<String>>()
                        .join(", "),
                    cb
                ));
                parts.push("\n".to_string());
            }

            Node::UseDeclaration { declaration, .. } => {
                parts.push(format_node(declaration, line, col, options));
            }

            Node::IfStatement {
                if_branch,
                elseif_branches,
                else_branch,
            } => parts.push(format!(
                "{}{} {} {}\n",
                " ".repeat(col),
                format_node(if_branch, line, col, options),
                elseif_branches
                    .iter()
                    .map(|i| format_node(i, line, col, options))
                    .collect::<Vec<String>>()
                    .join(""),
                if let Some(else_branch) = else_branch {
                    format_node(else_branch, line, col, options)
                } else {
                    String::from("")
                }
            )),
            Node::StaticVariablesStatement {
                token,
                assignments: vars,
            }
            | Node::GlobalVariablesStatement { token, vars } => parts.push(format!(
                "{} {}",
                token,
                vars.iter()
                    .map(|var| format_node(var, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", ")
            )),

            Node::SwitchCase { expr, body, .. } => parts.push(format!(
                "switch ({}) {}\n",
                format_node(expr, line, col, options),
                format_node(body, line, col, options)
            )),

            Node::TryCatch {
                try_block,
                catch_blocks,
                finally_block,
                ..
            } => parts.push(format!(
                "{}try {}{} {}\n",
                " ".repeat(col),
                format_node(try_block, line, col, options),
                catch_blocks
                    .iter()
                    .map(|cb| format_node(cb, line, col, options))
                    .collect::<Vec<String>>()
                    .join(""),
                if let Some(finally_block) = finally_block {
                    format_node(finally_block, line, col, options)
                } else {
                    String::from("")
                }
            )),

            Node::NamespaceStatement { token, type_ref } => parts.push(format!(
                "\n{} {};\n",
                token,
                format_node(type_ref, line, col, options)
            )),
            Node::NamespaceBlock {
                token,
                type_ref,
                block,
            } => parts.push(if let Some(type_ref) = type_ref {
                format!(
                    "{} {} {}\n",
                    token,
                    format_node(type_ref, line, col, options),
                    format_node(block, col + options.indent, line, options)
                )
            } else {
                format!(
                    "{} {}\n",
                    token,
                    format_node(block, col + options.indent, line, options)
                )
            }),

            Node::WhileStatement {
                condition, body, ..
            } => parts.push(format!(
                "{}while ({}) {}\n",
                " ".repeat(col),
                format_node(condition, line, col, options),
                format_node(body, line, col, options)
            )),
            Node::DoWhileStatement {
                condition, body, ..
            } => parts.push(format!(
                "{}do {} while ({});\n",
                " ".repeat(col),
                format_node(body, line, col, options),
                format_node(condition, line, col, options),
            )),
            Node::ForStatement {
                init,
                condition,
                step,
                body,
                ..
            } => parts.push(format!(
                "{}for ({}; {}; {}) {}\n",
                " ".repeat(col),
                init.iter()
                    .map(|i| format_node(i, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", "),
                condition
                    .iter()
                    .map(|c| format_node(c, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", "),
                step.iter()
                    .map(|s| format_node(s, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", "),
                format_node(body, line, col, options)
            )),
            Node::ForEachStatement {
                collection,
                kv,
                body,
                ..
            } => parts.push(format!(
                "{}foreach ({} as {}) {}\n",
                " ".repeat(col),
                format_node(collection, line, col, options),
                format_node(kv, line, col, options),
                format_node(body, line, col, options)
            )),
            Node::TokenStatement { token, .. } => {
                parts.push(" ".repeat(col));
                parts.push(token.to_string());
                parts.push(";".to_owned())
            }

            Node::Static { token, expr }
            | Node::EchoStatement {
                token,
                expressions: expr,
            }
            | Node::ConstStatement {
                token,
                constants: expr,
            }
            | Node::PrintStatement {
                token,
                expressions: expr,
            } => parts.push(format!(
                "{}{} {};\n",
                " ".repeat(col),
                token,
                expr.iter()
                    .map(|parameter| format_node(parameter, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", "),
            )),
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
        Node::Grouping(grouped) => format!("({})", format_node(grouped, line, col, options)),
        Node::FileInclude { token, resource } => {
            format!("{} {}", token, format_node(resource, line, col, options))
        }
        Node::UseDeclaration {
            declaration, alias, ..
        } => {
            if let Some(alias) = alias {
                format!(
                    "{} as {}",
                    format_node(declaration, line, col, options),
                    alias
                )
            } else {
                format_node(declaration, line, col, options)
            }
        }

        Node::GroupedUse {
            parent,
            uses,
            oc,
            cc,
            ..
        } => {
            format!(
                "{}{}\n{}{}{}\n{}",
                format_node(parent, line, col, options),
                oc,
                " ".repeat(col + options.indent),
                uses.iter()
                    .map(|n| format_node(n, line, col + options.indent, options))
                    .collect::<Vec<String>>()
                    .join(&format!(",\n{}", " ".repeat(col + options.indent))),
                " ".repeat(col),
                cc
            )
        }
        Node::ClassConstant { name, value, .. } => {
            format!(
                "{}{} = {}",
                " ".repeat(col),
                name,
                format_node(value, line, col, options)
            )
        }
        Node::Block { statements, .. } => {
            format!(
                "{{\n{}{}}}",
                format(statements, line, col + options.indent, options),
                " ".repeat(col),
            )
        }
        Node::FunctionDefinitionStatement {
            cp,
            op,
            body,
            arguments,
            return_type,
            ..
        } => {
            let arguments = if let Some(arguments) = arguments {
                arguments
                    .iter()
                    .map(|a| format_node(a, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", ")
            } else {
                String::new()
            };

            let return_type = if let Some(rt) = return_type {
                format_node(rt, line, col, options)
            } else {
                String::new()
            };

            // Interfaces and abstract methods do not have a body
            if let Some(body) = body {
                format!(
                    "{}{}{}{}\n{}{}",
                    op,
                    arguments,
                    cp,
                    return_type,
                    " ".repeat(col),
                    format_node(body, line, col, options)
                )
            } else {
                format!("{}{}{}{};", op, arguments, cp, return_type)
            }
        }
        Node::ArrowFunction {
            is_static,
            token,
            op,
            arguments,
            cp,
            arrow,
            return_type,
            body,
            attributes,
            ..
        } => {
            let arguments = if let Some(arguments) = arguments {
                arguments
                    .iter()
                    .map(|a| format_node(a, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", ")
            } else {
                String::new()
            };

            let return_type = if let Some(rt) = return_type {
                format_node(rt, line, col, options)
            } else {
                String::new()
            };
            let is_static = if let Some(is_static) = is_static {
                is_static.to_string()
            } else {
                String::new()
            };

            let mut attributes_formatted = String::from("");
            if !attributes.is_empty() {
                attributes_formatted
                    .push_str(&format(attributes, line, 0, options).trim().to_string());
                attributes_formatted.push(' ');
            }

            format!(
                "{}{}{} {}{}{}{} {} {}",
                attributes_formatted,
                is_static,
                token,
                op,
                arguments,
                cp,
                return_type,
                arrow,
                format_node(body, line, col, options)
            )
        }
        Node::Function {
            op,
            cp,
            is_static,
            body,
            attributes,
            arguments,
            return_type,
            token,
            uses,
            ..
        } => {
            let arguments = if let Some(arguments) = arguments {
                arguments
                    .iter()
                    .map(|a| format_node(a, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", ")
            } else {
                String::new()
            };

            let return_type = if let Some(rt) = return_type {
                format_node(rt, line, col, options)
            } else {
                String::new()
            };
            let is_static = if let Some(is_static) = is_static {
                is_static.to_string()
            } else {
                String::new()
            };

            let mut attributes_formatted = String::from("");
            if !attributes.is_empty() {
                attributes_formatted
                    .push_str(&format(attributes, line, 0, options).trim().to_string());
                attributes_formatted.push(' ');
            }

            format!(
                "{}{}{} {}{}{}{}{} {}",
                attributes_formatted,
                is_static,
                token,
                op,
                arguments,
                cp,
                return_type,
                optional_ident_list!("(", ")", uses, line, col, options),
                format_node(body, line, col, options)
            )
        }
        Node::ReturnType { token, data_type } => {
            format!("{} {}", token, format_node(data_type, line, col, options))
        }
        Node::Binary { left, right, token } => {
            format!(
                "{} {} {}",
                format_node(left, line, col, options),
                token,
                format_node(right, line, col, options)
            )
        }
        Node::New { token, class }
        | Node::Clone {
            token,
            object: class,
        }
        | Node::YieldFrom { token, expr: class }
        | Node::ThrowStatement {
            token,
            expression: class,
        } => {
            format!("{} {}", token, format_node(class, line, col, options))
        }
        Node::Class {
            token,
            extends,
            body,
            arguments,
            implements,
            attributes,
        } => {
            let attributes = if !attributes.is_empty() {
                format!(
                    "{} ",
                    format(attributes, line, 0, options).trim().to_string()
                )
            } else {
                String::from("")
            };

            format!(
                "{}{}{}{}{} {}",
                attributes,
                token,
                optional_ident_list!("(", ")", arguments, line, col, options),
                optional_ident!(" extends ", "", extends, line, col, options),
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
            callee, parameters, ..
        } => {
            format!(
                "{}({})",
                format_node(callee, line, col, options),
                parameters
                    .iter()
                    .map(|p| format_node(p, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", "),
            )
        }
        Node::DataType {
            nullable,
            type_refs,
        } => {
            let trs = type_refs
                .iter()
                .map(|tr| format_node(tr, line, col, options))
                .collect::<Vec<String>>()
                .join(" | ");

            if let Some(nullable) = nullable {
                format!("{}{}", nullable, trs)
            } else {
                trs
            }
        }
        Node::FunctionArgument {
            argument_type,
            default_value,
            name,
            spread,
            reference,
            doc_comment: _,
            attributes,
            ..
        } => {
            let mut parts = Vec::new();

            if !attributes.is_empty() {
                parts.push(format(attributes, line, 0, options).trim().to_string());
                parts.push(String::from(" "));
            }
            parts.push(optional_ident!("", " ", argument_type, line, col, options));
            push_unpadded_if_some!(spread, parts);
            push_unpadded_if_some!(reference, parts);
            parts.push(name.to_string());
            parts.push(optional_ident!(
                " = ",
                " ",
                default_value,
                line,
                col,
                options
            ));

            parts.join("")
        }
        Node::Identifier(token) | Node::Literal(token) | Node::Variable(token) => token.to_string(),
        Node::TypeRef(tokens) => tokens.to_fqdn(),
        Node::Missing(..) => "<Missing>".to_string(),
        Node::Array { cb, elements, ob } => {
            format!(
                "{}{}{}",
                ob,
                elements
                    .iter()
                    .map(|element| format_node(element, line, col, options))
                    .collect::<Vec<String>>()
                    .join(", "),
                cb
            )
        }
        Node::ArrayElement { arrow, key, value } => {
            let mut parts = Vec::new();
            parts.push(optional_ident!("", " ", key, line, col, options));
            push_if_some!(arrow, parts);
            parts.push(format_node(value, line, col, options));
            parts.join("")
        }
        Node::NamedParameter { name, colon, expr } => {
            format!(
                "{}{} {}",
                name,
                colon,
                format_node(expr, line, col, options)
            )
        }
        Node::Unary { token, expr } => {
            format!("{} {}", token, format_node(expr, line, col, options))
        }

        Node::DocComment { comment, .. } => comment.to_string(),
        Node::PostUnary { expr, token } => {
            format!("{}{}", format_node(expr, line, col, options), token)
        }
        Node::Const { name, token, value } => format!(
            "{} {} = {}",
            token,
            name,
            format_node(value, line, col, options)
        ),
        Node::Ternary {
            check,
            qm,
            true_arm,
            colon,
            false_arm,
        } => {
            if let Some(ta) = true_arm {
                format!(
                    "{} {} {} {} {}",
                    format_node(check, line, col, options),
                    qm,
                    format_node(ta, line, col, options),
                    colon,
                    format_node(false_arm, line, col, options)
                )
            } else {
                format!(
                    "{} {} {} {}",
                    format_node(check, line, col, options),
                    qm,
                    colon,
                    format_node(false_arm, line, col, options)
                )
            }
        }
        Node::LexicalVariable {
            reference,
            variable,
        } => format!("{}{}", str_if_some!(reference), variable),
        Node::AliasedVariable { variable, expr } => {
            format!("{}{}", variable, format_node(expr, line, col, options))
        }
        Node::DynamicVariable {
            variable,
            oc,
            expr,
            cc,
        } => format!(
            "{}{}{}{}",
            variable,
            oc,
            format_node(expr, line, col, options),
            cc
        ),
        Node::StaticVariable {
            variable, value, ..
        } => {
            if let Some(value) = value {
                format!("{} = {}", variable, format_node(value, line, col, options))
            } else {
                format!("{}", variable)
            }
        }
        Node::OldArray {
            token,
            op,
            elements,
            cp,
        }
        | Node::List {
            token,
            op,
            elements,
            cp,
        } => format!(
            "{}{}{}{}",
            token,
            op,
            elements
                .iter()
                .map(|element| format_node(element, line, col, options))
                .collect::<Vec<String>>()
                .join(", "),
            cp,
        ),
        Node::Isset {
            isset,
            op,
            parameters,
            cp,
        } => format!(
            "{}{}{}{}",
            isset,
            op,
            parameters
                .iter()
                .map(|parameter| format_node(parameter, line, col, options))
                .collect::<Vec<String>>()
                .join(", "),
            cp
        ),

        Node::DieStatement {
            token,
            op,
            expr,
            cp,
        } => {
            if let Some(expr) = expr {
                format!(
                    "{}{}{}{}",
                    token,
                    op,
                    format_node(expr, line, col, options),
                    cp,
                )
            } else {
                format!("{}{}{}", token, op, cp,)
            }
        }
        Node::UnsetStatement {
            token,
            op,
            vars,
            cp,
        } => format!(
            "{}{}{}{}",
            token,
            op,
            vars.iter()
                .map(|var| format_node(var, line, col, options))
                .collect::<Vec<String>>()
                .join(", "),
            cp,
        ),
        Node::Empty {
            empty,
            op,
            parameters,
            cp,
        } => format!(
            "{}{}{}{}",
            empty,
            op,
            parameters
                .iter()
                .map(|parameter| format_node(parameter, line, col, options))
                .collect::<Vec<String>>()
                .join(", "),
            cp,
        ),

        Node::Exit {
            exit: token,
            parameters,
            ..
        }
        | Node::HaltCompiler {
            hc: token,
            parameters,
            ..
        }
        | Node::Die {
            die: token,
            parameters,
            ..
        } => {
            if let Some(parameters) = parameters {
                format!(
                    "{}({})",
                    token,
                    parameters
                        .iter()
                        .map(|parameter| format_node(parameter, line, col, options))
                        .collect::<Vec<String>>()
                        .join(", "),
                )
            } else {
                format!("{} ", token)
            }
        }

        Node::Yield { token, expr } => {
            if let Some(expr) = expr {
                format!("{} {}", token, format_node(expr, line, col, options))
            } else {
                token.to_string()
            }
        }
        Node::UseConst {
            token,
            constant: thing,
            aliased,
            alias,
        }
        | Node::UseFunction {
            token,
            function: thing,
            aliased,
            alias,
        } => {
            let mut result = String::new();

            if let Some(token) = token {
                result.push_str(&token.to_string());
                result.push(' ');
            }
            if let Some(aliased) = aliased.as_ref() {
                result.push_str(&format!(
                    "{} {} {}",
                    format_node(thing, col, line, options),
                    aliased,
                    alias.as_ref().unwrap()
                ));
            } else {
                result.push_str(&format_node(thing, col, line, options));
            }

            result
        }
        Node::GotoStatement { token, label } => format!("{}{}", token, label),
        Node::LabelStatement { label, colon } => format!("{}{}", label, colon),
        Node::DeclareStatement {
            directive,
            assignment,
            op,
            cp,
            token,
            ..
        } => format!("{}{}{}{}{}", token, op, directive, assignment, cp),
        Node::DefineStatement {
            name,
            value,
            op,
            cp,
            is_caseinsensitive,
            ..
        } => {
            if let Some(is_caseinsensitive) = is_caseinsensitive {
                format!(
                    "{}{},{},{}{}",
                    op,
                    format_node(name, line, col, options),
                    format_node(value, line, col, options),
                    is_caseinsensitive,
                    cp
                )
            } else {
                format!(
                    "{}{},{}{}",
                    op,
                    format_node(value, line, col, options),
                    format_node(name, line, col, options),
                    cp
                )
            }
        }
        Node::AlternativeBlock {
            statements,
            terminator,
            ..
        } => format!(
            ":\n{}\n{}{};",
            statements
                .iter()
                .map(|i| format_node(i, line, col + options.indent, options))
                .collect::<Vec<String>>()
                .join(", "),
            " ".repeat(col),
            terminator
        ),
        Node::IfBranch {
            token,
            condition,
            body,
            ..
        } => format!(
            "{} ({}) {}",
            token,
            format_node(condition, line, col, options),
            format_node(body, line, col, options),
        ),
        Node::ElseBranch { body, .. } => {
            format!("else {}", format_node(body, line, col, options),)
        }
        Node::SwitchBranch { cases, body } => {
            let caselist = cases
                .iter()
                .map(|n| {
                    if let Some(n) = n {
                        format!(
                            "{}case {}:",
                            " ".repeat(col),
                            format_node(n, line, col + options.indent, options)
                        )
                    } else {
                        format!("{}default:", " ".repeat(col),)
                    }
                })
                .collect::<Vec<String>>()
                .join("\n");

            format!(
                "{}\n{}",
                caselist,
                format(body, line, col + options.indent, options),
            )
        }
        Node::SwitchBody {
            start,
            branches,
            end,
        } => format!(
            "{}\n{}{}{}",
            start,
            " ".repeat(col),
            branches
                .iter()
                .map(|i| format_node(i, line, col + options.indent, options))
                .collect::<Vec<String>>()
                .join("\n"),
            end,
        ),
        Node::CatchBlock {
            types, var, body, ..
        } => format!(
            " catch ({} {}) {}",
            types
                .iter()
                .map(|i| format_node(i, line, col, options))
                .collect::<Vec<String>>()
                .join(" | "),
            var,
            format_node(body, line, col, options)
        ),
        Node::FinallyBlock { body, .. } => {
            dbg!(body);
            format!("finally {}", format_node(body, line, col, options))
        }

        _ => unimplemented!("{:?}", node),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    fn ast(source: &str) -> Vec<Node> {
        let mut scanner = Scanner::new(&format!("<?php\n{}", source));

        scanner.scan().unwrap();

        Parser::ast(scanner.tokens).unwrap().0
    }

    #[test]
    fn test_formats_conditionals() {
        let src = "\
if ($a == 1) {
    echo '1';
} elseif ($a == 2) {
    echo '2';
} else {
    echo '3';
}

switch ($rofl) {
    case 1:
    case 2:
        doIt();
        break;
    default:
        echo 'oh oh';
        return;
}
";

        let opt = FormatterOptions {
            indent: 4,
            max_line_length: 0,
        };
        assert_eq!(src, format_file(&ast(src), 0, 0, &opt));
    }

    #[test]
    fn test_formats_try_catch() {
        let src = "\
try {
    echo 'hello my friend';
} catch (TestException | RoflExeption $e) {
    echo 'Well, this failed!';
} catch (OhOhException $e) {
    echo 'even worse!';
} finally {
    cleanUp();
    cleanUp();
}
";

        let opt = FormatterOptions {
            indent: 4,
            max_line_length: 0,
        };
        assert_eq!(src, format_file(&ast(src), 0, 0, &opt));
    }

    #[test]
    fn test_formats_loops() {
        let src = "\
while (true) {
    echo 'still there';
}

for ($i = 0, $y = 2; $y > $i; $y++) {
    echo 'going strong';
}

do {
    nothing();
} while ($gogogogo);

foreach ($rofl as $copter => $sropter) {
    nothing();
}
";

        let opt = FormatterOptions {
            indent: 4,
            max_line_length: 0,
        };
        assert_eq!(src, format_file(&ast(src), 0, 0, &opt));
    }

    #[test]
    fn test_formats_namespace_blocks() {
        let src = "
namespace Lol;

namespace {
}

namespace Rofl {
}

namespace Rofl\\Copter {
}
";

        let opt = FormatterOptions {
            indent: 4,
            max_line_length: 0,
        };
        assert_eq!(src, format_file(&ast(src), 0, 0, &opt));
    }

    #[test]
    fn test_respects_one_additional_line_between_statements() {
        let src = "\
echo 'test';

echo 'test2';
";

        let opt = FormatterOptions {
            indent: 4,
            max_line_length: 0,
        };
        assert_eq!(src, format_file(&ast(src), 0, 0, &opt));
    }
}
