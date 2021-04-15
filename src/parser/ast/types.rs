use super::super::node::Node;
use super::super::token::{Token, TokenType};
use super::super::{ExpressionListResult, ExpressionResult, Parser, Result};

pub(crate) fn type_ref_list(parser: &mut Parser) -> ExpressionListResult {
    let mut type_refs = vec![non_empty_type_ref(parser)?];

    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        type_refs.push(non_empty_type_ref(parser)?);
    }

    Ok(type_refs)
}

// path -> identifier ("\" identifier)*
pub(crate) fn type_ref(parser: &mut Parser) -> Result<Option<Box<Node>>> {
    let mut path = Vec::new();

    if let Some(ns) = parser.consume_or_ignore(TokenType::NamespaceSeparator) {
        path.push(ns);
    }

    while let Some(identifier) = parser.peek() {
        if identifier.is_identifier() {
            path.push(parser.next().unwrap());

            if let Some(ns) = parser.consume_or_ignore(TokenType::NamespaceSeparator) {
                path.push(ns);
            } else {
                break;
            }
        } else {
            break;
        }
    }

    if path.is_empty() {
        Ok(None)
    } else {
        Ok(Some(Box::new(Node::TypeRef(path.into()))))
    }
}

// non_empty_type_ref -> identifier ("\" identifier)*
pub(crate) fn non_empty_type_ref(parser: &mut Parser) -> ExpressionResult {
    let mut path = Vec::new();

    if let Some(ns) = parser.consume_or_ignore(TokenType::NamespaceSeparator) {
        path.push(ns);
    }

    path.push(parser.consume_identifier()?);

    while let Some(ns) = parser.consume_or_ignore(TokenType::NamespaceSeparator) {
        path.push(ns);
        path.push(parser.consume_identifier()?);
    }

    Ok(Node::TypeRef(path.into()))
}

// non_empty_namespace_ref -> "\"? identifier ("\" identifier)* "\"?
pub(crate) fn non_empty_namespace_ref(parser: &mut Parser) -> Result<Vec<Token>> {
    let mut path = Vec::new();

    if let Some(ns) = parser.consume_or_ignore(TokenType::NamespaceSeparator) {
        path.push(ns);
    }

    path.push(parser.consume_identifier()?);

    while let Some(ns) = parser.consume_or_ignore(TokenType::NamespaceSeparator) {
        path.push(ns);

        if let Some(ident) = parser.peek() {
            if ident.is_identifier() {
                path.push(parser.next().unwrap());

                continue;
            }
        }

        break;
    }
    Ok(path)
}

/// Parses a path list, pipe separated. This needs to become a type-list!
///
/// # Details
/// ```php
/// catch (/** from here **/Exception1 | Exception2/** to here **/ $e) {}
///     echo "stuff";
/// }
///
/// ```
pub(crate) fn non_empty_type_ref_union(parser: &mut Parser) -> Result<Vec<Node>> {
    let mut paths = vec![non_empty_type_ref(parser)?];

    while parser.consume_or_ignore(TokenType::BinaryOr).is_some() {
        paths.push(non_empty_type_ref(parser)?);
    }

    Ok(paths)
}

/// Parses an optional path list, pipe separated
///
/// # Details
/// ```php
/// catch (/** from here **/Exception1 | Exception2/** to here **/ $e) {}
///     echo "stuff";
/// }
///
/// ```
pub(crate) fn type_ref_union(parser: &mut Parser) -> Result<Option<Vec<Node>>> {
    let mut paths = Vec::new();

    if let Some(tr) = type_ref(parser)? {
        paths.push(*tr);
    } else {
        return Ok(None);
    }

    while parser.consume_or_ignore(TokenType::BinaryOr).is_some() {
        paths.push(non_empty_type_ref(parser)?);
    }

    Ok(Some(paths))
}

/// Parse a variable type
///
/// # Details
/// ```php
/// function something(/** from here */?int | string/** to here */) {}
/// ```
pub(crate) fn data_type(parser: &mut Parser) -> Result<Node> {
    Ok(Node::DataType {
        nullable: parser.consume_or_ignore(TokenType::QuestionMark),
        type_refs: non_empty_type_ref_union(parser)?,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        formatter::{format_file, FormatterOptions},
        parser::{scanner::Scanner, Parser},
    };

    #[test]
    fn test_parses_union_type() {
        let mut scanner = Scanner::new(
            "<?php function func(string | int $param): string | int { return $param; }",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
function func(string | int $param): string | int {
    return $param;
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }
}
