use crate::expression::Node;
use crate::parser::{ExpressionListResult, ExpressionResult, Parser, Result};
use crate::token::{Token, TokenType};

pub(crate) fn type_ref_list(parser: &mut Parser) -> ExpressionListResult {
    let mut type_refs = Vec::new();

    type_refs.push(non_empty_type_ref(parser)?);

    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        type_refs.push(non_empty_type_ref(parser)?);
    }

    Ok(type_refs)
}

// path -> identifier ("\" identifier)*
pub(crate) fn type_ref(parser: &mut Parser) -> Result<Option<Node>> {
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
        Ok(Some(Node::TypeRef(path)))
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

    Ok(Node::TypeRef(path))
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
pub(crate) fn type_ref_union(parser: &mut Parser) -> Result<Vec<Node>> {
    let mut paths = Vec::new();

    paths.push(non_empty_type_ref(parser)?);

    while parser.consume_or_ignore(TokenType::BinaryOr).is_some() {
        paths.push(non_empty_type_ref(parser)?);
    }

    Ok(paths)
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
        type_ref: Box::new(non_empty_type_ref(parser)?),
    })
}
