use crate::expression::Node;
use crate::parser::{Parser, StatementResult};
use crate::statement::*;
use crate::token::{Token, TokenType};

/// Parses a path list, pipe separated. This needs to become a type-list!
///
/// # Details
/// ```php
/// catch (/** from here **/Exception1 | Exception2/** to here **/ $e) {}
///     echo "stuff";
/// }
///
/// ```
pub(crate) fn type_ref_union(parser: &mut Parser) -> Result<Vec<Node>, String> {
    let mut paths = Vec::new();

    paths.push(non_empty_type_ref(parser)?);

    while parser.consume_or_ignore(TokenType::BinaryOr).is_some() {
        if let Some(type_ref) = type_ref(parser)? {
            paths.push(type_ref);
        } else {
            return Err(String::from("Expected type after | operator"));
        }
    }

    Ok(paths)
}

/// Parses a single namespace statement
///
/// # Details
/// ```php
/// namespace /** from here **/My\Super\Duper\Namespace;/** to here **/
/// ```
pub(crate) fn namespace_statement(parser: &mut Parser) -> StatementResult {
    let type_ref = type_ref(parser)?;

    if parser.next_token_one_of(&[TokenType::OpenCurly]) {
        Ok(Box::new(NamespaceBlock::new(type_ref, parser.block()?)))
    } else if let Some(type_ref) = type_ref {
        parser.consume_end_of_statement()?;
        Ok(Box::new(NamespaceStatement::new(type_ref)))
    } else {
        Err("Empty path after namespace".to_owned())
    }
}

pub(crate) fn symbol_import(parser: &mut Parser) -> Result<Node, String> {
    if parser.consume_or_ignore(TokenType::Function).is_some() {
        let name = non_empty_type_ref(parser)?;

        if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
            return Ok(Node::UseFunction {
                function: Box::new(name),
                aliased: Some(alias),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        } else {
            return Ok(Node::UseFunction {
                function: Box::new(name),
                aliased: None,
                alias: None,
            });
        }
    }

    if parser.consume_or_ignore(TokenType::Const).is_some() {
        let name = non_empty_type_ref(parser)?;

        if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
            return Ok(Node::UseConst {
                constant: Box::new(name),
                aliased: Some(alias),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        } else {
            return Ok(Node::UseConst {
                constant: Box::new(name),
                aliased: None,
                alias: None,
            });
        }
    }

    let name = non_empty_type_ref(parser)?;

    if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
        Ok(Node::UseDeclaration {
            declaration: Box::new(name),
            aliased: Some(alias),
            alias: Some(parser.consume(TokenType::Identifier)?),
        })
    } else {
        Ok(Node::UseDeclaration {
            declaration: Box::new(name),
            aliased: None,
            alias: None,
        })
    }
}

pub(crate) fn symbol_imports(parser: &mut Parser) -> Result<Vec<Node>, String> {
    let mut symbols = Vec::new();

    symbols.push(symbol_import(parser)?);

    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        if !parser.next_token_one_of(&[
            TokenType::Identifier,
            TokenType::Const,
            TokenType::Function,
        ]) {
            break;
        }

        symbols.push(symbol_import(parser)?);
    }

    Ok(symbols)
}

pub(crate) fn use_function_statement(parser: &mut Parser) -> StatementResult {
    let mut symbols = Vec::new();

    loop {
        let name = non_empty_type_ref(parser)?;

        if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
            symbols.push(Node::UseFunction {
                function: Box::new(name),
                aliased: Some(alias),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        } else {
            symbols.push(Node::UseFunction {
                function: Box::new(name),
                aliased: None,
                alias: None,
            });
        }

        if parser.consume_or_ignore(TokenType::Comma).is_some() {
            continue;
        }

        break;
    }

    parser.consume_end_of_statement()?;

    Ok(Box::new(UseFunctionStatement::new(symbols)))
}

pub(crate) fn use_const_statement(parser: &mut Parser) -> StatementResult {
    let mut symbols = Vec::new();

    loop {
        let name = non_empty_type_ref(parser)?;

        if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
            symbols.push(Node::UseConst {
                constant: Box::new(name),
                aliased: Some(alias),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        } else {
            symbols.push(Node::UseConst {
                constant: Box::new(name),
                aliased: None,
                alias: None,
            });
        }

        if parser.consume_or_ignore(TokenType::Comma).is_some() {
            continue;
        }

        break;
    }

    parser.consume_end_of_statement()?;

    Ok(Box::new(UseConstStatement::new(symbols)))
}

// use -> "use" ("function" | "const")? path (("{" use_group "}") | ("as" identifier))?
pub(crate) fn use_statement(parser: &mut Parser) -> StatementResult {
    let mut imports = Vec::new();

    loop {
        let declaration = non_empty_namespace_ref(parser)?;

        // Ends with \, so it should be followed by a group wrapped in curly braces
        if declaration.last().unwrap().t == TokenType::NamespaceSeparator {
            imports.push(Node::GroupedUse {
                parent: Box::new(Node::TypeRef(declaration)),
                oc: parser.consume(TokenType::OpenCurly)?,
                uses: symbol_imports(parser)?,
                cc: parser.consume(TokenType::CloseCurly)?,
            });
        // Is aliased
        } else if let Some(aliased) = parser.consume_or_ignore(TokenType::As) {
            imports.push(Node::UseDeclaration {
                declaration: Box::new(Node::TypeRef(declaration)),
                aliased: Some(aliased),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        // Is a regular use
        } else {
            imports.push(Node::UseDeclaration {
                declaration: Box::new(Node::TypeRef(declaration)),
                aliased: None,
                alias: None,
            });
        }

        if parser.consume_or_ignore(TokenType::Comma).is_some() {
            continue;
        }

        parser.consume_end_of_statement()?;

        break;
    }

    Ok(Box::new(UseStatement::new(imports)))
}

// use -> "use" identifier (, identifier)*
pub(crate) fn use_trait_statement(parser: &mut Parser) -> StatementResult {
    let statement = Box::new(UseTraitStatement::new(
        parser.consume(TokenType::Use)?,
        type_ref_list(parser)?,
    ));

    parser.consume_end_of_statement()?;

    Ok(statement)
}

pub(crate) fn type_ref_list(parser: &mut Parser) -> Result<Vec<Node>, String> {
    let mut type_refs = Vec::new();

    type_refs.push(non_empty_type_ref(parser)?);

    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        type_refs.push(non_empty_type_ref(parser)?);
    }

    Ok(type_refs)
}

// path -> identifier ("\" identifier)*
pub(crate) fn type_ref(parser: &mut Parser) -> Result<Option<Node>, String> {
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
pub(crate) fn non_empty_type_ref(parser: &mut Parser) -> Result<Node, String> {
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
pub(crate) fn non_empty_namespace_ref(parser: &mut Parser) -> Result<Vec<Token>, String> {
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
