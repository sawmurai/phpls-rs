use crate::expression::Node;
use crate::parser::types;
use crate::parser::{Parser, StatementResult};
use crate::statement::*;
use crate::token::TokenType;

/// Parses a single namespace statement
///
/// # Details
/// ```php
/// namespace /** from here **/My\Super\Duper\Namespace;/** to here **/
/// ```
pub(crate) fn namespace_statement(parser: &mut Parser) -> StatementResult {
    let type_ref = types::type_ref(parser)?;

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
        let name = types::non_empty_type_ref(parser)?;

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
        let name = types::non_empty_type_ref(parser)?;

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

    let name = types::non_empty_type_ref(parser)?;

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
        let name = types::non_empty_type_ref(parser)?;

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
        let name = types::non_empty_type_ref(parser)?;

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
        let declaration = types::non_empty_namespace_ref(parser)?;

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
