use crate::expression::Node;
use crate::parser::types;
use crate::parser::{Error, ExpressionListResult, ExpressionResult, Parser};
use crate::token::{Token, TokenType};

/// Parses a single namespace statement or namespace block
///
/// # Details
/// ```php
/// namespace /** from here **/My\Super\Duper\Namespace;/** to here **/
/// ```
pub(crate) fn namespace_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Namespace)?;
    let type_ref = types::type_ref(parser)?;

    if parser.next_token_one_of(&[TokenType::OpenCurly]) {
        Ok(Node::NamespaceBlock {
            token,
            type_ref: Box::new(type_ref),
            block: Box::new(parser.block()?),
        })
    } else if let Some(type_ref) = type_ref {
        parser.consume_end_of_statement()?;
        Ok(Node::NamespaceStatement {
            token,
            type_ref: Box::new(type_ref),
        })
    } else if let Some(token) = parser.next() {
        Err(Error::WrongTokenError {
            expected: vec![TokenType::OpenBrackets, TokenType::Identifier],
            found: token,
        })
    } else {
        Err(Error::UnexpectedEndOfFileError {
            expected: vec![TokenType::OpenBrackets, TokenType::Identifier],
        })
    }
}

fn symbol_import(parser: &mut Parser) -> ExpressionResult {
    if parser.consume_or_ignore(TokenType::Function).is_some() {
        let name = types::non_empty_type_ref(parser)?;

        if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
            return Ok(Node::UseFunction {
                token: None,
                function: Box::new(name),
                aliased: Some(alias),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        } else {
            return Ok(Node::UseFunction {
                token: None,
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
                token: None,
                constant: Box::new(name),
                aliased: Some(alias),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        } else {
            return Ok(Node::UseConst {
                token: None,
                constant: Box::new(name),
                aliased: None,
                alias: None,
            });
        }
    }

    let name = types::non_empty_type_ref(parser)?;

    if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
        Ok(Node::UseDeclaration {
            token: None,
            declaration: Box::new(name),
            aliased: Some(alias),
            alias: Some(parser.consume(TokenType::Identifier)?),
        })
    } else {
        Ok(Node::UseDeclaration {
            token: None,
            declaration: Box::new(name),
            aliased: None,
            alias: None,
        })
    }
}

fn symbol_imports(parser: &mut Parser) -> ExpressionListResult {
    let mut imports = Vec::new();

    imports.push(symbol_import(parser)?);

    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        if !parser.next_token_one_of(&[
            TokenType::Identifier,
            TokenType::Const,
            TokenType::Function,
        ]) {
            break;
        }

        imports.push(symbol_import(parser)?);
    }

    Ok(imports)
}

/// Parses a `use function ...` statement. The `use` is passed as it was already consumed
/// before consuming the `function` token in order to know what type of import
/// is done
pub(crate) fn use_function_statement(parser: &mut Parser, token: Token) -> ExpressionResult {
    let mut imports = Vec::new();

    loop {
        let name = types::non_empty_type_ref(parser)?;

        if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
            imports.push(Node::UseFunction {
                token: Some(token.clone()),
                function: Box::new(name),
                aliased: Some(alias),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        } else {
            imports.push(Node::UseFunction {
                token: Some(token.clone()),
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

    Ok(Node::UseFunctionStatement { token, imports })
}

/// Parses a `use const ...` statement. The `use` is passed as it was already consumed
/// before consuming the `function` token in order to know what type of import
/// is done
pub(crate) fn use_const_statement(parser: &mut Parser, token: Token) -> ExpressionResult {
    let mut imports = Vec::new();

    loop {
        let name = types::non_empty_type_ref(parser)?;

        if let Some(alias) = parser.consume_or_ignore(TokenType::As) {
            imports.push(Node::UseConst {
                token: Some(token.clone()),
                constant: Box::new(name),
                aliased: Some(alias),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        } else {
            imports.push(Node::UseConst {
                token: Some(token.clone()),
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

    Ok(Node::UseConstStatement { token, imports })
}

/// Parses a `use ...` statement. The `use` is passed as it was already consumed
/// before in order to know what type of import
/// is done
pub(crate) fn use_statement(parser: &mut Parser, token: Token) -> ExpressionResult {
    let mut imports = Vec::new();

    loop {
        let declaration = types::non_empty_namespace_ref(parser)?;

        // Ends with \, so it should be followed by a group wrapped in curly braces
        if declaration.last().unwrap().t == TokenType::NamespaceSeparator {
            imports.push(Node::GroupedUse {
                token: token.clone(),
                parent: Box::new(Node::TypeRef(declaration)),
                oc: parser.consume(TokenType::OpenCurly)?,
                uses: symbol_imports(parser)?,
                cc: parser.consume(TokenType::CloseCurly)?,
            });
        // Is aliased
        } else if let Some(aliased) = parser.consume_or_ignore(TokenType::As) {
            imports.push(Node::UseDeclaration {
                token: Some(token.clone()),
                declaration: Box::new(Node::TypeRef(declaration)),
                aliased: Some(aliased),
                alias: Some(parser.consume(TokenType::Identifier)?),
            });
        // Is a regular use
        } else {
            imports.push(Node::UseDeclaration {
                token: Some(token.clone()),
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

    Ok(Node::UseStatement { token, imports })
}
