use crate::parser::{Parser, StatementResult};
use crate::statement::*;
use crate::token::{Token, TokenType};

/// Parses a for loop
///
/// # Details
/// ```php
/// for /** from here **/ ($i = 0; $i < 100; $i++) {
///     do_stuff();
/// }
/// /** to here **/
/// ```
pub(crate) fn for_statement(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::OpenParenthesis)?;

    let mut init = Vec::new();
    while !parser.next_token_one_of(&[TokenType::Semicolon]) {
        init.push(parser.expression_statement()?);

        if parser.next_token_one_of(&[TokenType::Comma]) {
            parser.next();
        } else {
            break;
        }
    }

    parser.consume_or_err(TokenType::Semicolon)?;

    let mut condition = Vec::new();
    while !parser.next_token_one_of(&[TokenType::Semicolon]) {
        condition.push(parser.expression_statement()?);

        if parser.next_token_one_of(&[TokenType::Comma]) {
            parser.next();
        } else {
            break;
        }
    }

    parser.consume_or_err(TokenType::Semicolon)?;

    let mut step = Vec::new();
    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        step.push(parser.expression_statement()?);

        if parser.next_token_one_of(&[TokenType::Comma]) {
            parser.next();
        } else {
            break;
        }
    }
    parser.consume_or_err(TokenType::CloseParenthesis)?;

    let body = parser.statement()?;

    Ok(Box::new(ForStatement::new(init, condition, step, body)))
}

/// Parses a while loop
///
/// # Details
/// ```php
/// while /** from here **/(true) {
///     do_stuff();
/// }
/// /** to here **/
/// ```
pub(crate) fn while_statement(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let condition = parser.expression()?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;
    let body = match parser.peek() {
        Some(Token {
            t: TokenType::OpenCurly,
            ..
        }) => parser.block()?,
        Some(_) => Box::new(Block::new(vec![parser.statement()?])),
        None => return Err(String::from("Unexpected EOF!")),
    };

    Ok(Box::new(WhileStatement::new(condition, body)))
}

/// Parses a foreach loop
///
/// # Details
/// ```php
/// foreach /** from here **/($array as $k => $v) {
///     do_stuff();
/// }
/// /** to here **/
/// ```
pub(crate) fn foreach_statement(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let collection = parser.expression()?;

    parser.consume_or_err(TokenType::As)?;
    let key_value = parser.array_pair()?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;

    let body = match parser.peek() {
        Some(Token {
            t: TokenType::OpenCurly,
            ..
        }) => parser.block()?,
        Some(_) => Box::new(Block::new(vec![parser.statement()?])),
        None => return Err(String::from("Unexpected EOF!")),
    };

    Ok(Box::new(ForEachStatement::new(collection, key_value, body)))
}

/// Parses a do-while loop
///
/// # Details
/// ```php
/// do
/// /** from here **/ {
///     do_stuff();
/// } while (true);
/// /** to here **/
/// ```
pub(crate) fn do_while_statement(parser: &mut Parser) -> StatementResult {
    let body = parser.block()?;

    parser.consume_or_err(TokenType::While)?;
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let condition = parser.expression()?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;
    parser.consume_end_of_statement()?;

    Ok(Box::new(DoWhileStatement::new(condition, body)))
}
