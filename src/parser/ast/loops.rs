use super::super::node::Node;
use super::super::token::TokenType;
use crate::parser::arrays;
use crate::parser::{expressions, ExpressionResult, Parser};

/// Parses a for loop
///
/// # Details
/// ```php
/// /** from here **/
/// for ($i = 0; $i < 100; $i++) {
///     do_stuff();
/// }
/// /** to here **/
/// ```
pub(crate) fn for_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::For)?;
    parser.consume_or_err(TokenType::OpenParenthesis)?;

    let mut init = Vec::new();
    while !parser.next_token_one_of(&[TokenType::Semicolon]) {
        init.push(expressions::expression_statement(parser)?);

        if parser.next_token_one_of(&[TokenType::Comma]) {
            parser.next();
        } else {
            break;
        }
    }

    parser.consume_or_err(TokenType::Semicolon)?;

    let mut condition = Vec::new();
    while !parser.next_token_one_of(&[TokenType::Semicolon]) {
        condition.push(expressions::expression_statement(parser)?);

        if parser.next_token_one_of(&[TokenType::Comma]) {
            parser.next();
        } else {
            break;
        }
    }

    parser.consume_or_err(TokenType::Semicolon)?;

    let mut step = Vec::new();
    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        step.push(expressions::expression_statement(parser)?);

        if parser.next_token_one_of(&[TokenType::Comma]) {
            parser.next();
        } else {
            break;
        }
    }
    parser.consume_or_err(TokenType::CloseParenthesis)?;

    let body = Box::new(parser.alternative_block_or_statement(TokenType::EndFor)?);

    Ok(Node::ForStatement {
        token,
        init,
        condition,
        step,
        body,
    })
}

/// Parses a while loop
///
/// # Details
/// ```php
/// /** from here **/
/// while (true) {
///     do_stuff();
/// }
/// /** to here **/
/// ```
pub(crate) fn while_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::While)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let condition = Box::new(expressions::expression(parser)?);
    let cp = parser.consume(TokenType::CloseParenthesis)?;
    let body = Box::new(parser.alternative_block_or_statement(TokenType::EndWhile)?);

    Ok(Node::WhileStatement {
        token,
        op,
        condition,
        cp,
        body,
    })
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
pub(crate) fn foreach_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Foreach)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let collection = Box::new(expressions::expression(parser)?);

    let as_token = parser.consume(TokenType::As)?;
    let kv = Box::new(arrays::array_pair(parser)?);
    let cp = parser.consume(TokenType::CloseParenthesis)?;

    let body = Box::new(parser.alternative_block_or_statement(TokenType::EndForeach)?);

    Ok(Node::ForEachStatement {
        token,
        op,
        collection,
        as_token,
        kv,
        cp,
        body,
    })
}

/// Parses a do-while loop
///
/// # Details
/// ```php
/// /** from here **/
/// do {
///     do_stuff();
/// } while (true);
/// /** to here **/
/// ```
pub(crate) fn do_while_statement(parser: &mut Parser) -> ExpressionResult {
    let do_token = parser.consume(TokenType::Do)?;
    let body = Box::new(parser.statement()?);
    let while_token = parser.consume(TokenType::While)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let condition = Box::new(expressions::expression(parser)?);
    let cp = parser.consume(TokenType::CloseParenthesis)?;
    parser.consume_end_of_statement()?;

    Ok(Node::DoWhileStatement {
        do_token,
        while_token,
        op,
        cp,
        condition,
        body,
    })
}
