use crate::expression::Node;
use crate::parser::{Parser, StatementResult};
use crate::statement::*;
use crate::token::{Token, TokenType};

/// Parses an if statement
///
/// # Details
/// ```php
/// if /** from here **/ (true) {
///     do_stuff();
/// } elseif (false) {
///     other_stuff();
/// } else {
///     rest_stuff();
/// }
/// /** to here **/
/// ```
pub(crate) fn if_statement(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let condition = parser.expression()?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;
    let if_branch = IfBranch::new(condition, parser.statement()?);

    let mut elseif_branches = Vec::new();
    while parser.next_token_one_of(&[TokenType::ElseIf]) {
        parser.next();
        parser.consume_or_err(TokenType::OpenParenthesis)?;
        let condition = parser.expression()?;
        parser.consume_or_err(TokenType::CloseParenthesis)?;

        elseif_branches.push(IfBranch::new(condition, parser.statement()?));
    }

    let else_branch = match parser.peek() {
        Some(Token {
            t: TokenType::Else, ..
        }) => {
            parser.next();
            Some(parser.statement()?)
        }
        _ => None,
    };

    Ok(Box::new(IfStatement::new(
        Box::new(if_branch),
        elseif_branches,
        else_branch,
    )))
}

/// Parses a switch case
///
/// # Details
/// ```php
/// switch /** from here **/(true) {
///     case "bla":
///         echo "stuff";
/// }
/// /** to here **/
/// ```
pub(crate) fn switch_statement(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let expr = parser.expression()?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;
    parser.consume_or_err(TokenType::OpenCurly)?;

    let mut branches = Vec::new();
    while !parser.next_token_one_of(&[TokenType::CloseCurly]) {
        let cases_current_branch = case_list(parser)?;

        let mut statements = Vec::new();
        while !parser.next_token_one_of(&[
            TokenType::CloseCurly,
            TokenType::Case,
            TokenType::Default,
        ]) {
            statements.push(parser.statement()?);
        }

        branches.push(SwitchBranch::new(cases_current_branch, statements));
    }

    parser.next();

    Ok(Box::new(SwitchCase::new(expr, branches)))
}

pub(crate) fn case_list(parser: &mut Parser) -> Result<Vec<Option<Node>>, String> {
    let mut cases_current_branch = Vec::new();

    loop {
        match parser.peek() {
            Some(Token {
                t: TokenType::Default,
                ..
            }) => {
                cases_current_branch.push(None);
                parser.next();
                parser
                    .consume_or_err(TokenType::Colon)
                    .or_else(|_| parser.consume_end_of_statement())?;
            }
            Some(Token {
                t: TokenType::Case, ..
            }) => {
                parser.next();
                cases_current_branch.push(Some(parser.expression()?));
                parser
                    .consume_or_err(TokenType::Colon)
                    .or_else(|_| parser.consume_end_of_statement())?;
            }
            _ => {
                break;
            }
        }
    }
    Ok(cases_current_branch)
}
