use crate::expression::Node;
use crate::parser::{expressions, ExpressionResult, Parser};
use crate::token::{Token, TokenType};

/// Parses an if statement
///
/// # Details
/// ```php
/// /** from here **/
/// if (true) {
///     do_stuff();
/// } elseif (false) {
///     other_stuff();
/// } else {
///     rest_stuff();
/// }
/// /** to here **/
/// ```
pub(crate) fn if_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::If)?;

    let op = parser.consume(TokenType::OpenParenthesis)?;
    let condition = Box::new(expressions::expression(parser)?);
    let cp = parser.consume(TokenType::CloseParenthesis)?;

    let if_branch = Node::IfBranch {
        token,
        op,
        condition,
        cp,
        body: Box::new(parser.statement()?),
    };

    let mut elseif_branches = Vec::new();
    while let Some(token) = parser.consume_or_ignore(TokenType::ElseIf) {
        let op = parser.consume(TokenType::OpenParenthesis)?;
        let condition = Box::new(expressions::expression(parser)?);
        let cp = parser.consume(TokenType::CloseParenthesis)?;

        elseif_branches.push(Node::IfBranch {
            token,
            op,
            condition,
            cp,
            body: Box::new(parser.statement()?),
        });
    }

    let else_branch = if let Some(else_branch) = parser.consume_or_ignore(TokenType::Else) {
        Some(Box::new(Node::ElseBranch {
            token: else_branch,
            body: Box::new(parser.statement()?),
        }))
    } else {
        None
    };

    Ok(Node::IfStatement {
        if_branch: Box::new(if_branch),
        elseif_branches,
        else_branch,
    })
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
pub(crate) fn switch_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Switch)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let expr = Box::new(expressions::expression(parser)?);
    let cp = parser.consume(TokenType::CloseParenthesis)?;
    let oc = parser.consume(TokenType::OpenCurly)?;

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

        branches.push(Node::SwitchBranch {
            cases: cases_current_branch,
            body: statements,
        });
    }

    let cc = parser.consume(TokenType::CloseCurly)?;

    Ok(Node::SwitchCase {
        token,
        op,
        expr,
        cp,
        oc,
        branches,
        cc,
    })
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
                cases_current_branch.push(Some(expressions::expression(parser)?));
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
