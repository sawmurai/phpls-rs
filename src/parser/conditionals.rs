use crate::expression::Node;
use crate::parser::{expressions, ExpressionResult, Parser, Result};
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

    // Alternative syntax
    if let Some(colon) = parser.consume_or_ignore(TokenType::Colon) {
        let mut statements = Vec::new();

        while !parser.next_token_one_of(&[TokenType::EndIf, TokenType::Else, TokenType::ElseIf]) {
            statements.push(parser.statement()?);
        }

        let mut terminator =
            parser.consume_one_of(&[TokenType::EndIf, TokenType::Else, TokenType::ElseIf])?;

        let if_branch = Node::IfBranch {
            token,
            op,
            condition,
            cp,
            body: Box::new(Node::AlternativeBlock {
                colon,
                statements,
                terminator: terminator.clone(),
            }),
        };

        // Collect all elseif branches
        let mut elseif_branches = Vec::new();
        while terminator.t == TokenType::ElseIf {
            let token = terminator.clone();

            let mut statements = Vec::new();
            let op = parser.consume(TokenType::OpenParenthesis)?;
            let condition = Box::new(expressions::expression(parser)?);
            let cp = parser.consume(TokenType::CloseParenthesis)?;
            let colon = parser.consume(TokenType::Colon)?;

            while !parser.next_token_one_of(&[TokenType::EndIf, TokenType::Else, TokenType::ElseIf])
            {
                statements.push(parser.statement()?);
            }
            terminator =
                parser.consume_one_of(&[TokenType::EndIf, TokenType::Else, TokenType::ElseIf])?;

            elseif_branches.push(Node::IfBranch {
                token,
                op,
                condition,
                cp,
                body: Box::new(Node::AlternativeBlock {
                    colon,
                    statements,
                    terminator: terminator.clone(),
                }),
            });
        }

        let else_branch = if terminator.t == TokenType::Else {
            Some(Box::new(Node::ElseBranch {
                token: terminator.clone(),
                body: Box::new(parser.alternative_block(TokenType::EndIf)?),
            }))
        } else {
            None
        };

        return Ok(Node::IfStatement {
            if_branch: Box::new(if_branch),
            elseif_branches,
            else_branch,
        });
    }

    // Regular syntax
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
    let body = Box::new(switch_body(parser)?);

    Ok(Node::SwitchCase {
        token,
        op,
        expr,
        cp,
        body,
    })
}

/// Parses the body part of the switch case and can handle { -> } as well as : -> endswitch
fn switch_body(parser: &mut Parser) -> ExpressionResult {
    let mut branches = Vec::new();

    let (start, terminator_type) = if parser.next_token_one_of(&[TokenType::Colon]) {
        (parser.consume(TokenType::Colon)?, TokenType::EndSwitch)
    } else {
        (parser.consume(TokenType::OpenCurly)?, TokenType::CloseCurly)
    };

    while !parser.next_token_one_of(&[terminator_type.clone()]) {
        let cases_current_branch = case_list(parser)?;

        let mut statements = Vec::new();
        while !parser.next_token_one_of(&[
            terminator_type.clone(),
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

    let end = parser.consume(terminator_type)?;

    Ok(Node::SwitchBody {
        start,
        branches,
        end,
    })
}

pub(crate) fn case_list(parser: &mut Parser) -> Result<Vec<Option<Node>>> {
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
