use super::super::node::Node;
use super::super::token::TokenType;
use crate::parser::{expressions, variables};
use crate::parser::{ExpressionResult, Parser};

/// Parses class-member access and array access. This also includes non-method call member access!
pub(crate) fn call(parser: &mut Parser) -> ExpressionResult {
    let expr = expressions::primary(parser)?;

    init_call(parser, expr)
}

fn init_call(parser: &mut Parser, mut expr: Node) -> ExpressionResult {
    loop {
        if parser.next_token_one_of(&[TokenType::OpenParenthesis]) {
            expr = finish_call(parser, expr)?;

        // Accessing an object member
        } else if let Some(os) = parser.consume_or_ignore(TokenType::ObjectOperator) {
            // Using the ->{} syntax, so the member is the result of an expression
            if parser.consume_or_ignore(TokenType::OpenCurly).is_some() {
                expr = Node::Member {
                    object: Box::new(expr),
                    arrow: os,
                    member: Box::new(expressions::expression(parser)?),
                };
                parser.consume_or_err(TokenType::CloseCurly)?;

            // Using the ->member syntax, so the member is either an identifier or a variable
            } else if parser.next_token_one_of(&[TokenType::Variable]) {
                expr = Node::Member {
                    object: Box::new(expr),
                    arrow: os,
                    member: Box::new(variables::variable(parser)?),
                };
            } else {
                expr = Node::Member {
                    object: Box::new(expr),
                    arrow: os,
                    member: Box::new(Node::Literal(parser.consume_identifier()?)),
                };
            }
        // Accessing a static class member
        } else if let Some(pn) = parser.consume_or_ignore(TokenType::PaamayimNekudayim) {
            // Class property
            if parser.next_token_one_of(&[TokenType::Variable]) {
                expr = Node::StaticMember {
                    object: Box::new(expr),
                    pn,
                    member: Box::new(variables::variable(parser)?),
                };

            // Class method
            } else if parser.next_token_one_of(&[TokenType::OpenCurly]) {
                expr = Node::StaticMethod {
                    class: Box::new(expr),
                    pn,
                    oc: parser.consume(TokenType::OpenCurly)?,
                    method: Box::new(variables::variable(parser)?),
                    cc: parser.consume(TokenType::CloseCurly)?,
                };
            // Class constant
            } else {
                expr = Node::StaticMember {
                    object: Box::new(expr),
                    pn,
                    member: Box::new(Node::Literal(parser.consume_member()?)),
                };
            }
        } else if let Some(ob) = parser.consume_or_ignore(TokenType::OpenBrackets) {
            // TODO: Think about a nicer solution for $a[] = ...
            expr = match parser.consume_or_ignore(TokenType::CloseBrackets) {
                Some(cb) => Node::Field {
                    array: Box::new(expr),
                    ob,
                    index: None,
                    cb,
                },
                None => Node::Field {
                    array: Box::new(expr),
                    ob,
                    index: Some(Box::new(expressions::expression(parser)?)),
                    cb: parser.consume(TokenType::CloseBrackets)?,
                },
            };
        } else if let Some(oc) = parser.consume_or_ignore(TokenType::OpenCurly) {
            expr = Node::Field {
                array: Box::new(expr),
                ob: oc,
                index: Some(Box::new(expressions::expression(parser)?)),
                cb: parser.consume(TokenType::CloseCurly)?,
            };
        } else if let Some(assignment) = parser.consume_or_ignore(TokenType::Assignment) {
            // Something like static::$member = 1;
            expr = Node::Binary {
                left: Box::new(expr),
                token: assignment,
                right: Box::new(expressions::expression(parser)?),
            };
        } else {
            break;
        }
    }

    Ok(expr)
}

/// Parses all the parameters of a call
fn finish_call(parser: &mut Parser, expr: Node) -> ExpressionResult {
    let op = parser.consume(TokenType::OpenParenthesis)?;

    let mut parameters = Vec::new();
    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        parameters.push(expressions::expression(parser)?);

        if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
            break;
        } else {
            parser.consume_or_err(TokenType::Comma)?;
        }
    }
    Ok(Node::Call {
        callee: Box::new(expr),
        op,
        parameters,
        cp: parser.consume(TokenType::CloseParenthesis)?,
    })
}
