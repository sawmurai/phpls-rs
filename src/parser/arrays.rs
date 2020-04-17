use crate::expression::{Expr, Node};
use crate::parser::expressions;
use crate::parser::variables;
use crate::parser::{ExpressionResult, Parser};
use crate::token::TokenType;

/// Parses an array surounded by regular brackets. To parse an array according to the old syntax
/// like `array(1, 2, 3)` use `arrays::old_array`.
pub(crate) fn array(parser: &mut Parser) -> ExpressionResult {
    let start = parser.consume(TokenType::OpenBrackets)?;
    let mut elements = Vec::new();

    while !parser.next_token_one_of(&[TokenType::CloseBrackets]) {
        // TODO: This is only allowed in a destructuring context. Probably need to split
        // this
        if parser.consume_or_ignore(TokenType::Comma).is_some() {
            continue;
        }

        elements.push(array_pair(parser)?);

        parser.consume_or_ignore(TokenType::Comma);
    }

    Ok(Node::Array {
        ob: start,
        elements,
        cb: parser.consume(TokenType::CloseBrackets)?,
    })
}

pub(crate) fn old_array(parser: &mut Parser) -> ExpressionResult {
    let start = parser.consume(TokenType::TypeArray)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let mut elements = Vec::new();

    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        elements.push(array_pair(parser)?);

        parser.consume_or_ignore(TokenType::Comma);
    }

    Ok(Node::OldArray {
        token: start,
        op,
        elements,
        cp: parser.consume(TokenType::CloseParenthesis)?,
    })
}

pub(crate) fn array_pair(parser: &mut Parser) -> ExpressionResult {
    // At this point key might as well be the value
    let key = expressions::expression(parser)?;

    if let Some(arrow) = parser.consume_or_ignore(TokenType::DoubleArrow) {
        // TODO: Raise warning if key is access by reference ... this no works

        // Todo: Rather check for scalarity
        if !key.is_offset() {
            return Err(format!(
                "Illegal offset type at line {} col {}",
                arrow.line, arrow.col,
            ));
        }

        if parser.next_token_one_of(&[TokenType::BinaryAnd]) {
            Ok(Node::ArrayElement {
                key: Some(Box::new(key)),
                arrow: Some(arrow),
                value: Box::new(variables::lexical_variable(parser)?),
            })
        } else {
            Ok(Node::ArrayElement {
                key: Some(Box::new(key)),
                arrow: Some(arrow),
                value: Box::new(expressions::expression(parser)?),
            })
        }
    } else {
        Ok(Node::ArrayElement {
            key: None,
            arrow: None,
            value: Box::new(key),
        })
    }
}
