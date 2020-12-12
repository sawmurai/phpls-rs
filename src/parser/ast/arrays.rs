use super::super::node::Node;
use super::super::token::TokenType;
use crate::parser::expressions;
use crate::parser::variables;
use crate::parser::{Error, ExpressionResult, Parser};

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
            return Err(Error::IllegalOffsetType { expr: key });
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::token::{Token, TokenType};
    use crate::parser::Parser;

    #[test]
    fn test_parses_an_array() {
        let mut tokens = vec![
            Token::new(TokenType::OpenBrackets, 1, 1),
            Token::named(TokenType::LongNumber, 1, 1, "10"),
            Token::new(TokenType::Comma, 1, 1),
            Token::named(TokenType::LongNumber, 1, 1, "12"),
            Token::new(TokenType::CloseBrackets, 10, 10),
        ];
        tokens.reverse();

        let mut parser = Parser {
            doc_comments: Vec::new(),
            errors: Vec::new(),
            tokens,
        };

        let expected = Node::Array {
            ob: Token::new(TokenType::OpenBrackets, 1, 1),
            cb: Token::new(TokenType::CloseBrackets, 10, 10),
            elements: vec![
                Node::ArrayElement {
                    key: None,
                    arrow: None,
                    value: Box::new(Node::Literal(Token::named(
                        TokenType::LongNumber,
                        1,
                        1,
                        "10",
                    ))),
                },
                Node::ArrayElement {
                    key: None,
                    arrow: None,
                    value: Box::new(Node::Literal(Token::named(
                        TokenType::LongNumber,
                        1,
                        1,
                        "12",
                    ))),
                },
            ],
        };

        assert_eq!(expected, array(&mut parser).unwrap());
    }
}
