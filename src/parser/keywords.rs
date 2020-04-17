use crate::expression::Node;
use crate::parser::{arrays, functions};
use crate::parser::{expressions, ExpressionResult, Parser, StatementResult};
use crate::statement::*;
use crate::token::TokenType;

/// Parses declare statements
pub(crate) fn declare_statement(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::Declare)?;
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let directive = parser.consume(TokenType::Identifier)?;
    parser.consume_or_err(TokenType::Assignment)?;
    let val = parser.consume_one_of(&[
        TokenType::False,
        TokenType::True,
        TokenType::Null,
        TokenType::LongNumber,
        TokenType::DecimalNumber,
        TokenType::ExponentialNumber,
        TokenType::HexNumber,
        TokenType::BinaryNumber,
        TokenType::ConstantEncapsedString,
        TokenType::EncapsedAndWhitespaceString,
    ])?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;

    Ok(Box::new(DeclareStatement::new(directive, val)))
}

/// Parses unset statements
pub(crate) fn unset_statement(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::Unset)?;
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let vars = functions::non_empty_parameter_list(parser)?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;

    Ok(Box::new(UnsetStatement::new(vars)))
}

pub(crate) fn echo_statement(parser: &mut Parser) -> StatementResult {
    let mut values = Vec::new();

    values.push(expressions::expression(parser)?);

    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        values.push(expressions::expression(parser)?);
    }

    parser.consume_end_of_statement()?;

    Ok(Box::new(EchoStatement::new(values)))
}

pub(crate) fn print_statement(parser: &mut Parser) -> StatementResult {
    let mut values = Vec::new();

    values.push(expressions::expression(parser)?);

    parser.consume_end_of_statement()?;

    Ok(Box::new(PrintStatement::new(values)))
}

/// Parses the list destructuring operation
pub(crate) fn list(parser: &mut Parser) -> ExpressionResult {
    let start = parser.consume(TokenType::List)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let mut elements = Vec::new();

    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        // Empty element ... list(,,,$a)
        if parser.consume_or_ignore(TokenType::Comma).is_some() {
            continue;
        }

        elements.push(arrays::array_pair(parser)?);

        parser.consume_or_ignore(TokenType::Comma);
    }
    Ok(Node::List {
        token: start,
        op,
        elements,
        cp: parser.consume(TokenType::CloseParenthesis)?,
    })
}

pub(crate) fn goto_statement(parser: &mut Parser) -> StatementResult {
    let label = parser.consume_identifier()?;

    parser.consume_end_of_statement()?;

    Ok(Box::new(GotoStatement::new(label)))
}
