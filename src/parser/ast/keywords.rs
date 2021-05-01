use crate::parser::token::ScriptStartType;

use super::super::node::Node;
use super::super::token::TokenType;
use super::super::{ExpressionResult, Parser};
use super::{arrays, expressions, functions};

/// Parses declare statements
pub(crate) fn declare_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Declare)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let directive = parser.consume(TokenType::Identifier)?;
    let assignment = parser.consume(TokenType::Assignment)?;
    let value = parser.consume_one_of(&[
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
    let cp = parser.consume(TokenType::CloseParenthesis)?;

    Ok(Node::DeclareStatement {
        token,
        op,
        directive,
        assignment,
        value,
        cp,
    })
}

/// Parses unset statements
pub(crate) fn unset_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Unset)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let vars = functions::non_empty_parameter_list(parser)?;
    let cp = parser.consume(TokenType::CloseParenthesis)?;

    Ok(Node::UnsetStatement {
        token,
        cp,
        op,
        vars,
    })
}

/// Parses die statements
pub(crate) fn die_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Die)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;

    let (expr, cp) = if let Some(cp) = parser.consume_or_ignore(TokenType::CloseParenthesis) {
        (None, cp)
    } else {
        (
            Some(Box::new(expressions::expression(parser, 0)?)),
            parser.consume(TokenType::CloseParenthesis)?,
        )
    };

    Ok(Node::DieStatement {
        token,
        cp,
        op,
        expr,
    })
}

/// Parses define statements
pub(crate) fn define_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Define)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let name = Box::new(expressions::expression(parser, 0)?);
    parser.consume(TokenType::Comma)?;
    let value = Box::new(expressions::expression(parser, 0)?);

    let is_caseinsensitive = if parser.consume_or_ignore(TokenType::Comma).is_some() {
        Some(parser.consume_one_of(&[TokenType::True, TokenType::False])?)
    } else {
        None
    };

    let cp = parser.consume(TokenType::CloseParenthesis)?;

    Ok(Node::DefineStatement {
        token,
        op,
        name,
        value,
        cp,
        is_caseinsensitive,
    })
}

pub(crate) fn echo_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Echo)?;
    let mut expressions = vec![expressions::expression(parser, 0)?];

    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        expressions.push(expressions::expression(parser, 0)?);
    }

    parser.consume_end_of_statement()?;

    Ok(Node::EchoStatement { token, expressions })
}

pub(crate) fn short_tag_echo_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::ScriptStart(ScriptStartType::Echo))?;
    let mut expressions = vec![expressions::expression(parser, 0)?];

    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        expressions.push(expressions::expression(parser, 0)?);
    }

    parser.consume_end_of_statement()?;

    parser.consume_or_ff_after(TokenType::ScriptEnd, &[TokenType::ScriptEnd])?;

    Ok(Node::EchoStatement { token, expressions })
}

pub(crate) fn print_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Print)?;
    let expressions = vec![expressions::expression(parser, 0)?];

    parser.consume_end_of_statement()?;

    Ok(Node::PrintStatement { token, expressions })
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

pub(crate) fn goto_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Goto)?;

    let label = parser.consume_identifier()?;

    parser.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;

    Ok(Node::GotoStatement { token, label })
}
