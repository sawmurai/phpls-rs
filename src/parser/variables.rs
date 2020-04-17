use crate::expression::Node;
use crate::parser::{expressions, Parser, StatementResult};
use crate::statement::*;
use crate::token::TokenType;

pub(crate) fn variable(parser: &mut Parser) -> Result<Node, String> {
    let variable = parser.consume(TokenType::Variable)?;

    // Named, regular variable. No problem here.
    if variable.label.is_some() {
        return Ok(Node::Literal(variable));
    }

    // Dynamic member ${expr}
    if let Some(oc) = parser.consume_or_ignore(TokenType::OpenCurly) {
        return Ok(Node::DynamicVariable {
            variable,
            oc,
            expr: Box::new(expressions::expression(parser)?),
            cc: parser.consume(TokenType::CloseCurly)?,
        });
    }

    // Aliased variable $$$$a
    Ok(Node::AliasedVariable {
        variable,
        expr: Box::new(expressions::primary(parser)?),
    })
}

pub(crate) fn global_variables(parser: &mut Parser) -> StatementResult {
    let mut vars = Vec::new();
    vars.push(variable(parser)?);

    parser.consume_or_ignore(TokenType::Comma);

    while !parser.next_token_one_of(&[TokenType::Semicolon]) {
        vars.push(variable(parser)?);

        if parser.next_token_one_of(&[TokenType::Semicolon]) {
            break;
        } else {
            parser.consume_or_err(TokenType::Comma)?;
        }
    }

    parser.consume_end_of_statement()?;

    Ok(Box::new(GlobalVariablesStatement::new(vars)))
}

pub(crate) fn static_variables(parser: &mut Parser) -> StatementResult {
    let mut variables = Vec::new();

    loop {
        let variable = parser.consume(TokenType::Variable)?;

        if let Some(assignment) = parser.consume_or_ignore(TokenType::Assignment) {
            variables.push(Node::StaticVariable {
                variable,
                assignment: Some(assignment),
                value: Some(Box::new(expressions::expression(parser)?)),
            });
        }

        if parser.consume_or_ignore(TokenType::Comma).is_none() {
            break;
        }
    }
    Ok(Box::new(StaticVariablesStatement::new(variables)))
}

pub(crate) fn const_statement(parser: &mut Parser) -> StatementResult {
    let mut constants = Vec::new();

    constants.push(Node::Const {
        name: parser.consume_identifier()?,
        token: parser.consume(TokenType::Assignment)?,
        value: Box::new(expressions::expression(parser)?),
    });

    while let None = parser.consume_or_ignore(TokenType::Semicolon) {
        parser.consume_or_err(TokenType::Comma)?;
        constants.push(Node::Const {
            name: parser.consume_identifier()?,
            token: parser.consume(TokenType::Assignment)?,
            value: Box::new(expressions::expression(parser)?),
        });
    }

    Ok(Box::new(ConstStatement::new(constants)))
}

/// Parses all the arguments of a call
pub(crate) fn non_empty_lexical_variables_list(parser: &mut Parser) -> Result<Vec<Node>, String> {
    parser.consume_or_err(TokenType::OpenParenthesis)?;

    let mut arguments = Vec::new();
    arguments.push(lexical_variable(parser)?);

    parser.consume_or_ignore(TokenType::Comma);

    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        arguments.push(lexical_variable(parser)?);

        if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
            break;
        } else {
            parser.consume_or_err(TokenType::Comma)?;
        }
    }

    parser.consume_or_err(TokenType::CloseParenthesis)?;

    Ok(arguments)
}

/// Parses a lexical variable, used in a "use ()" list for example
pub(crate) fn lexical_variable(parser: &mut Parser) -> Result<Node, String> {
    Ok(Node::LexicalVariable {
        reference: parser.consume_or_ignore(TokenType::BinaryAnd),
        variable: parser.consume(TokenType::Variable)?,
    })
}
