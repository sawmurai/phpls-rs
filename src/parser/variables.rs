use crate::node::Node;
use crate::parser::{expressions, ExpressionListResult, ExpressionResult, Parser};
use crate::token::{Token, TokenType};

pub(crate) fn variable(parser: &mut Parser) -> ExpressionResult {
    let variable = parser.consume(TokenType::Variable)?;

    // Named, regular variable. No problem here.
    if variable.label.is_some() {
        return Ok(Node::Variable(variable));
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

pub(crate) fn global_variables(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Global)?;
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

    Ok(Node::GlobalVariablesStatement { token, vars })
}

/// Parses a static variables definition. The token is passed as a parameter as it needs to be fetched
/// in the main loop to tell `static $a` from `static::$a`.
pub(crate) fn static_variables(parser: &mut Parser, token: Token) -> ExpressionResult {
    let mut assignments = Vec::new();

    loop {
        let variable = parser.consume(TokenType::Variable)?;

        if let Some(assignment) = parser.consume_or_ignore(TokenType::Assignment) {
            assignments.push(Node::StaticVariable {
                variable,
                assignment: Some(assignment),
                value: Some(Box::new(expressions::expression(parser)?)),
            });
        }

        if parser.consume_or_ignore(TokenType::Comma).is_none() {
            break;
        }
    }

    Ok(Node::StaticVariablesStatement { token, assignments })
}

/// Parses a global variables definition
pub(crate) fn const_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Const)?;
    let mut constants = Vec::new();

    constants.push(Node::Const {
        name: parser.consume_identifier()?,
        token: parser.consume(TokenType::Assignment)?,
        value: Box::new(expressions::expression(parser)?),
    });

    while parser.consume_or_ignore(TokenType::Semicolon).is_none() {
        parser.consume_or_err(TokenType::Comma)?;
        constants.push(Node::Const {
            name: parser.consume_identifier()?,
            token: parser.consume(TokenType::Assignment)?,
            value: Box::new(expressions::expression(parser)?),
        });
    }

    Ok(Node::ConstStatement { token, constants })
}

/// Parses all the arguments of a call
pub(crate) fn non_empty_lexical_variables_list(parser: &mut Parser) -> ExpressionListResult {
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
pub(crate) fn lexical_variable(parser: &mut Parser) -> ExpressionResult {
    Ok(Node::LexicalVariable {
        reference: parser.consume_or_ignore(TokenType::BinaryAnd),
        variable: parser.consume(TokenType::Variable)?,
    })
}
