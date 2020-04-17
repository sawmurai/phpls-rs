use crate::expression::Node;
use crate::parser::declarations;
use crate::parser::variables;
use crate::parser::{expressions, ArgumentListResult, ExpressionResult, Parser, StatementResult};
use crate::statement::*;
use crate::token::{Token, TokenType};

/// Parses the argument list of a function, excluding the parenthesis
///
/// # Details
/// ```php
/// function my_funy (/** from here **/string $a, int $b/** to here **/): void {
///     echo "Hello!";
/// }
/// ```
pub(crate) fn argument_list(parser: &mut Parser) -> ArgumentListResult {
    let mut arguments = Vec::new();

    if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        return Ok(None);
    }

    loop {
        let t = argument_type(parser)?;
        let spread = parser.consume_or_ignore(TokenType::Elipsis);
        let reference = parser.consume_or_ignore(TokenType::BinaryAnd);
        let name = parser.consume(TokenType::Variable)?;
        let has_default = parser.consume_or_ignore(TokenType::Assignment);

        let default_value = if has_default.is_some() {
            Some(Box::new(expressions::expression(parser)?))
        } else {
            None
        };

        arguments.push(Node::FunctionArgument {
            argument_type: Box::new(t),
            name,
            spread,
            reference,
            has_default,
            default_value,
        });

        if parser.next_token_one_of(&[TokenType::Comma]) {
            parser.next();
        } else {
            break;
        }
    }

    Ok(Some(arguments))
}

/// Parses the argument type of a function argument
///
/// # Details
/// ```php
/// function my_funy (/** from here **/string/** to here **/ $a, int $b): ?int {
///     echo "Hello!";
/// }
/// ```
pub(crate) fn argument_type(parser: &mut Parser) -> Result<Option<Node>, String> {
    if let Some(qm) = parser.consume_or_ignore(TokenType::QuestionMark) {
        Ok(Some(Node::ArgumentType {
            nullable: Some(qm),
            type_ref: Box::new(declarations::non_empty_type_ref(parser)?),
        }))
    } else if let Some(type_ref) = declarations::type_ref(parser)? {
        Ok(Some(Node::ArgumentType {
            nullable: None,
            type_ref: Box::new(type_ref),
        }))
    } else {
        Ok(None)
    }
}

/// Parses the return type of a function, excluding the colon
///
/// # Details
/// ```php
/// function my_funy (string $a, int $b): /** from here **/?int/** to here **/ {
///     echo "Hello!";
/// }
/// ```
pub(crate) fn return_type(parser: &mut Parser) -> Result<Option<Node>, String> {
    if let Some(colon) = parser.consume_or_ignore(TokenType::Colon) {
        Ok(Some(Node::ReturnType {
            token: colon,
            nullable: parser.consume_or_ignore(TokenType::QuestionMark),
            type_ref: Box::new(declarations::non_empty_type_ref(parser)?),
        }))
    } else {
        Ok(None)
    }
}

/// Parses a function definition by calling methods to parse the argument list, return type and body.
/// Handles named function
///
/// # Details
/// ```php
/// function /** from here **/my_funy (string $a, int $b): void {
///     echo "Hello!";
/// }
/// /** to here **/
/// ```
pub(crate) fn named_function(parser: &mut Parser) -> StatementResult {
    Ok(Box::new(NamedFunctionDefinitionStatement::new(
        parser.consume_or_ignore(TokenType::BinaryAnd),
        parser.consume_identifier()?,
        anonymous_function_statement(parser)?,
    )))
}

/// Parses a function definition by calling methods to parse the argument list, return type and body.
/// It only handles anonymous functions, since the name of a named function was parses previously ... and
/// a named function stripped off of the name is ... anonymous :)
///
/// # Details
/// ```php
/// function my_funy /** from here **/ (string $a, int $b): void {
///     echo "Hello!";
/// }
/// /** to here **/
/// ```
pub(crate) fn anonymous_function_statement(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let arguments = argument_list(parser)?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;

    let return_type = return_type(parser)?;

    if parser.consume_or_ignore(TokenType::Semicolon).is_some() {
        return Ok(Box::new(FunctionDefinitionStatement::new(
            arguments,
            return_type,
            None,
        )));
    }

    let body = parser.block()?;

    Ok(Box::new(FunctionDefinitionStatement::new(
        arguments,
        return_type,
        Some(body),
    )))
}

pub(crate) fn anonymous_function(
    parser: &mut Parser,
    is_static: Option<Token>,
) -> ExpressionResult {
    let token = parser.consume(TokenType::Function)?;

    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let arguments = argument_list(parser)?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;

    let uses = if parser.next_token_one_of(&[TokenType::Use]) {
        parser.next();
        Some(variables::non_empty_lexical_variables_list(parser)?)
    } else {
        None
    };

    let return_type = return_type(parser)?;

    let body = parser.block()?;

    Ok(Node::Function {
        is_static,
        token,
        arguments,
        return_type: Box::new(return_type),
        uses,
        body,
    })
}

/// Parses all the parameters of a call
pub(crate) fn non_empty_parameter_list(parser: &mut Parser) -> Result<Vec<Node>, String> {
    let mut arguments = Vec::new();
    arguments.push(expressions::expression(parser)?);

    parser.consume_or_ignore(TokenType::Comma);

    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        arguments.push(expressions::expression(parser)?);

        if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
            break;
        } else {
            parser.consume_or_err(TokenType::Comma)?;
        }
    }

    Ok(arguments)
}

/// Parses all the arguments of a call
/// Does not parse the surounding parenthesis so the caller can fetch and store them
pub(crate) fn parameter_list(parser: &mut Parser) -> Result<Vec<Node>, String> {
    let mut arguments = Vec::new();
    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        arguments.push(expressions::expression(parser)?);

        if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
            break;
        } else {
            parser.consume_or_err(TokenType::Comma)?;
        }
    }
    Ok(arguments)
}

pub(crate) fn return_statement(parser: &mut Parser) -> StatementResult {
    if parser.next_token_one_of(&[TokenType::Semicolon]) {
        parser.consume_end_of_statement()?;
        Ok(Box::new(ReturnStatement::new(None)))
    } else {
        let value = expressions::expression(parser)?;

        parser.consume_end_of_statement()?;

        Ok(Box::new(ReturnStatement::new(Some(value))))
    }
}
