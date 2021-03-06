use super::super::token::{Token, TokenType};
use super::super::{ArgumentListResult, ExpressionListResult, ExpressionResult, Parser, Result};
use super::comments;
use super::expressions;
use super::types;
use super::variables;
use super::{super::node::Node, attributes::attributes_block};

/// Parses the argument list of a function, excluding the parenthesis
///
/// # Details
/// ```php
/// function my_funy (/** from here **/string $a, int $b/** to here **/): void {
///     echo "Hello!";
/// }
/// ```
pub(crate) fn argument_list(
    parser: &mut Parser,
    doc_comment: &Option<Box<Node>>,
) -> ArgumentListResult {
    let mut arguments = Vec::new();

    if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        return Ok(None);
    }

    loop {
        let attributes = attributes_block(parser)?;
        let argument_type = argument_type(parser)?;
        let reference = parser.consume_or_ignore(TokenType::BinaryAnd);
        let spread = parser.consume_or_ignore(TokenType::Elipsis);
        let name = parser.consume(TokenType::Variable)?;
        let has_default = parser.consume_or_ignore(TokenType::Assignment);

        let default_value = if has_default.is_some() {
            Some(Box::new(expressions::expression(parser, 0)?))
        } else {
            None
        };

        let doc_comment = comments::param_comment_for(doc_comment, &name).map(Box::new);

        arguments.push(Node::FunctionArgument {
            argument_type,
            name,
            spread,
            reference,
            has_default,
            default_value,
            doc_comment,
            attributes,
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
pub(crate) fn argument_type(parser: &mut Parser) -> Result<Option<Box<Node>>> {
    if let Some(qm) = parser.consume_or_ignore(TokenType::QuestionMark) {
        Ok(Some(Box::new(Node::DataType {
            nullable: Some(qm),
            type_refs: types::non_empty_type_ref_union(parser)?,
        })))
    } else if let Some(type_refs) = types::type_ref_union(parser)? {
        Ok(Some(Box::new(Node::DataType {
            nullable: None,
            type_refs,
        })))
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
pub(crate) fn return_type(parser: &mut Parser) -> Result<Option<Box<Node>>> {
    if let Some(colon) = parser.consume_or_ignore(TokenType::Colon) {
        Ok(Some(Box::new(Node::ReturnType {
            token: colon,
            data_type: Box::new(types::data_type(parser)?),
        })))
    } else {
        Ok(None)
    }
}

/// Parses a function definition by calling methods to parse the argument list, return type and body.
/// Handles named function
///
/// # Details
/// ```php
/// /** from here **/function my_funy (string $a, int $b): void {
///     echo "Hello!";
/// }
/// /** to here **/
/// ```
pub(crate) fn named_function(
    parser: &mut Parser,
    doc_comment: &Option<Box<Node>>,
    attributes: Vec<Node>,
) -> ExpressionResult {
    Ok(Node::NamedFunctionDefinitionStatement {
        token: parser.consume(TokenType::Function)?,
        by_ref: parser.consume_or_ignore(TokenType::BinaryAnd),
        name: parser.consume_identifier()?,
        function: Box::new(anonymous_function_statement(parser, doc_comment)?),
        attributes,
    })
}

/// Parses a function definition by calling methods to parse the argument list, return type and body.
/// It only handles anonymous functions, since the name of a named function was parses previously ... and
/// a named function stripped off of the name is ... anonymous :)
///
/// # Details
/// ```php
/// function /** from here **/ (string $a, int $b): void {
///     echo "Hello!";
/// }
/// /** to here **/
/// ```
pub(crate) fn anonymous_function_statement(
    parser: &mut Parser,
    doc_comment: &Option<Box<Node>>,
) -> ExpressionResult {
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let arguments = argument_list(parser, doc_comment)?;
    let cp = parser.consume(TokenType::CloseParenthesis)?;

    let return_type = return_type(parser)?;

    if parser.consume_or_ignore(TokenType::Semicolon).is_some() {
        return Ok(Node::FunctionDefinitionStatement {
            op,
            arguments,
            cp,
            return_type,
            body: None,
            doc_comment: doc_comment.clone(),
        });
    }

    let body = Some(Box::new(parser.block()?));

    Ok(Node::FunctionDefinitionStatement {
        op,
        arguments,
        cp,
        return_type,
        body,
        doc_comment: doc_comment.clone(),
    })
}

pub(crate) fn arrow_function(
    parser: &mut Parser,
    is_static: Option<Token>,
    attributes: Vec<Node>,
) -> ExpressionResult {
    let token = parser.consume(TokenType::Fn)?;
    let by_ref = parser.consume_or_ignore(TokenType::BinaryAnd);

    let op = parser.consume(TokenType::OpenParenthesis)?;
    let arguments = argument_list(parser, &None)?;
    let cp = parser.consume(TokenType::CloseParenthesis)?;

    let return_type = return_type(parser)?;

    let arrow = parser.consume(TokenType::DoubleArrow)?;
    let body = Box::new(expressions::expression(parser, 0)?);

    Ok(Node::ArrowFunction {
        is_static,
        by_ref,
        token,
        op,
        arguments,
        cp,
        return_type,
        arrow,
        body,
        attributes,
    })
}

pub(crate) fn anonymous_function(
    parser: &mut Parser,
    is_static: Option<Token>,
    attributes: Vec<Node>,
) -> ExpressionResult {
    let token = parser.consume(TokenType::Function)?;
    let by_ref = parser.consume_or_ignore(TokenType::BinaryAnd);

    let op = parser.consume(TokenType::OpenParenthesis)?;
    let arguments = argument_list(parser, &None)?;
    let cp = parser.consume(TokenType::CloseParenthesis)?;

    let uses = if parser.next_token_one_of(&[TokenType::Use]) {
        parser.next();
        Some(variables::non_empty_lexical_variables_list(parser)?)
    } else {
        None
    };

    let return_type = return_type(parser)?;

    let body = Box::new(parser.block()?);

    Ok(Node::Function {
        is_static,
        by_ref,
        token,
        op,
        arguments,
        cp,
        return_type,
        uses,
        body,
        attributes,
    })
}

/// Parses all the parameters of a call
pub(crate) fn non_empty_parameter_list(parser: &mut Parser) -> ExpressionListResult {
    let mut arguments = vec![expressions::expression(parser, 0)?];

    parser.consume_or_ignore(TokenType::Comma);

    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        arguments.push(expressions::expression(parser, 0)?);

        if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
            break;
        } else {
            parser.consume_or_ff_after(TokenType::Comma, &[TokenType::CloseParenthesis])?;
        }
    }

    Ok(arguments)
}

/// Parses all the arguments of a call
/// Does not parse the surounding parenthesis so the caller can fetch and store them
pub(crate) fn parameter_list(parser: &mut Parser) -> ExpressionListResult {
    let mut arguments = Vec::new();
    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        arguments.push(expressions::expression(parser, 0)?);

        if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
            break;
        } else {
            parser.consume_or_ff_after(TokenType::Comma, &[TokenType::CloseParenthesis])?;
        }
    }
    Ok(arguments)
}

pub(crate) fn return_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Return)?;

    if parser.next_token_one_of(&[TokenType::Semicolon]) {
        parser.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;
        Ok(Node::ReturnStatement {
            token,
            expression: None,
        })
    } else {
        let value = Box::new(expressions::expression(parser, 0)?);

        parser.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;

        Ok(Node::ReturnStatement {
            token,
            expression: Some(value),
        })
    }
}
