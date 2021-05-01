use super::super::node::Node;
use super::super::token::{Token, TokenType};
use super::super::{expressions, ExpressionListResult, ExpressionResult, Parser};

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
            expr: Box::new(expressions::expression(parser, 0)?),
            cc: parser.consume(TokenType::CloseCurly)?,
        });
    }

    // Collect the list of aliases and return the actual variable at the end
    let mut list = vec![variable];
    let mut root = loop {
        let variable = parser.consume_or_ff_before(
            TokenType::Variable,
            &[TokenType::Semicolon, TokenType::ScriptEnd],
        )?;

        if variable.label.is_some() {
            break Node::Variable(variable);
        }

        list.push(variable);
    };

    for link in list.drain(..).rev() {
        root = Node::AliasedVariable {
            variable: link,
            expr: Box::new(root),
        };
    }

    // Aliased variable $$$$a
    Ok(root)
}

pub(crate) fn global_variables(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Global)?;
    let mut vars = vec![variable(parser)?];

    parser.consume_or_ignore(TokenType::Comma);

    while !parser.next_token_one_of(&[TokenType::Semicolon]) {
        vars.push(variable(parser)?);

        if parser.next_token_one_of(&[TokenType::Semicolon]) {
            break;
        } else {
            parser.consume_or_ff_after(TokenType::Comma, &[TokenType::Semicolon])?;
        }
    }

    parser.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;

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
                value: Some(Box::new(expressions::expression(parser, 0)?)),
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

    let mut constants = vec![Node::Const {
        name: parser.consume_identifier()?,
        token: parser.consume(TokenType::Assignment)?,
        value: Box::new(expressions::expression(parser, 0)?),
    }];

    while parser.consume_or_ignore(TokenType::Semicolon).is_none() {
        parser.consume_or_ff_after(TokenType::Comma, &[TokenType::Semicolon])?;
        constants.push(Node::Const {
            name: parser.consume_identifier()?,
            token: parser.consume(TokenType::Assignment)?,
            value: Box::new(expressions::expression(parser, 0)?),
        });
    }

    Ok(Node::ConstStatement { token, constants })
}

/// Parses all the arguments of a call
pub(crate) fn non_empty_lexical_variables_list(parser: &mut Parser) -> ExpressionListResult {
    parser.consume_or_ff_after(TokenType::OpenParenthesis, &[TokenType::Semicolon])?;

    let mut arguments = vec![lexical_variable(parser)?];

    parser.consume_or_ignore(TokenType::Comma);

    while !parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
        arguments.push(lexical_variable(parser)?);

        if parser.next_token_one_of(&[TokenType::CloseParenthesis]) {
            break;
        } else {
            parser.consume_or_ff_after(TokenType::Comma, &[TokenType::Semicolon])?;
        }
    }

    parser.consume_or_ff_after(TokenType::CloseParenthesis, &[TokenType::Semicolon])?;

    Ok(arguments)
}

/// Parses a lexical variable, used in a "use ()" list for example
pub(crate) fn lexical_variable(parser: &mut Parser) -> ExpressionResult {
    Ok(Node::LexicalVariable {
        reference: parser.consume_or_ignore(TokenType::BinaryAnd),
        variable: parser.consume(TokenType::Variable)?,
    })
}

#[cfg(test)]
mod test {
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    #[test]
    fn test_parses_aliased_variables() {
        let code_semicolon = "<?php $$$$$$a = 2;";

        let mut scanner = Scanner::new(code_semicolon);
        let tokens = scanner.scan().unwrap();
        let ast_result = Parser::ast(tokens.clone()).unwrap();

        assert!(ast_result.1.is_empty());
    }
}
