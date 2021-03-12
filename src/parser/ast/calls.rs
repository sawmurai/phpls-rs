use crate::parser;

use super::super::node::Node;
use super::super::token::TokenType;
use super::super::Error;
use super::super::{ExpressionResult, Parser};
use super::{expressions, variables};

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
            if let Some(oc) = parser.consume_or_ignore(TokenType::OpenCurly) {
                expr = Node::Member {
                    object: Box::new(expr),
                    arrow: os,
                    oc: Some(oc),
                    member: Box::new(expressions::expression(parser)?),
                    cc: Some(parser.consume(TokenType::CloseCurly)?),
                };

            // Using the ->member syntax, so the member is either an identifier or a variable
            } else if parser.next_token_one_of(&[TokenType::Variable]) {
                expr = Node::Member {
                    object: Box::new(expr),
                    arrow: os,
                    oc: None,
                    member: Box::new(variables::variable(parser)?),
                    cc: None,
                };
            } else if let Ok(identifier) = parser.consume_identifier() {
                expr = Node::Member {
                    object: Box::new(expr),
                    arrow: os,
                    oc: None,
                    member: Box::new(Node::Literal(identifier)),
                    cc: None,
                };
            } else {
                parser
                    .errors
                    .push(Error::MissingIdentifier { token: os.clone() });
                expr = Node::Member {
                    object: Box::new(expr),
                    arrow: os.clone(),
                    oc: None,
                    member: Box::new(Node::Missing(os)),
                    cc: None,
                };
            }
        // Accessing a static class member
        } else if let Some(pn) = parser.consume_or_ignore(TokenType::PaamayimNekudayim) {
            // Class property
            if parser.next_token_one_of(&[TokenType::Variable]) {
                expr = Node::StaticMember {
                    object: Box::new(expr),
                    pn,
                    oc: None,
                    member: Box::new(variables::variable(parser)?),
                    cc: None,
                };

            // Class method
            } else if parser.next_token_one_of(&[TokenType::OpenCurly]) {
                expr = Node::StaticMember {
                    object: Box::new(expr),
                    pn,
                    oc: Some(parser.consume(TokenType::OpenCurly)?),
                    member: Box::new(variables::variable(parser)?),
                    cc: Some(parser.consume(TokenType::CloseCurly)?),
                };
            // Class constant
            } else if let Ok(identifier) = parser.consume_identifier() {
                expr = Node::StaticMember {
                    object: Box::new(expr),
                    pn,
                    oc: None,
                    member: Box::new(Node::Literal(identifier)),
                    cc: None,
                };
            } else {
                parser
                    .errors
                    .push(Error::MissingIdentifier { token: pn.clone() });
                expr = Node::StaticMember {
                    object: Box::new(expr),
                    pn: pn.clone(),
                    oc: None,
                    member: Box::new(Node::Missing(pn)),
                    cc: None,
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

#[cfg(test)]
mod tests {
    use crate::formatter::{format, FormatterOptions};
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    #[test]
    fn test_parses_array_access() {
        let mut scanner = Scanner::new("<?php $var[2] = 1;");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$var[2] = 1;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_array_access_without_index() {
        let mut scanner = Scanner::new("<?php $var[] = 1;");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$var[] = 1;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_object_method_access() {
        let mut scanner = Scanner::new("<?php $object->method();");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$object->method();
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_object_property_access() {
        let mut scanner = Scanner::new("<?php $object->property;");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$object->property;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_static_property_access() {
        let mut scanner = Scanner::new("<?php Thing::property;");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
Thing::property;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_static_method_access() {
        let mut scanner = Scanner::new("<?php self::method();");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
self::method();
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_static_method_access_with_parameters() {
        let mut scanner = Scanner::new("<?php self::method(1,2,3);");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
self::method(1, 2, 3);
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_static_property_access_via_variable() {
        let mut scanner = Scanner::new("<?php self::$variable;");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
self::$variable;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_array_access_with_curly() {
        let mut scanner = Scanner::new("<?php $a{2} = 10;");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$a{2} = 10;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_object_property_access_with_curly() {
        let mut scanner = Scanner::new("<?php $object->{$dyn} = 10;");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$object->{$dyn} = 10;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_object_method_access_with_curly() {
        let mut scanner = Scanner::new("<?php $object->{$dyn}();");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$object->{$dyn}();
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_static_method_access_with_curly() {
        let mut scanner = Scanner::new("<?php $object::{$dyn}();");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$object::{$dyn}();
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_object_property_access_with_variable() {
        let mut scanner = Scanner::new("<?php $object->$dyn = 10;");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$object->$dyn = 10;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_incomplete_object_property_access() {
        let mut scanner = Scanner::new("<?php $object->");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(false, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$object-><Missing>;
"
        .to_owned();

        assert_eq!(expected, formatted);
    }
}
