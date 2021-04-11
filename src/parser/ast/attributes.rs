use crate::parser::ExpressionListResult;

use super::super::node::Node;
use super::super::token::TokenType;
use super::super::{expressions, ExpressionResult, Parser};

/// Parses attributes
///
/// # Example
/// ```php
/// #[Attr()]
/// class Rofl {
/// }
/// ```
fn attribute(parser: &mut Parser) -> ExpressionResult {
    let ats = parser.consume(TokenType::AttributeStart)?;

    let mut expressions = Vec::with_capacity(5);
    expressions.push(expressions::expression(parser, 0)?);
    while parser.consume_or_ignore(TokenType::Comma).is_some() {
        expressions.push(expressions::expression(parser, 0)?);
    }

    let cb = parser.consume(TokenType::CloseBrackets)?;

    Ok(Node::Attribute {
        ats,
        expressions,
        cb,
    })
}

pub fn attributes_block(parser: &mut Parser) -> ExpressionListResult {
    let mut attributes = Vec::new();
    while parser.next_token_one_of(&[TokenType::AttributeStart]) {
        attributes.push(attribute(parser)?);
    }

    Ok(attributes)
}

#[cfg(test)]
mod tests {
    use crate::formatter::{format, FormatterOptions};
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    #[test]
    fn test_parses_attributes_on_classes() {
        let mut scanner = Scanner::new(
            "<?php \
        #[Sample([1, 2, 3])]
        class Test {}",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
#[Sample([1, 2, 3])]
class Test {

}"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_without_arguments() {
        let mut scanner = Scanner::new(
            "<?php \
        #[Sample]
        class Test {}",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
#[Sample]
class Test {

}"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_on_methods() {
        let mut scanner = Scanner::new(
            "<?php \
        class Test { #[Sample([1, 2, 3])] public function rofl() {}}",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\

class Test {
    #[Sample([1, 2, 3])]
    public function rofl() {

    }
}"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_on_properties() {
        let mut scanner = Scanner::new(
            "<?php \
        class Test { #[Sample([1, 2, 3])] public $rofl;}",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\

class Test {
    #[Sample([1, 2, 3])]
    public $rofl;
}"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_on_constants() {
        let mut scanner = Scanner::new(
            "<?php \
        class Test { #[Sample([1, 2, 3])] public const ROFL = '';}",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\

class Test {
    #[Sample([1, 2, 3])]
    public const ROFL = '';
}"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_multiple_attributes() {
        let mut scanner = Scanner::new(
            "<?php \
        class Test { #[Sample([1, 2, 3])] #[Sample2([1, 2, 3]), Sample3([1, 2, 3])] public $rofl;}",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\

class Test {
    #[Sample([1, 2, 3])]
    #[Sample2([1, 2, 3]), Sample3([1, 2, 3])]
    public $rofl;
}"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_of_parameters() {
        let mut scanner = Scanner::new(
            "<?php \
        function test(#[Attr('lol')] string $theAttr) { }",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\

function test(#[Attr('lol')] string $theAttr) {

}"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_of_functions() {
        let mut scanner = Scanner::new(
            "<?php \
            #[Attr('lol')] 
            function test(string $theAttr) { }",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
#[Attr('lol')]
function test(string $theAttr) {

}"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_of_closures() {
        let mut scanner = Scanner::new(
            "<?php \
            
            $x = #[Attr('lol')] function (string $theAttr) { };",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$x = #[Attr('lol')] function (string $theAttr) {

};
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_of_arrow_function() {
        let mut scanner = Scanner::new(
            "<?php \
            
            $x = #[Attr('lol')] fn (string $theAttr) => 'lol';",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
$x = #[Attr('lol')] fn (string $theAttr) => 'lol';
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_attributes_of_anonymous_class() {
        let mut scanner = Scanner::new(
            "<?php \
            $x = new #[Attr('lol')] class extends Exception {};",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        eprintln!("{:?}", errors);
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format(&ast, 0, 0, &options);

        let expected = "\
        $x = new #[Attr('lol')] class extends Exception {

};
"
        .to_owned();

        assert_eq!(expected, formatted);
    }
}
