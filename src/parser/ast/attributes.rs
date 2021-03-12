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
pub fn attribute(parser: &mut Parser) -> ExpressionResult {
    let ats = parser.consume(TokenType::AttributeStart)?;
    let expression = Box::new(expressions::expression(parser)?);
    let cb = parser.consume(TokenType::CloseBrackets)?;

    Ok(Node::Attribute {
        ats,
        expression,
        cb,
    })
}

#[cfg(test)]
mod tests {
    use crate::formatter::{format, FormatterOptions};
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    #[test]
    fn test_parses_attributes() {
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
}
