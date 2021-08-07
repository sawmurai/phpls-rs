use crate::parser::node::ClassStatement;

use super::super::token::{Token, TokenType};
use super::super::{ExpressionListResult, ExpressionResult, Parser};
use super::{super::node::Node, attributes};
use super::{comments, expressions, functions, types};

// abstract_class -> "abstract" class
pub(crate) fn abstract_class_statement(
    parser: &mut Parser,
    attributes: Vec<Node>,
) -> ExpressionResult {
    let is_abstract = parser.consume(TokenType::Abstract)?;

    class_statement(parser, Some(is_abstract), None, attributes)
}

// final_class -> "final" class
pub(crate) fn final_class_statement(
    parser: &mut Parser,
    attributes: Vec<Node>,
) -> ExpressionResult {
    let is_final = parser.consume(TokenType::Final)?;

    class_statement(parser, None, Some(is_final), attributes)
}

// class -> "class" identifier (extends identifier_list)? (implements identifier_list)?
pub(crate) fn class_statement(
    parser: &mut Parser,
    is_abstract: Option<Token>,
    is_final: Option<Token>,
    attributes: Vec<Node>,
) -> ExpressionResult {
    let doc_comment = comments::consume_optional_doc_comment(parser);
    let token = parser.consume(TokenType::Class)?;
    let name = parser.consume_identifier()?;

    let extends = match parser.consume_or_ignore(TokenType::Extends) {
        Some(_) => Some(Box::new(types::non_empty_type_ref(parser)?)),
        None => None,
    };

    let implements = match parser.consume_or_ignore(TokenType::Implements) {
        Some(_) => Some(identifier_list(parser)?),
        None => None,
    };

    Ok(Node::ClassStatement(ClassStatement {
        token,
        name,
        is_abstract,
        is_final,
        extends,
        implements,
        body: Box::new(class_block(parser)?),
        doc_comment,
        attributes,
    }))
}

pub(crate) fn anonymous_class(parser: &mut Parser, attributes: Vec<Node>) -> ExpressionResult {
    let token = parser.consume(TokenType::Class)?;

    let arguments = if parser.next_token_one_of(&[TokenType::OpenParenthesis]) {
        parser.consume_or_ff_after(TokenType::OpenParenthesis, &[TokenType::OpenParenthesis])?;
        let result = Some(functions::parameter_list(parser)?);
        parser.consume_or_ff_after(TokenType::CloseParenthesis, &[TokenType::OpenCurly])?;

        result
    } else {
        None
    };

    let extends = match parser.consume_or_ignore(TokenType::Extends) {
        Some(_) => Some(Box::new(types::non_empty_type_ref(parser)?)),
        None => None,
    };

    let implements = match parser.consume_or_ignore(TokenType::Implements) {
        Some(_) => Some(identifier_list(parser)?),
        None => None,
    };

    Ok(Node::Class {
        token,
        arguments,
        extends,
        implements,
        body: Box::new(class_block(parser)?),
        attributes,
    })
}

pub(crate) fn class_block_statement(parser: &mut Parser) -> ExpressionResult {
    if parser.next_token_one_of(&[TokenType::Use]) {
        return use_trait_statement(parser);
    }

    let doc_comment = comments::consume_optional_doc_comment(parser);

    let attributes = attributes::attributes_block(parser)?;

    let doc_comment = if doc_comment.is_some() {
        doc_comment
    } else {
        comments::consume_optional_doc_comment(parser)
    };

    let mut is_abstract = None;
    let mut is_final = None;
    let mut visibility = None;
    let mut is_static = None;

    // Collect all modifiers
    while parser.next_token_one_of(&[
        TokenType::Abstract,
        TokenType::Final,
        TokenType::Public,
        TokenType::Var,
        TokenType::Private,
        TokenType::Protected,
        TokenType::Static,
    ]) {
        is_abstract = parser.consume_or_ignore(TokenType::Abstract);
        is_final = parser.consume_or_ignore(TokenType::Final);
        visibility = parser.consume_one_of_or_ignore(&[
            TokenType::Public,
            TokenType::Var,
            TokenType::Private,
            TokenType::Protected,
        ]);
        is_static = parser.consume_or_ignore(TokenType::Static);
    }

    if let Some(token) = parser.consume_or_ignore(TokenType::Const) {
        let mut consts = Vec::new();
        loop {
            let name = parser.consume_identifier()?;

            parser.consume_or_ff_after(TokenType::Assignment, &[TokenType::Semicolon])?;

            let value = Box::new(expressions::expression(parser, 0)?);
            consts.push(Node::ClassConstant {
                name,
                value,
                visibility: visibility.clone(),
            });

            if parser.consume_or_ignore(TokenType::Comma).is_some() {
                continue;
            }

            break;
        }

        let statement = Node::ClassConstantDefinitionStatement {
            visibility,
            token,
            consts,
            doc_comment,
            attributes,
        };

        parser.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;

        return Ok(statement);
    };

    if let Some(token) = parser.consume_or_ignore(TokenType::Function) {
        let by_ref = parser.consume_or_ignore(TokenType::BinaryAnd);
        let name = parser.consume_identifier()?;

        return Ok(Node::MethodDefinitionStatement {
            token,
            is_final,
            by_ref,
            name,
            visibility,
            is_abstract,
            function: Box::new(functions::anonymous_function_statement(
                parser,
                &doc_comment,
            )?),
            is_static,
            doc_comment,
            attributes,
        });
    }

    let data_type = if !parser.next_token_one_of(&[TokenType::Variable]) {
        // TODO: Handle nullable
        Some(Box::new(types::data_type(parser)?))
    } else {
        None
    };

    let mut props = Vec::new();
    loop {
        let name = parser.consume(TokenType::Variable)?;

        // The next variable
        let assignment = if parser.next_token_one_of(&[TokenType::Assignment]) {
            parser.next();
            Some(Box::new(expressions::expression(parser, 0)?))
        } else {
            None
        };

        props.push(Node::Property {
            name,
            value: assignment,
        });

        if !parser.next_token_one_of(&[TokenType::Comma]) {
            break;
        }

        // The comma
        parser.next();
    }

    parser.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;

    Ok(Node::PropertyDefinitionStatement {
        properties: props,
        data_type,
        visibility,
        is_abstract,
        is_static,
        doc_comment,
        attributes,
    })
}

/// Parses a class block, so basically the body that contains all the method definitions etc.
/// It expects to be past the `{` and will read until it encounters a `}`
///
/// # Details
/// ```php
/// abstract class Whatever {
/// // Parse here
/// }
/// ```
pub(crate) fn class_block(parser: &mut Parser) -> ExpressionResult {
    let oc = parser.consume(TokenType::OpenCurly)?;
    let mut statements = Vec::new();

    while !parser.next_token_one_of(&[TokenType::CloseCurly]) {
        if let Ok(statement) = class_block_statement(parser) {
            statements.push(statement);
        }

        if parser.peek().is_none() {
            break;
        }
    }

    let cc = parser.consume(TokenType::CloseCurly)?;

    Ok(Node::Block { oc, statements, cc })
}

/// Parses an interface definition
pub(crate) fn interface(parser: &mut Parser) -> ExpressionResult {
    let doc_comment = comments::consume_optional_doc_comment(parser);
    let token = parser.consume(TokenType::Interface)?;
    let name = parser.consume(TokenType::Identifier)?;

    let extends = match parser.consume_or_ignore(TokenType::Extends) {
        Some(_) => Some(identifier_list(parser)?),
        None => None,
    };

    let body = class_block(parser)?;

    Ok(Node::Interface {
        token,
        name,
        extends,
        body: Box::new(body),
        doc_comment,
    })
}

// (("implements" identifier) (, "implements" identifier)*)?
fn identifier_list(parser: &mut Parser) -> ExpressionListResult {
    let mut extends = Vec::new();

    loop {
        extends.push(types::non_empty_type_ref(parser)?);

        if !parser.next_token_one_of(&[TokenType::Comma]) {
            break;
        }

        parser.next();
    }
    Ok(extends)
}

/// Parses a trait
pub(crate) fn trait_statement(parser: &mut Parser) -> ExpressionResult {
    let doc_comment = comments::consume_optional_doc_comment(parser);
    Ok(Node::TraitStatement {
        token: parser.consume(TokenType::Trait)?,
        name: parser.consume(TokenType::Identifier)?,
        body: Box::new(class_block(parser)?),
        doc_comment,
    })
}

// use -> "use" identifier (, identifier)*
pub(crate) fn use_trait_statement(parser: &mut Parser) -> ExpressionResult {
    let statement = Node::UseTraitStatement {
        token: parser.consume(TokenType::Use)?,
        traits_usages: trait_usages(parser)?,
    };

    Ok(statement)
}

/// Parses a list of trait usages, which can either be a simple identifier or a more complicated block
fn trait_usages(parser: &mut Parser) -> ExpressionListResult {
    let mut usages = Vec::new();
    let mut type_refs = Vec::new();

    while !parser.next_token_one_of(&[TokenType::Semicolon]) {
        let type_ref = types::non_empty_type_ref(parser)?;
        type_refs.push(type_ref);

        if parser.next_token_one_of(&[TokenType::OpenCurly]) {
            usages.push(trait_usage_alteration_group(parser, type_refs)?);

            // Early return as there will be no semicolon after the use statment to be consumed.
            return Ok(usages);
        }

        if parser.consume_or_ignore(TokenType::Comma).is_none() {
            break;
        }
    }

    for type_ref in type_refs.drain(..) {
        usages.push(Node::UseTrait {
            type_ref: Box::new(type_ref),
        });
    }

    parser.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;

    Ok(usages)
}

/// Parses a trait usage alteration group
fn trait_usage_alteration_group(
    parser: &mut Parser,
    alteration_group_type_refs: Vec<Node>,
) -> ExpressionResult {
    let oc = parser.consume(TokenType::OpenCurly)?;
    let mut alterations = Vec::new();

    while !parser.next_token_one_of(&[TokenType::CloseCurly]) {
        let class_or_member = Box::new(types::non_empty_type_ref(parser)?);

        let (class_name, paa, member) =
            if let Some(paa) = parser.consume_or_ignore(TokenType::PaamayimNekudayim) {
                (
                    Some(class_or_member),
                    Some(paa),
                    Box::new(types::non_empty_type_ref(parser)?),
                )
            } else {
                (None, None, class_or_member)
            };

        if let Some(as_token) = parser.consume_or_ignore(TokenType::As) {
            let visibility = parser.consume_one_of_or_ignore(&[
                TokenType::Private,
                TokenType::Public,
                TokenType::Protected,
            ]);

            if !parser.next_token_one_of(&[TokenType::Semicolon]) {
                alterations.push(Node::UseTraitAs {
                    left: class_name,
                    paa,
                    member,
                    as_token,
                    visibility,
                    as_name: Some(parser.consume_identifier()?),
                });
            } else {
                alterations.push(Node::UseTraitAs {
                    left: class_name,
                    paa,
                    member,
                    as_token,
                    visibility,
                    as_name: None,
                });
            }
        } else if let Some(insteadof) = parser.consume_or_ignore(TokenType::Insteadof) {
            alterations.push(Node::UseTraitInsteadOf {
                left: class_name,
                paa,
                member,
                insteadof,
                insteadof_list: types::type_ref_list(parser)?,
            });
        }

        parser.consume_or_ff_after(TokenType::Semicolon, &[TokenType::CloseCurly])?;
    }

    Ok(Node::UseTraitAlterationBlock {
        alteration_group_type_refs,
        oc,
        alterations,
        cc: parser.consume(TokenType::CloseCurly)?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::formatter::{format_file, FormatterOptions};
    use crate::parser::{
        scanner::Scanner,
        token::{Token, TokenType},
    };
    use crate::parser::{Context, Parser};

    #[test]
    fn test_parses_trait_that_uses_trait() {
        let mut tokens = vec![
            Token::new(TokenType::Trait, 1, 1, 0),
            Token::named(TokenType::Identifier, 2, 1, 0, "TestControllerTrait"),
            Token::new(TokenType::OpenCurly, 3, 1, 0),
            Token::new(TokenType::Use, 4, 1, 0),
            Token::named(TokenType::Identifier, 5, 1, 0, "ControllerTrait"),
            Token::new(TokenType::OpenCurly, 6, 1, 0),
            Token::named(TokenType::Identifier, 7, 1, 0, "generateUrl"),
            Token::new(TokenType::As, 8, 1, 0),
            Token::new(TokenType::Public, 9, 1, 0),
            Token::new(TokenType::Semicolon, 10, 1, 0),
            Token::named(TokenType::Identifier, 11, 1, 0, "redirect"),
            Token::new(TokenType::As, 12, 1, 0),
            Token::named(TokenType::Identifier, 13, 1, 0, "roflcopter"),
            Token::new(TokenType::Semicolon, 14, 1, 0),
            Token::new(TokenType::CloseCurly, 15, 1, 0),
            Token::new(TokenType::CloseCurly, 16, 1, 0),
        ];
        tokens.reverse();

        let mut parser = Parser {
            doc_comments: Vec::new(),
            errors: Vec::new(),
            tokens,
            context: Context::Out,
            eof: (16, 1),
            end_of_prev_token: (0, 0),
        };

        trait_statement(&mut parser).unwrap();
    }

    #[test]
    fn test_parses_class_statement_with_method() {
        let mut scanner = Scanner::new("<?php class Test { public static function test() {} }");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
class Test
{
    public static function test()
    {
    }
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_class_statement_with_const() {
        let mut scanner = Scanner::new("<?php class Test { public const ROFL = 'test'; }");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
class Test
{
    public const ROFL = 'test';
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_class_statement_with_multi_const() {
        let mut scanner =
            Scanner::new("<?php class Test { public const ROFL = 'test', COPTER = 2; }");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
class Test
{
    public const ROFL = 'test',
                 COPTER = 2;
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_class_statement_with_property() {
        let mut scanner = Scanner::new("<?php class Test { public $rofl = 1; }");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
class Test
{
    public $rofl = 1;
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_abstract_class_statement() {
        let mut scanner =
            Scanner::new("<?php abstract class Test { public static function test() {} }");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
abstract class Test
{
    public static function test()
    {
    }
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_final_class_statement() {
        let mut scanner =
            Scanner::new("<?php final class Test { public static function test() {} }");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
final class Test
{
    public static function test()
    {
    }
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_class_that_extends_and_implements() {
        let mut scanner = Scanner::new("<?php class Test extends ParentC implements Treatable { }");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
class Test extends ParentC implements Treatable
{
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_class_with_traits() {
        let original = "<?php
class Aliased_Talker
{
    use Some;
    use A, B {
        B::smallTalk insteadof A;
        A::bigTalk insteadof B;
        B::bigTalk as talk;
        B::test as private;
        unique as protected stillUnique;
    }
}
";
        let mut scanner = Scanner::new(original);
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();

        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);
        let expected = "\
class Aliased_Talker
{
    use Some;
    use A, B {
        B::smallTalk insteadof A;
        A::bigTalk insteadof B;
        B::bigTalk as talk;
        B::test as private;
        unique as protected stillUnique;
    }
}
";
        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_anonymous_class() {
        let mut scanner =
            Scanner::new("<?php $o = new class { public static function test() {} };");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
$o = new class {
    public static function test()
    {
    }
};
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_anonymous_class_that_extends_and_implements() {
        let mut scanner =
            Scanner::new("<?php $o = new class extends ParentC implements Treatable { public static function test() {} };");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
$o = new class extends ParentC implements Treatable {
    public static function test()
    {
    }
};
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_anonymous_class_with_arguments() {
        let mut scanner =
            Scanner::new("<?php $o = new class($variable) { public static function test() {} };");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
$o = new class($variable) {
    public static function test()
    {
    }
};
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_interface() {
        let mut scanner = Scanner::new("<?php interface Treatable { public function callMe(); }");
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
interface Treatable 
{
    public function callMe();
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }

    #[test]
    fn test_parses_interface_that_extends() {
        let mut scanner = Scanner::new(
            "<?php interface Treatable extends OtherInterface { public function callMe(); }",
        );
        scanner.scan().unwrap();

        let (ast, errors) = Parser::ast(scanner.tokens).unwrap();
        assert_eq!(true, errors.is_empty());

        let options = FormatterOptions {
            max_line_length: 100,
            indent: 4,
        };

        let formatted = format_file(&ast, 0, 0, &options);

        let expected = "\
interface Treatable extends OtherInterface 
{
    public function callMe();
}
"
        .to_owned();

        assert_eq!(expected, formatted);
    }
}
