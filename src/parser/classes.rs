use crate::expression::Node;
use crate::parser::{expressions, functions, types};
use crate::parser::{ExpressionListResult, ExpressionResult, Parser};
use crate::token::{Token, TokenType};
use std::rc::Rc;

// abstract_class -> "abstract" class
pub(crate) fn abstract_class_statement(parser: &mut Parser) -> ExpressionResult {
    let is_abstract = parser.consume(TokenType::Abstract)?;

    class_statement(parser, Some(is_abstract), None)
}

// final_class -> "final" class
pub(crate) fn final_class_statement(parser: &mut Parser) -> ExpressionResult {
    let is_final = parser.consume(TokenType::Final)?;

    class_statement(parser, None, Some(is_final))
}

// class -> "class" identifier (extends identifier_list)? (implements identifier_list)?
pub(crate) fn class_statement(
    parser: &mut Parser,
    is_abstract: Option<Token>,
    is_final: Option<Token>,
) -> ExpressionResult {
    let token = parser.consume(TokenType::Class)?;
    let name = parser.consume_identifier()?;

    let extends = match parser.consume_or_ignore(TokenType::Extends) {
        Some(_) => Some(identifier_list(parser)?),
        None => None,
    };

    let implements = match parser.consume_or_ignore(TokenType::Implements) {
        Some(_) => Some(identifier_list(parser)?),
        None => None,
    };

    Ok(Node::ClassStatement {
        token,
        name,
        is_abstract,
        is_final,
        extends,
        implements,
        body: Box::new(class_block(parser)?),
    })
}

pub(crate) fn anonymous_class(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Class)?;

    let arguments = if parser.next_token_one_of(&[TokenType::OpenParenthesis]) {
        parser.consume_or_err(TokenType::OpenParenthesis)?;
        let result = Some(functions::parameter_list(parser)?);
        parser.consume_or_err(TokenType::CloseParenthesis)?;

        result
    } else {
        None
    };

    let extends = match parser.consume_or_ignore(TokenType::Extends) {
        Some(_) => Some(identifier_list(parser)?),
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
        if parser.next_token_one_of(&[TokenType::Use]) {
            statements.push(use_trait_statement(parser)?);

            continue;
        }

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

        if parser.next_token_one_of(&[TokenType::Const]) {
            parser.next();
            let name = parser.consume_identifier()?;

            parser.consume_or_err(TokenType::Assignment)?;
            statements.push(Node::ClassConstantDefinitionStatement {
                name,
                visibility,
                value: Box::new(expressions::expression(parser)?),
            });

            parser.consume_end_of_statement()?;

            continue;
        };

        if let Some(token) = parser.consume_or_ignore(TokenType::Function) {
            let by_ref = parser.consume_or_ignore(TokenType::BinaryAnd);
            let name = parser.consume_identifier()?;

            statements.push(Node::MethodDefinitionStatement {
                token,
                is_final,
                by_ref,
                name,
                visibility,
                is_abstract,
                function: Box::new(functions::anonymous_function_statement(parser)?),
                is_static,
            });
        } else {
            let data_type = if !parser.next_token_one_of(&[TokenType::Variable]) {
                Some(Rc::new(types::non_empty_type_ref(parser)?))
            } else {
                None
            };

            loop {
                let name = parser.consume(TokenType::Variable)?;

                // The next variable
                let assignment = if parser.next_token_one_of(&[TokenType::Assignment]) {
                    parser.next();
                    Some(Box::new(expressions::expression(parser)?))
                } else {
                    None
                };

                statements.push(Node::PropertyDefinitionStatement {
                    name,
                    data_type: data_type.clone(),
                    visibility: visibility.clone(),
                    is_abstract: is_abstract.clone(),
                    value: assignment,
                    is_static: is_static.clone(),
                });

                if !parser.next_token_one_of(&[TokenType::Comma]) {
                    break;
                }

                // The comma
                parser.next();
            }

            parser.consume_end_of_statement()?;
        }
    }

    let cc = parser.consume(TokenType::CloseCurly)?;

    Ok(Node::Block { oc, statements, cc })
}

/// Parses an interface definition
pub(crate) fn interface(parser: &mut Parser) -> ExpressionResult {
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
    })
}

// (("extends" identifier) (, "extends" identifier)*)?
// (("implements" identifier) (, "implements" identifier)*)?
fn identifier_list(parser: &mut Parser) -> Result<Vec<Node>, String> {
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
    Ok(Node::TraitStatement {
        token: parser.consume(TokenType::Trait)?,
        name: parser.consume(TokenType::Identifier)?,
        body: Box::new(class_block(parser)?),
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

    while !parser.next_token_one_of(&[TokenType::Semicolon]) {
        let type_ref = types::non_empty_type_ref(parser)?;

        if parser.next_token_one_of(&[TokenType::OpenCurly]) {
            usages.push(trait_usage_alteration_group(parser)?);

            // Early return as there will be no semicolon after the use statment to be consumed.
            return Ok(usages);
        } else {
            usages.push(Node::UseTrait {
                type_ref: Box::new(type_ref),
            });
        }

        if parser.consume_or_ignore(TokenType::Comma).is_some() {
            continue;
        }
    }

    parser.consume_end_of_statement()?;

    Ok(usages)
}

/// Parses a trait usage alteration group
fn trait_usage_alteration_group(parser: &mut Parser) -> ExpressionResult {
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
            alterations.push(Node::UseTraitAs {
                left: class_name,
                paa,
                member,
                as_token,
                as_name: parser.consume_identifier()?,
            });
        } else if let Some(insteadof) = parser.consume_or_ignore(TokenType::Insteadof) {
            alterations.push(Node::UseTraitInsteadOf {
                left: class_name,
                paa,
                member,
                insteadof,
                insteadof_list: types::type_ref_list(parser)?,
            });
        }

        parser.consume_or_err(TokenType::Semicolon)?;
    }

    Ok(Node::UseTraitAlterationBlock {
        oc,
        alterations,
        cc: parser.consume(TokenType::CloseCurly)?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_parses_trait_that_uses_trait() {
        let mut tokens = vec![
            Token::new(TokenType::Trait, 1, 1),
            Token::named(TokenType::Identifier, 2, 1, "TestControllerTrait"),
            Token::new(TokenType::OpenCurly, 3, 1),
            Token::new(TokenType::Use, 4, 1),
            Token::named(TokenType::Identifier, 5, 1, "ControllerTrait"),
            Token::new(TokenType::OpenCurly, 6, 1),
            Token::named(TokenType::Identifier, 7, 1, "generateUrl"),
            Token::new(TokenType::As, 8, 1),
            Token::new(TokenType::Public, 9, 1),
            Token::new(TokenType::Semicolon, 10, 1),
            Token::named(TokenType::Identifier, 11, 1, "redirect"),
            Token::new(TokenType::As, 12, 1),
            Token::named(TokenType::Identifier, 13, 1, "roflcopter"),
            Token::new(TokenType::Semicolon, 14, 1),
            Token::new(TokenType::CloseCurly, 15, 1),
            Token::new(TokenType::CloseCurly, 16, 1),
        ];
        tokens.reverse();

        let mut parser = Parser {
            errors: Vec::new(),
            tokens,
        };

        // TODO: Find a way to compare the return value that is not creating the AST from scratch
        // since nobody can read that
        trait_statement(&mut parser).unwrap();
    }
}
