use crate::expression::Node;
use crate::parser::{expressions, functions, types};
use crate::parser::{ExpressionResult, Parser};
use crate::token::{Token, TokenType};

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

        if let Some(next) = parser.peek() {
            match next.t {
                TokenType::Function => {
                    parser.next();

                    let by_ref = parser.consume_or_ignore(TokenType::BinaryAnd);
                    let name = parser.consume_identifier()?;

                    statements.push(Node::MethodDefinitionStatement {
                        is_final,
                        by_ref,
                        name,
                        visibility,
                        is_abstract,
                        function: Box::new(functions::anonymous_function_statement(parser)?),
                        is_static,
                    });
                }
                // One or more of those ...
                TokenType::Variable => {
                    loop {
                        // The next variable
                        let name = parser.consume(TokenType::Variable)?;
                        let assignment = if parser.next_token_one_of(&[TokenType::Assignment]) {
                            parser.next();
                            Some(Box::new(expressions::expression(parser)?))
                        } else {
                            None
                        };

                        statements.push(Node::PropertyDefinitionStatement {
                            name,
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
                _ => {
                    return Err(format!(
                        "Unexpected {:?} on line {}, col {}",
                        next.t, next.line, next.col
                    ));
                }
            }
        } else {
            return Err(String::from("End of file"));
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
        type_refs: types::type_ref_list(parser)?,
    };

    parser.consume_end_of_statement()?;

    Ok(statement)
}
