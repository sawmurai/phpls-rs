use super::super::token::TokenType;
use super::super::{Error, ExpressionResult, Parser};
use super::{super::node::Node, attributes::attributes_block};
use super::{arrays, calls, classes, conditionals, functions, keywords, types, variables};

pub(crate) fn expression_statement(parser: &mut Parser) -> ExpressionResult {
    let value = expression(parser, 0)?;

    Ok(Node::ExpressionStatement {
        expression: Box::new(value),
    })
}

/// Parses an expression. This can be anything that evaluates to a value. A function call, a comparison or even an assignment

/// Pratt parser which is heavily inspired by this awesome
/// blogpost: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
pub(crate) fn expression(parser: &mut Parser, min_bp: u8) -> ExpressionResult {
    // Start with the first left hand side, which can either be an operator
    // or an operand
    let mut lhs = if let Some(token) = parser.peek() {
        if let Some(rb) = token.t.prefix_binding_power() {
            let token = parser.next().unwrap();
            let rhs = expression(parser, rb)?;
            Node::Unary {
                token,
                expr: Box::new(rhs),
            }
        // Also no parenthesis, so its an operand
        } else {
            calls::call(parser)?
        }
    } else {
        return Err(Error::Eof);
    };

    while parser.next_is_operator() {
        let op = parser.peek().unwrap();

        // is the next one a postfix operator?
        if let Some(lb) = op.t.postfix_binding_power() {
            if lb < min_bp {
                break;
            }

            let token = parser.next().unwrap();

            lhs = Node::PostUnary {
                token,
                expr: Box::new(lhs),
            };

            continue;
        }

        // If not, is it an infix operator?
        if let Some((lb, rb)) = op.t.infix_binding_power() {
            if lb < min_bp {
                break;
            }

            let op = parser.next().unwrap();

            lhs = if op.t == TokenType::QuestionMark {
                if let Some(colon) = parser.consume_or_ignore(TokenType::Colon) {
                    let rhs = expression(parser, rb)?;
                    Node::Ternary {
                        check: Box::new(lhs),
                        true_arm: None,
                        colon,
                        qm: op,
                        false_arm: Box::new(rhs),
                    }
                } else {
                    let mhs = expression(parser, 0)?;
                    let colon = parser.consume(TokenType::Colon)?;
                    let rhs = expression(parser, rb)?;
                    Node::Ternary {
                        check: Box::new(lhs),
                        true_arm: Some(Box::new(mhs)),
                        colon,
                        qm: op,
                        false_arm: Box::new(rhs),
                    }
                }
            } else {
                let rhs = expression(parser, rb)?;
                Node::Binary {
                    token: op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                }
            };

            continue;
        }

        // If its neither a postfix nor an infix operator we are done.
        break;
    }

    Ok(lhs)
}

pub(crate) fn primary(parser: &mut Parser) -> ExpressionResult {
    if parser.next_token_one_of(&[
        TokenType::False,
        TokenType::True,
        TokenType::Null,
        TokenType::LongNumber,
        TokenType::DecimalNumber,
        TokenType::ExponentialNumber,
        TokenType::HexNumber,
        TokenType::BinaryNumber,
        TokenType::ConstantEncapsedString,
        TokenType::EncapsedAndWhitespaceString,
        TokenType::ShellEscape,
        TokenType::ConstDir,
        TokenType::ConstFile,
        TokenType::ConstFunction,
        TokenType::ConstLine,
        TokenType::ConstMethod,
        TokenType::ConstTrait,
        TokenType::ConstClass,
        TokenType::ConstNan,
        TokenType::ConstInf,
    ]) {
        return Ok(Node::Literal(parser.next().unwrap()));
    }

    if parser.next_token_one_of(&[TokenType::Variable]) {
        return variables::variable(parser);
    }

    if parser.next_token_one_of(&[TokenType::NamespaceSeparator, TokenType::Identifier]) {
        // Load path as identifier
        return types::non_empty_type_ref(parser);
    }

    if parser.next_token_one_of(&[TokenType::HereDocStart]) {
        parser.consume_or_ff_after(TokenType::HereDocStart, &[TokenType::HereDocStart])?;
        let string = parser.next().unwrap();
        parser.consume_or_ff_after(TokenType::HereDocEnd, &[TokenType::HereDocEnd])?;

        return Ok(Node::Literal(string));
    }

    if let Some(isset) = parser.consume_or_ignore(TokenType::Isset) {
        return Ok(Node::Isset {
            isset,
            op: parser.consume(TokenType::OpenParenthesis)?,
            parameters: functions::non_empty_parameter_list(parser)?,
            cp: parser.consume(TokenType::CloseParenthesis)?,
        });
    }

    if let Some(exit) = parser.consume_or_ignore(TokenType::Exit) {
        if let Some(op) = parser.consume_or_ignore(TokenType::OpenParenthesis) {
            return Ok(Node::Exit {
                exit,
                op: Some(op),
                parameters: Some(functions::parameter_list(parser)?),
                cp: Some(parser.consume(TokenType::CloseParenthesis)?),
            });
        }

        return Ok(Node::Exit {
            exit,
            op: None,
            parameters: None,
            cp: None,
        });
    }

    if let Some(empty) = parser.consume_or_ignore(TokenType::Empty) {
        return Ok(Node::Empty {
            empty,
            op: parser.consume(TokenType::OpenParenthesis)?,
            parameters: functions::non_empty_parameter_list(parser)?,
            cp: parser.consume(TokenType::CloseParenthesis)?,
        });
    }

    if parser.next_token_one_of(&[TokenType::TypeArray]) {
        return arrays::old_array(parser);
    }

    if parser.next_token_one_of(&[TokenType::List]) {
        return keywords::list(parser);
    }

    if let Some(include) = parser.consume_one_of_or_ignore(&[
        TokenType::Require,
        TokenType::RequireOnce,
        TokenType::Include,
        TokenType::IncludeOnce,
    ]) {
        return Ok(Node::FileInclude {
            token: include,
            resource: Box::new(expression(parser, 0)?),
        });
    }

    if parser.next_token_one_of(&[TokenType::OpenBrackets]) {
        let expr = arrays::array(parser)?;

        return Ok(expr);
    }

    if parser.next_token_one_of(&[TokenType::OpenParenthesis]) {
        parser.consume_or_ff_after(TokenType::OpenParenthesis, &[TokenType::OpenParenthesis])?;
        let expr = expression(parser, 0)?;
        parser.consume_or_ff_after(TokenType::CloseParenthesis, &[TokenType::Semicolon])?;

        return Ok(Node::Grouping(Box::new(expr)));
    }

    let attributes = attributes_block(parser)?;

    if parser.next_token_one_of(&[TokenType::Fn]) {
        return functions::arrow_function(parser, None, attributes);
    }

    if parser.next_token_one_of(&[TokenType::Function]) {
        return functions::anonymous_function(parser, None, attributes);
    }

    // Static is fun ... watch this ...
    if let Some(static_token) = parser.consume_or_ignore(TokenType::Static) {
        // Followed by ::? Probably a member access
        if parser.next_token_one_of(&[TokenType::PaamayimNekudayim]) {
            return Ok(Node::Literal(static_token));
        }

        // Followed by "function"? Static function expression
        if parser.next_token_one_of(&[TokenType::Function]) {
            return functions::anonymous_function(parser, Some(static_token), attributes);
        }

        if parser.next_token_one_of(&[TokenType::Fn]) {
            return functions::arrow_function(parser, Some(static_token), attributes);
        }

        // Otherwise probably used in a instantiation context
        return Ok(Node::Literal(static_token));
    }

    // self is like static but less mighty
    if let Some(parser_token) =
        parser.consume_one_of_or_ignore(&[TokenType::TypeSelf, TokenType::Parent])
    {
        // Followed by ::? Probably a member access
        if parser.next_token_one_of(&[TokenType::PaamayimNekudayim]) {
            return Ok(Node::Literal(parser_token));
        }

        // Otherwise ... no clue if an error after all. Need to check official grammar
        return Ok(Node::Literal(parser_token));
    }

    if parser.next_token_one_of(&[TokenType::Class]) {
        return classes::anonymous_class(parser, attributes);
    }

    if let Some(new) = parser.consume_or_ignore(TokenType::New) {
        return Ok(Node::New {
            token: new,
            class: Box::new(calls::call(parser)?),
        });
    }

    if let Some(token) = parser.consume_or_ignore(TokenType::Yield) {
        if parser.next_token_one_of(&[TokenType::Semicolon]) {
            return Ok(Node::Yield { token, expr: None });
        }

        return Ok(Node::Yield {
            token,
            expr: Some(Box::new(arrays::array_pair(parser)?)),
        });
    }

    if let Some(from) = parser.consume_or_ignore(TokenType::YieldFrom) {
        return Ok(Node::YieldFrom {
            token: from,
            expr: Box::new(expression(parser, 0)?),
        });
    }

    if let Some(mtch) = parser.consume_or_ignore(TokenType::Match) {
        return Ok(Node::Match {
            mtch,
            op: parser.consume(TokenType::OpenParenthesis)?,
            condition: Box::new(expression(parser, 0)?),
            cp: parser.consume(TokenType::CloseParenthesis)?,
            oc: parser.consume(TokenType::OpenCurly)?,
            body: conditionals::match_body(parser)?,
            cc: parser.consume(TokenType::CloseCurly)?,
        });
    }

    if let Some(next) = parser.next() {
        // Maybe some sort of other identifier?
        if next.is_identifier() {
            return Ok(Node::Literal(next));
        } else {
            return Err(Error::UnexpectedTokenError { token: next });
        }
    }

    Err(Error::Eof {})
}

#[cfg(test)]
mod tests {
    use super::expression;
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    #[test]
    fn test_parses_basic_math() {
        let code = "<?php 1 + 2 +3";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan().unwrap();
        let mut parser = Parser::new(tokens.iter().skip(1).map(|t| t.clone()).collect::<Vec<_>>());

        dbg!(expression(&mut parser, 0).unwrap());
    }

    #[test]
    fn test_parses_grouping() {
        let code = "<?php (1 + 2) * 3";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan().unwrap();
        let mut parser = Parser::new(tokens.iter().skip(1).map(|t| t.clone()).collect::<Vec<_>>());

        dbg!(expression(&mut parser, 0).unwrap());
    }

    #[test]
    fn test_parses_assignment_chains() {
        let code = "<?php 1 = 2 = 3 = 4 = 5 = 6 = 7 = 8 = 9 = 10 = 11 = 12 = 13 = 14";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan().unwrap();
        let mut parser = Parser::new(tokens.iter().skip(1).map(|t| t.clone()).collect::<Vec<_>>());

        dbg!(expression(&mut parser, 0).unwrap());
    }

    #[test]
    fn test_parses_the_ternary() {
        let code = "<?php true ? 'lol' : 'notsolol' ";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan().unwrap();
        let mut parser = Parser::new(tokens.iter().skip(1).map(|t| t.clone()).collect::<Vec<_>>());

        dbg!(expression(&mut parser, 0).unwrap());
    }

    #[test]
    fn test_parses_match_expression() {
        let code = "<?php match ($a) { 1, 2 => 3, 4, 5 => callme(), caller() => called() };";
        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan().unwrap();
        let mut parser = Parser::new(tokens.iter().skip(1).map(|t| t.clone()).collect::<Vec<_>>());

        dbg!(expression(&mut parser, 0).unwrap());
    }
}
