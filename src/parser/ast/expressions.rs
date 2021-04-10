use super::super::token::TokenType;
use super::super::{Error, ExpressionResult, Parser};
use super::{super::node::Node, attributes::attributes_block};
use super::{arrays, calls, classes, functions, keywords, types, variables};

pub(crate) fn expression_statement(parser: &mut Parser) -> ExpressionResult {
    let value = expression(parser)?;

    Ok(Node::ExpressionStatement {
        expression: Box::new(value),
    })
}

/// Parses an expression. This can be anything that evaluates to a value. A function call, a comparison or even an assignment
pub(crate) fn expression(parser: &mut Parser) -> ExpressionResult {
    let expr = logic(parser);

    if let Some(qm) = parser.consume_or_ignore(TokenType::QuestionMark) {
        if let Some(colon) = parser.consume_or_ignore(TokenType::Colon) {
            return Ok(Node::Ternary {
                check: Box::new(expr?),
                qm,
                true_arm: None,
                colon,
                false_arm: Box::new(expression(parser)?),
            });
        } else {
            return Ok(Node::Ternary {
                check: Box::new(expr?),
                qm,
                true_arm: Some(Box::new(expression(parser)?)),
                colon: parser.consume(TokenType::Colon)?,
                false_arm: Box::new(expression(parser)?),
            });
        }
    }

    expr
}

pub(crate) fn logic(parser: &mut Parser) -> ExpressionResult {
    let mut expr = equality(parser)?;

    let potential_matches = vec![TokenType::LogicOr, TokenType::LogicAnd, TokenType::LogicXor];
    // Todo: Make this an if and force && or || in between. Basically add a new non-terminal
    while parser.next_token_one_of(&potential_matches) {
        // Unwrap should be fine since the condition already checks that there is a next element
        let next = parser.next().unwrap();
        let right = equality(parser)?;

        expr = Node::Binary {
            left: Box::new(expr),
            token: next,
            right: Box::new(right),
        };
    }

    Ok(expr)
}

pub(crate) fn equality(parser: &mut Parser) -> ExpressionResult {
    let mut expr = comparison(parser)?;

    let potential_matches = vec![
        TokenType::IsNotEqual,
        TokenType::IsNotEqualAlt,
        TokenType::IsEqual,
        TokenType::IsNotIdentical,
        TokenType::IsIdentical,
        TokenType::Coalesce,
        TokenType::InstanceOf,
    ];
    // Todo: Make this an if and force && or || in between. Basically add a new non-terminal
    while parser.next_token_one_of(&potential_matches) {
        // Unwrap should be fine since the condition already checks that there is a next element
        let next = parser.next().unwrap();
        let right = comparison(parser)?;

        expr = Node::Binary {
            left: Box::new(expr),
            token: next,
            right: Box::new(right),
        };
    }

    Ok(expr)
}

pub(crate) fn comparison(parser: &mut Parser) -> ExpressionResult {
    let mut expr = assignment(parser)?;

    let potential_matches = vec![
        TokenType::Greater,
        TokenType::GreaterOrEqual,
        TokenType::Smaller,
        TokenType::SmallerOrEqual,
        TokenType::SpaceShip,
    ];

    while parser.next_token_one_of(&potential_matches) {
        let next = parser.next().unwrap();
        let right = assignment(parser)?;

        expr = Node::Binary {
            left: Box::new(expr),
            token: next,
            right: Box::new(right),
        };
    }

    Ok(expr)
}

/// Parses an assignment. First parses the l-value as a normal expression. Then determines if it is followed by an assignment
/// operator which says we are looking at an assignment. Then checks, if the already parsed l-value is something that values
/// can be assigned to. If it is, parse the r-value as another expression and wrap all up in an `Assignment`-expression. If not,
/// return an error.
pub(crate) fn assignment(parser: &mut Parser) -> ExpressionResult {
    let expr = addition(parser)?;

    if parser.next_token_one_of(&[
        TokenType::Assignment,
        TokenType::BinaryAndAssignment,
        TokenType::BinaryOrAssignment,
        TokenType::ModuloAssignment,
        TokenType::ConcatAssignment,
        TokenType::XorAssignment,
        TokenType::RightShiftAssignment,
        TokenType::LeftShiftAssignment,
        TokenType::ModuloAssignment,
        TokenType::ConcatAssignment,
        TokenType::XorAssignment,
        TokenType::RightShiftAssignment,
        TokenType::LeftShiftAssignment,
        TokenType::PowerAssignment,
        TokenType::CoalesceAssignment,
        TokenType::PlusAssign,
        TokenType::MinusAssign,
        TokenType::MulAssign,
        TokenType::DivAssign,
    ]) {
        // Past the assignment token
        let operator = parser.next().unwrap();
        let value = assignment(parser)?;

        if expr.is_lvalue() {
            return Ok(Node::Binary {
                left: Box::new(expr),
                token: operator,
                right: Box::new(value),
            });
        } else {
            return Err(Error::RValueInWriteContext { token: operator });
        }
    }

    Ok(expr)
}

pub(crate) fn addition(parser: &mut Parser) -> ExpressionResult {
    let mut expr = multiplication(parser)?;

    let potential_matches = vec![
        TokenType::Minus,
        TokenType::Plus,
        TokenType::LeftShift,
        TokenType::RightShift,
        TokenType::BinaryAnd,
        TokenType::BinaryOr,
        TokenType::BinaryXor,
        TokenType::Modulo,
    ];

    while parser.next_token_one_of(&potential_matches) {
        let next = parser.next().unwrap();
        let right = multiplication(parser)?;

        expr = Node::Binary {
            left: Box::new(expr),
            token: next,
            right: Box::new(right),
        };
    }

    Ok(expr)
}

pub(crate) fn multiplication(parser: &mut Parser) -> ExpressionResult {
    let mut expr = unary(parser)?;

    let potential_matches = vec![
        TokenType::Multiplication,
        TokenType::Power,
        TokenType::Division,
        // TODO: Make sure precendence is the same, otherwise split
        TokenType::Concat,
        TokenType::BinaryOr,
        TokenType::BinaryAnd,
    ];

    while parser.next_token_one_of(&potential_matches) {
        let next = parser.next().unwrap();
        let right = unary(parser)?;

        expr = Node::Binary {
            left: Box::new(expr),
            token: next.clone(),
            right: Box::new(right),
        };
    }

    Ok(expr)
}

pub(crate) fn unary(parser: &mut Parser) -> ExpressionResult {
    if parser.next_token_one_of(&[
        TokenType::Negation,
        TokenType::Minus,
        TokenType::Plus,
        TokenType::Increment,
        TokenType::Decrement,
        TokenType::BoolCast,
        TokenType::IntCast,
        TokenType::StringCast,
        TokenType::ArrayCast,
        TokenType::ObjectCast,
        TokenType::DoubleCast,
        TokenType::UnsetCast,
        TokenType::Elipsis,
        TokenType::Clone,
        TokenType::Silencer,
        TokenType::BinaryAnd,
        TokenType::BitwiseNegation,
    ]) {
        let next = parser.next().unwrap();
        let right = unary(parser)?;

        return Ok(Node::Unary {
            token: next,
            expr: Box::new(right),
        });
    }

    let primary = calls::call(parser)?;

    if parser.next_token_one_of(&[TokenType::Increment, TokenType::Decrement]) {
        let next = parser.next().unwrap();

        return Ok(Node::PostUnary {
            token: next,
            expr: Box::new(primary),
        });
    }
    Ok(primary)
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
            resource: Box::new(expression(parser)?),
        });
    }

    if parser.next_token_one_of(&[TokenType::OpenBrackets]) {
        let expr = arrays::array(parser)?;

        return Ok(expr);
    }

    if parser.next_token_one_of(&[TokenType::OpenParenthesis]) {
        parser.consume_or_ff_after(TokenType::OpenParenthesis, &[TokenType::OpenParenthesis])?;
        let expr = expression(parser)?;
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
            expr: Box::new(expression(parser)?),
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
