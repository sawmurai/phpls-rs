use super::super::node::Node;
use super::super::token::TokenType;
use super::super::{ExpressionResult, Parser};
use super::{expressions, types};

/// Parses a try catch statement
///
/// # Details
/// ```php
/// /** from here **/
/// try (true) {
/// } catch (Exception $e) {}
///     echo "stuff";
/// }
/// /** to here **/
/// ```
pub(crate) fn try_catch_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Try)?;
    let try_block = parser.block()?;

    let mut catch_blocks = Vec::new();

    while parser.next_token_one_of(&[TokenType::Catch]) {
        catch_blocks.push(*catch_block(parser)?);
    }

    let finally_block = if let Some(finally_token) = parser.consume_or_ignore(TokenType::Finally) {
        Some(Box::new(Node::FinallyBlock {
            token: finally_token,
            body: parser.block()?,
        }))
    } else {
        None
    };

    Ok(Box::new(Node::TryCatch {
        token,
        try_block,
        catch_blocks,
        finally_block,
    }))
}

/// Parses a catch block (including the catch-keyword, yes, I need to make my mind up about including / excluding the keyword)
///
/// # Details
/// ```php
/// /** from here **/catch (Exception $e) {}
///     echo "stuff";
/// }
/// /** to here **/
/// ```
pub(crate) fn catch_block(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Catch)?;
    let op = parser.consume(TokenType::OpenParenthesis)?;
    let types = types::non_empty_type_ref_union(parser)?;
    let var = parser.consume(TokenType::Variable)?;
    let cp = parser.consume(TokenType::CloseParenthesis)?;

    let body = parser.block()?;

    Ok(Box::new(Node::CatchBlock {
        token,
        op,
        types,
        var,
        cp,
        body,
    }))
}

pub(crate) fn throw_statement(parser: &mut Parser) -> ExpressionResult {
    let token = parser.consume(TokenType::Throw)?;
    let expression = expressions::expression(parser)?;

    parser.consume_end_of_statement()?;

    Ok(Box::new(Node::ThrowStatement { token, expression }))
}
