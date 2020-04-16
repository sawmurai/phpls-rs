use crate::parser::{Parser, StatementResult};
use crate::statement::*;
use crate::token::{Token, TokenType};

/// Parses a try catch statement
///
/// # Details
/// ```php
/// try /** from here **/(true) {
/// } catch (Exception $e) {}
///     echo "stuff";
/// }
/// /** to here **/
/// ```
pub(crate) fn try_catch_statement(parser: &mut Parser) -> StatementResult {
    let try_block = parser.block()?;

    let mut catch_blocks = Vec::new();

    while parser.next_token_one_of(&[TokenType::Catch]) {
        catch_blocks.push(catch_block(parser)?);
    }

    let finally_block = match parser.peek() {
        Some(Token {
            t: TokenType::Finally,
            ..
        }) => {
            parser.next();
            Some(parser.block()?)
        }
        _ => None,
    };

    Ok(Box::new(TryCatch::new(
        try_block,
        catch_blocks,
        finally_block,
    )))
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
pub(crate) fn catch_block(parser: &mut Parser) -> StatementResult {
    parser.consume_or_err(TokenType::Catch)?;
    parser.consume_or_err(TokenType::OpenParenthesis)?;
    let types = parser.type_ref_union()?;
    let var = parser.consume_cloned(TokenType::Variable)?;
    parser.consume_or_err(TokenType::CloseParenthesis)?;

    let catch_block = parser.block()?;

    Ok(Box::new(CatchBlock::new(types, var, catch_block)))
}
