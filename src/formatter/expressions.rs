use crate::parser::{
    node::Node,
    token::{Token, TokenType},
};

use super::v2::{next_that, node_to_spans, prev_that, Chunk, Span};

// Convert an expression statement into a set of spans.
//
// First get the spans of the inner expression. Then, take the first inner expression and
// prepend all multiline comments that are between the expressions left edge and the previous
// non-comment token.
// Then, get the last inner span and find the actual end of the statement by finding the
// semicolon that terminates the statement. Check if the next token is a line comment and if so,
// make sure to also attach that one.
// Afterwards attach all inner spans to the span container
#[inline]
pub(crate) fn expression_stmt_to_spans(
    spans: &mut Vec<Span>,
    tokens: &[Token],
    expression: &Node,
    lvl: u8,
) {
    let mut inner_spans = node_to_spans(expression, tokens, lvl);

    if let Some(inner_span) = inner_spans.first_mut() {
        let left_edge = inner_span.left_offset();

        let prev_non_comment =
            prev_that(left_edge, tokens, &|t| t.t != TokenType::MultilineComment);

        // Add all the comments from before this expression
        inner_span.left_extend(Chunk::new(&tokens[prev_non_comment + 1..left_edge]));
    }

    if let Some(inner_span) = inner_spans.last_mut() {
        // Add all the stuff to the end of the line
        let right_edge = inner_span.right_offset();

        let end_of_statement = next_that(right_edge, tokens, &|t| t.t == TokenType::Semicolon);

        let end_of_span = if tokens[end_of_statement + 1].t == TokenType::LineComment {
            end_of_statement + 1
        } else {
            end_of_statement
        };

        // Expand the span to either directly after the statement or after a line comment
        inner_span.right_extend(Chunk::new(&tokens[right_edge + 1..=end_of_span]));
    }

    spans.extend(inner_spans);
}

// Walk down the binary (tree) and create chunks from it. The function also needs
// to take linecomments into account and create mulitple spans whenever one
// of those comments is encountered.
#[inline]
pub(crate) fn binary_to_spans(
    spans: &mut Vec<Span>,
    tokens: &[Token],
    left: &Node,
    right: &Node,
    lvl: u8,
) {
    let mut chunks = Vec::new();
    let mut subspans: Vec<Span> = Vec::new();

    // Split the left side down into spans
    let left_spans = node_to_spans(left, tokens, lvl);
    if let Some(first) = left_spans.first() {
        chunks.extend(first.chunks.clone());
        subspans.push(first.clone());
    }

    // Basically the right edge of the left side
    let rels = chunks.last().unwrap().right_offset();

    let mut right_spans = node_to_spans(right, tokens, lvl);

    // Take the first span on the right side to determine its left offset
    if let Some(right_first) = right_spans.first_mut() {
        // Now capture all in between. That includes comments and the operator
        let lmt = right_first.left_offset();

        let mut start = rels + 1;
        while let Some(lc) = &tokens[start..lmt]
            .iter()
            .find(|t| t.t == TokenType::LineComment)
        {
            // Try to find a line comment in the part between the left and the right side
            // If a line comment is found, add it and everything before it as one separate span

            let lc_offset = lc.offset.unwrap() as usize;
            let chunk = Chunk::unspaced(&tokens[start..=lc_offset]);

            chunks.push(chunk.clone());
            subspans.push(Span::leaf(vec![chunk], lvl));

            spans.push(Span::new(chunks, subspans, lvl));

            chunks = Vec::new();
            subspans = Vec::new();

            start = lc_offset + 1;
        }

        // Add the rest by glueing it the left side of the right operand
        let op_chunk = Chunk::unspaced(&tokens[start..lmt]);

        right_first.left_extend(op_chunk);

        chunks.extend(right_first.chunks.clone());

        subspans.push(right_first.clone());
        spans.push(Span::new(chunks, subspans, lvl));
    }

    // Add the rest of the right spans as individual spans. There are only multiple
    // spans if the expression on the right could not be fit into one line (intermittent line comment)
    for right in right_spans.iter().skip(1) {
        spans.push(right.clone());
    }
}

#[cfg(test)]
pub(crate) mod test {
    use crate::formatter::v2::{ast_to_spans, test::ast};

    #[test]
    fn test_formats_expression() {
        let src = "\
// 1###
$a = 1 * 1 * 1 * 1
// 2###
* 
// oh oh
1000  
// 3### /** no single */
// 4###
/** single */
/ 3000; // Behind
";
        let expected = "\
// 1###
$a = 1 * 1 * 1 * 1 // 2###
* // oh oh
1000 // 3### /** no single */
// 4###
/** single *// 3000; // Behind
";

        let (tokens, ast) = ast(src);

        let first_offset = tokens.first().unwrap().offset.unwrap();
        let last_offset = tokens.last().unwrap().offset.unwrap();

        let actual = ast_to_spans(&ast, &tokens, 0, first_offset, last_offset)
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("\n");

        assert_eq!(expected, actual);

        //eprintln!(
        //    "{:#?}",
        //    ast_to_spans(&ast, &tokens, 1, first_offset, last_offset)
        //);
    }
}
