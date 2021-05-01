use super::v2::{pre_statement_span, Chunk, Span};
use crate::{
    formatter::v2::node_to_spans,
    parser::{
        node::Node,
        token::{Token, TokenType},
    },
};

// Split a while loop into spans
#[inline]
pub(crate) fn while_to_spans(
    spans: &mut Vec<Span>,
    tokens: &[Token],
    token: &Token,
    op: &Token,
    cp: &Token,
    condition: &Node,
    body: &Node,
    lvl: u8,
) {
    let token_offset = token.offset.unwrap();
    let op_offset = op.offset.unwrap();
    let cp_offset = cp.offset.unwrap();

    // Start with all comments preceding this statement, then add the token chunk
    let pre_span = pre_statement_span(token_offset, tokens, lvl);

    // Add a chunk for the while token and the opening parenthesis
    let while_op = Chunk::new(&tokens[token_offset..=op_offset]);

    // Get all the stuff between the closing parenthesis and the start of the block, excluding the
    // start of the block. We are searching for something that is not a newline or a comment. Searching
    // for a { is not a good idea as the block might not have one
    let oc = tokens[cp_offset + 1..]
        .iter()
        .find(|t| !t.is_comment() && t.t != TokenType::Linebreak)
        .unwrap();

    // If the block starts with a { we glue it to this chunk
    let cp_oc = if oc.t == TokenType::OpenCurly {
        Chunk::new(&tokens[cp_offset..=oc.offset.unwrap()])
    } else {
        Chunk::unspaced(&tokens[cp_offset..oc.offset.unwrap()])
    };

    let token_span = Span::leaf(vec![while_op.clone()], lvl);

    let mut chunks = pre_span.chunks.clone();
    chunks.push(while_op);

    let mut subspans = vec![pre_span, token_span];
    let mut condition_spans = node_to_spans(condition, tokens, lvl + 1);
    let mut body_spans = node_to_spans(body, tokens, lvl + 1);

    if let Some(first) = condition_spans.first() {
        // Internalize span-spacing into chunks by attaching a space-right into each of the chunks, except for the last one.
        // TODO: Move this into a Span::chunks method that converts the internal chunks into externals, basically doing what the following block does.
        if first.spaced {
            let len = first.chunks.len();
            chunks.extend(first.chunks.iter().cloned().enumerate().map(|(i, c)| {
                if i == len - 1 {
                    c
                } else {
                    c.with_space_after()
                }
            }));
        } else {
            chunks.extend(first.chunks.clone());
        }
    }

    chunks.push(cp_oc.clone());

    subspans.extend(condition_spans.drain(..));
    subspans.push(Span::leaf(vec![cp_oc], lvl));

    let head_span = Span::unspaced(chunks, subspans, lvl);

    spans.push(head_span);
    spans.extend(body_spans.drain(..));
}

#[cfg(test)]
mod test {
    use super::super::v2;

    #[test]
    fn test_formats_while() {
        let src = "\
// line comment before while
/*d*/while ($rofl   == true ) {
// 1st line comment
/* c1 */true == /* c2 */true/* c3 */ && /* c4 */false == false /* c5 */; // c6
$a = 12 * /*lol*/ 1000 
// What happens now?
    / 3000* 1000 / 3000* 1000 / 3000* 1000; 
// 2nd line comment
// 2nd 2nd line comment
/* cb */$a = 2; /*cc*/$b = 3; /*ddddd*/

$a = 1;
$a = 1;
// 3rd new line comment
} // me as well??
";
        let expected = "\
// line comment before while
/*d*/while ($rofl == true) {
    // 1st line comment
    /* c1 */true == /* c2 */true /* c3 */&& /* c4 */false == false/* c5 */; // c6
    $a = 12 * /*lol*/1000 // What happens now?
    / 3000 * 1000 / 3000 * 1000 / 3000 * 1000;
    // 2nd line comment
    // 2nd 2nd line comment
    /* cb */$a = 2;
    /*cc*/$b = 3;
    /*ddddd*/
    
    $a = 1;
    $a = 1;
    // 3rd new line comment
} // me as well??
";
        let (tokens, ast) = v2::test::ast(src);

        let first_offset = tokens.first().unwrap().offset.unwrap();
        let last_offset = tokens.last().unwrap().offset.unwrap();
        let actual = v2::ast_to_spans(&ast, &tokens, 0, first_offset, last_offset)
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("\n");

        assert_eq!(expected, actual);

        //eprintln!(
        //    "{:#?}",
        //    ast_to_spans(&ast, &tokens, 0, first_offset, last_offset)
        //);
    }
}
