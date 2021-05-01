//! Based on http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written

use std::fmt::{Debug, Display, Formatter, Result};

use crate::parser::{
    node::Node,
    token::{Token, TokenType},
};

// A chunk represents a set of token that will never ever split.
#[derive(Clone, Debug)]
pub(crate) struct Chunk {
    tokens: Vec<Token>,
    spaced: bool,
    space_after: bool,
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let len = self.tokens.len();

        for (i, token) in self.tokens.iter().enumerate() {
            let is_last = i == len - 1;
            if token.t == TokenType::Linebreak {
                continue;
            }

            // Infix operators need a space to the operand
            if token.t.is_infix_operator()
                || !is_last && (self.spaced || token.t == TokenType::Semicolon)
                || self.space_after
            {
                write!(f, "{} ", token)?;

                continue;
            }

            write!(f, "{}", token)?;
        }

        Ok(())
    }
}

impl Chunk {
    pub fn new(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.into(),
            spaced: true,
            space_after: false,
        }
    }

    pub fn single(token: Token) -> Self {
        Self {
            tokens: vec![token],
            spaced: false,
            space_after: false,
        }
    }

    pub fn unspaced(tokens: &[Token]) -> Self {
        Self {
            tokens: tokens.into(),
            spaced: false,
            space_after: false,
        }
    }

    pub fn with_space_after(self) -> Self {
        Self {
            space_after: true,
            ..self
        }
    }

    // Return the offset of the right most token
    pub fn right_offset(&self) -> usize {
        self.tokens.last().unwrap().offset.unwrap()
    }

    // Return the offset of the left most token
    pub fn left_offset(&self) -> usize {
        self.tokens.first().unwrap().offset.unwrap()
    }
}

// A span is a set of chunks.
// Spans may split but should rather not. There can be multiple spans
// for a series of chunks. One span for each combination of breaks
// we want to offer. They are generated on order of preference, starting
// with the longest. All chunks in a span shall we written on the same
// line with a following newline. For example, for the following snippet:
//
// foreach ($someCollection as $someVar) {
//
// we would generate the following sets of spans:
// 1 span: [foreach ($someCollection as $someVar) {]
// 3 spans: [foreach (,], [$someCollection as $someVar], [) {]
#[derive(Clone)]
pub(crate) struct Span {
    chunks: Vec<Chunk>,

    // Subspans this span may be broken down into
    spans: Vec<Span>,

    // Indentation level
    lvl: u8,

    spaced: bool,
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}",
            " ".repeat(self.lvl as usize * 4),
            self.chunks
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<String>>()
                .join(if self.spaced { " " } else { "" })
        )
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Span")
            .field("lvl", &self.lvl)
            //.field("spaced", &self.spaced)
            .field(
                "chunks",
                &self
                    .chunks
                    .iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(if self.spaced { " " } else { "" }),
            )
            .field("spans", &self.spans)
            .finish()
    }
}

impl Span {
    pub(crate) fn new(chunks: Vec<Chunk>, spans: Vec<Span>, lvl: u8) -> Self {
        Span {
            chunks,
            spans,
            lvl,
            spaced: true,
        }
    }

    pub(crate) fn unspaced(chunks: Vec<Chunk>, spans: Vec<Span>, lvl: u8) -> Self {
        Span {
            chunks,
            spans,
            lvl,
            spaced: false,
        }
    }

    pub(crate) fn leaf(chunks: Vec<Chunk>, lvl: u8) -> Self {
        Span {
            chunks,
            spans: vec![],
            lvl,
            spaced: false,
        }
    }

    // Return the offset of the right most token
    pub fn right_offset(&self) -> usize {
        self.chunks.last().unwrap().right_offset()
    }

    // Return the offset of the right most token
    pub fn left_offset(&self) -> usize {
        self.chunks.first().unwrap().left_offset()
    }

    // Extend all last chunks in all last spans recursively with the provided
    // chunk.
    // Usually it contains the ; at the end of a statement and maybe
    // also a comment at the end of the line.
    pub fn right_extend(&mut self, chunk: Chunk) {
        self.chunks
            .last_mut()
            .unwrap()
            .tokens
            .extend(chunk.tokens.iter().cloned());

        if self.spans.len() > 0 {
            self.spans.last_mut().unwrap().right_extend(chunk.clone());
        }
    }

    // Extend all first chunks in all first spans recursively with the provided
    // chunk
    // Usually it contains the operator of a binary assignment and
    // also a comment at the start of the line.
    pub fn left_extend(&mut self, chunk: Chunk) {
        self.chunks
            .first_mut()
            .unwrap()
            .tokens
            .splice(0..0, chunk.tokens.iter().cloned());

        if self.spans.len() > 0 {
            self.spans.first_mut().unwrap().left_extend(chunk.clone());
        }
    }
}

// Returns the offset of the next token that matches a predicate
fn next_that(from: usize, tokens: &[Token], f: &dyn Fn(&Token) -> bool) -> usize {
    // |t| !t.is_comment()
    tokens[from + 1..]
        .iter()
        .find(|t| f(t))
        .unwrap()
        .offset
        .unwrap()
}

// Returns the offset of the next token that matches a predicate
fn prev_that(from: usize, tokens: &[Token], f: &dyn Fn(&Token) -> bool) -> usize {
    // |t| !t.is_comment()
    tokens[..from]
        .iter()
        .rev()
        .find(|t| f(t))
        .unwrap()
        .offset
        .unwrap()
}

// Collect all chunks between the statement and the previous one. It basically
// returns a vector of chunks, each containing a multiline comment
fn pre_statement_span(token_offset: usize, tokens: &[Token], lvl: u8) -> Span {
    let prev_non_comment = prev_that(token_offset, tokens, &|t| {
        t.t != TokenType::MultilineComment
    });

    return Span::new(
        tokens[prev_non_comment + 1..token_offset]
            .iter()
            .cloned()
            .map(Chunk::single)
            .collect(),
        vec![],
        lvl,
    );
}

/// Turn the ast recursivly into a set of spansets. This requires reading the
/// actual tokens from the tokenstream, as the tokenstream also contains comments!
/// At the same time we need to traverse via the ast to know the context of
/// the tokens as well as the current indentation level.
pub(crate) fn ast_to_spans(
    ast: &[Node],
    stream: &[Token],
    lvl: u8,
    mut prev_right_offset: usize,
    next_left_offset: usize,
) -> Vec<Span> {
    let mut spans = Vec::new();

    for node in ast {
        let mut node_spans = node_to_spans(node, stream, lvl);

        // Get all comments between the previously read node and this node and add them
        // between the previously read node and this node in the span collection.
        if let Some(first) = node_spans.first() {
            let cur_left_offset = first.left_offset();

            let mut tokens_in_between = &stream[prev_right_offset + 1..cur_left_offset];

            if tokens_in_between.len() > 1 {
                // Cut off leading newline from previous statement
                if tokens_in_between[0].t == TokenType::Linebreak {
                    tokens_in_between = &tokens_in_between[1..];
                }

                let len = tokens_in_between.len();
                if tokens_in_between[len - 1].t == TokenType::Linebreak {
                    tokens_in_between = &tokens_in_between[..len - 1];
                }

                // Split everything in between the statements into one span per line
                spans.extend(
                    tokens_in_between
                        .split(|t| t.t == TokenType::Linebreak)
                        .map(Chunk::new)
                        .map(|c| Span::leaf(vec![c], lvl)),
                );
            }
        }
        prev_right_offset = node_spans.last().unwrap().right_offset();

        spans.extend(node_spans.drain(..));
    }

    // Finally, get the tokens between the last read node and the delimiter of the block
    // whose offset was passed in
    let tokens_in_between = &stream[prev_right_offset + 1..next_left_offset];
    let chunks_in_between = tokens_in_between
        .split(|t| t.t == TokenType::Linebreak)
        .map(Chunk::new)
        .collect();
    spans.push(Span::leaf(chunks_in_between, lvl));

    spans
}

// Walk down the binary (tree) and create chunks from it. The function also needs
// to take linecomments into account and create mulitple spans whenever one
// of those comments is encountered.
#[inline]
fn binary_to_spans(spans: &mut Vec<Span>, tokens: &[Token], left: &Node, right: &Node, lvl: u8) {
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
        loop {
            // Try to find a line comment in the part between the left and the right side
            // If a line comment is found, add it and everything before it as one separate span
            if let Some(lc) = &tokens[start..lmt]
                .iter()
                .find(|t| t.t == TokenType::LineComment)
            {
                let lc_offset = lc.offset.unwrap() as usize;
                let chunk = Chunk::unspaced(&tokens[start..=lc_offset]);

                chunks.push(chunk.clone());
                subspans.push(Span::leaf(vec![chunk], lvl));

                spans.push(Span::new(chunks, subspans, lvl));

                chunks = Vec::new();
                subspans = Vec::new();

                start = lc_offset + 1;
            } else {
                // Break and add the rest as one chunk
                break;
            }
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

/// Convert a single node into a vec of spans
fn node_to_spans(node: &Node, tokens: &[Token], lvl: u8) -> Vec<Span> {
    // Get all tokens that encompass the node

    let mut spans = Vec::new();

    match node {
        Node::WhileStatement {
            token,
            op,
            cp,
            condition,
            body,
        } => {
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
        Node::Literal(token) => {
            spans.push(Span::leaf(vec![Chunk::unspaced(&[token.clone()])], lvl))
        }
        Node::Binary { left, right, .. } => binary_to_spans(&mut spans, tokens, left, right, lvl),
        Node::Variable(token) => {
            let chunk = Chunk::unspaced(&[token.clone()]);

            spans.push(Span::leaf(vec![chunk], lvl));
        }
        Node::ExpressionStatement { expression } => {
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

                let end_of_statement =
                    next_that(right_edge, tokens, &|t| t.t == TokenType::Semicolon);

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
        Node::Block {
            oc, cc, statements, ..
        } => {
            // Blocks never take care of formatting the opening curly, because the blocks do
            // not know if its supposed to be on a new line or on the previous line
            spans.extend(ast_to_spans(
                statements,
                tokens,
                lvl,
                oc.offset.unwrap(),
                cc.offset.unwrap(),
            ));

            let cc_offset = cc.offset.unwrap();
            let start_of_next = next_that(cc_offset, tokens, &|t| !t.is_comment());

            // Add a chunk that contains the close curly plus all the comments up to the next
            // siginificant bit
            let chunks = vec![Chunk::new(&tokens[cc_offset..start_of_next])];
            spans.push(Span::new(chunks, vec![], lvl - 1));
        }
        _ => unimplemented!("{:#?}", node),
    }

    spans
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    fn ast(source: &str) -> (Vec<Token>, Vec<Node>) {
        let mut scanner = Scanner::new(&format!("<?php\n{}", source));

        scanner.scan().unwrap();

        let t = scanner.tokens.clone();

        (t, Parser::ast(scanner.tokens).unwrap().0)
    }

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
        //    ast_to_spans(&ast, &tokens, 0, first_offset, last_offset)
        //);
    }

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
