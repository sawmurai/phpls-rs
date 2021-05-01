//! Based on http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written

use crate::{
    formatter::{
        expressions::{binary_to_spans, expression_stmt_to_spans},
        loops,
    },
    parser::{
        node::Node,
        token::{Token, TokenType},
    },
};
use loops::while_to_spans;
use std::fmt::{Debug, Display, Formatter, Result};

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
    pub(crate) chunks: Vec<Chunk>,

    // Subspans this span may be broken down into
    pub(crate) spans: Vec<Span>,

    // Indentation level
    pub(crate) lvl: u8,

    pub(crate) spaced: bool,
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

        if !self.spans.is_empty() {
            self.spans.last_mut().unwrap().right_extend(chunk);
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

        if !self.spans.is_empty() {
            self.spans.first_mut().unwrap().left_extend(chunk);
        }
    }
}

// Returns the offset of the next token that matches a predicate
pub(crate) fn next_that(from: usize, tokens: &[Token], f: &dyn Fn(&Token) -> bool) -> usize {
    tokens[from + 1..]
        .iter()
        .find(|t| f(t))
        .unwrap()
        .offset
        .unwrap()
}

// Returns the offset of the next token that matches a predicate
pub(crate) fn prev_that(from: usize, tokens: &[Token], f: &dyn Fn(&Token) -> bool) -> usize {
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
pub(crate) fn pre_statement_span(token_offset: usize, tokens: &[Token], lvl: u8) -> Span {
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

/// Convert a single node into a vec of spans
pub(crate) fn node_to_spans(node: &Node, tokens: &[Token], lvl: u8) -> Vec<Span> {
    // Get all tokens that encompass the node

    let mut spans = Vec::new();

    match node {
        Node::WhileStatement {
            token,
            op,
            cp,
            condition,
            body,
        } => while_to_spans(&mut spans, tokens, token, op, cp, condition, body, lvl),
        Node::Literal(token) => {
            spans.push(Span::leaf(vec![Chunk::unspaced(&[token.clone()])], lvl))
        }
        Node::Binary { left, right, .. } => binary_to_spans(&mut spans, tokens, left, right, lvl),
        Node::Variable(token) => {
            spans.push(Span::leaf(vec![Chunk::unspaced(&[token.clone()])], lvl));
        }
        Node::ExpressionStatement { expression } => {
            expression_stmt_to_spans(&mut spans, tokens, expression, lvl)
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
pub(crate) mod test {
    use super::*;
    use crate::parser::scanner::Scanner;
    use crate::parser::Parser;

    pub(crate) fn ast(source: &str) -> (Vec<Token>, Vec<Node>) {
        let mut scanner = Scanner::new(&format!("<?php\n{}", source));

        scanner.scan().unwrap();

        let t = scanner.tokens.clone();

        (t, Parser::ast(scanner.tokens).unwrap().0)
    }
}
