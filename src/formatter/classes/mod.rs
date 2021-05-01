use crate::parser::{
    node::{ClassStatement, Node},
    token::Token,
};

use super::v2::Span;

pub(crate) fn class_stmt_to_spans(
    spans: &mut Vec<Span>,
    tokens: &[Token],
    stmt: &ClassStatement,
    lvl: u8,
) {
}
