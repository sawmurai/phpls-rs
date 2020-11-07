use crate::node::Node;
use crate::parser::Parser;
use crate::parser::Result;
use crate::token::{Token, TokenType};

struct DocBlockScanner {
    col: u32,
    line: u32,

    pub tokens: Vec<Token>,
    chars: Vec<char>,
}

impl DocBlockScanner {
    pub fn new(comment: Token) -> Self {
        let chars = comment.label.unwrap().chars().rev().collect::<Vec<char>>();

        DocBlockScanner {
            col: comment.col,
            line: comment.line,
            tokens: Vec::new(),
            chars,
        }
    }

    pub fn scan(&mut self) -> Result<Option<Box<Node>>> {
        let mut description = String::new();
        let mut is_deprecated = false;
        let mut params = Vec::new();
        let mut return_type = Vec::new();
        let mut var_docs = Vec::new();

        while let Some(c) = self.advance() {
            match c {
                '@' => {
                    let directive = self.collect_identifer();
                    self.skip_blanks();

                    if directive.eq("param") {
                        let mut param_descr = String::new();
                        self.skip_blanks();

                        let mut type_ref_parts = Vec::new();
                        loop {
                            type_ref_parts.push(Token::named(
                                TokenType::Identifier,
                                self.line,
                                self.col,
                                &self.collect_identifer(),
                            ));

                            let n = self.advance();
                            match n {
                                Some(' ') | Some('\n') | None => break,
                                _ => continue,
                            }
                        }

                        self.advance();
                        let param_name = self.collect_identifer();
                        let identifer = (self.col, self.line);

                        while let Some(n) = self.advance() {
                            match n {
                                '\n' => break,
                                _ => param_descr.push(n),
                            }
                        }

                        params.push(Node::DocCommentParam {
                            name: Token::named(
                                TokenType::Variable,
                                identifer.0,
                                identifer.1,
                                &param_name,
                            ),
                            types: Some(Box::new(Node::TypeRef(type_ref_parts))),
                            description: param_descr,
                        });
                    } else if directive.eq("return") {
                        let mut return_descr = String::new();
                        self.skip_blanks();

                        let mut type_ref_parts = Vec::new();
                        'outer: loop {
                            type_ref_parts.push(Token::named(
                                TokenType::Identifier,
                                self.line,
                                self.col,
                                &self.collect_identifer(),
                            ));

                            let n = self.advance();
                            match n {
                                Some('|') => continue,
                                Some('\n') | None => break,
                                _ => (),
                            }

                            while let Some(n) = self.advance() {
                                match n {
                                    '\n' => break 'outer,
                                    _ => return_descr.push(n),
                                }
                            }
                        }

                        return_type.push(Node::DocCommentReturn {
                            types: Some(Box::new(Node::TypeRef(type_ref_parts))),
                            description: return_descr,
                        });
                    } else if directive.eq("var") {
                        // /** @var $rofl User */
                        let mut param_descr = String::new();
                        self.skip_blanks();

                        let mut type_ref_parts = Vec::new();
                        loop {
                            type_ref_parts.push(Token::named(
                                TokenType::Identifier,
                                self.line,
                                self.col,
                                &self.collect_identifer(),
                            ));

                            let n = self.advance();
                            match n {
                                Some(' ') | Some('\n') | None => break,
                                _ => continue,
                            }
                        }

                        self.advance();
                        let param_name = self.collect_identifer();
                        let identifer = (self.col, self.line);

                        while let Some(n) = self.advance() {
                            match n {
                                '\n' => break,
                                _ => param_descr.push(n),
                            }
                        }

                        var_docs.push(Node::DocCommentVar {
                            name: Token::named(
                                TokenType::Variable,
                                identifer.0,
                                identifer.1,
                                &param_name,
                            ),
                            types: Some(Box::new(Node::TypeRef(type_ref_parts))),
                            description: param_descr,
                        });
                    } else if directive.eq("deprecated") {
                        is_deprecated = true;
                    }
                }
                '*' => (),
                _ => description.push(c),
            }
        }

        Ok(Some(Box::new(Node::DocComment {
            description,
            is_deprecated,
            params,
            return_type,
            var_docs,
        })))
    }

    fn collect_identifer(&mut self) -> String {
        let mut name = String::new();

        while let Some(&c) = self.peek() {
            if c >= 'a' && c <= 'z'
                || c >= 'A' && c <= 'Z'
                || c >= '0' && c <= '9'
                || c == '_'
                || c >= 0x80 as char
            {
                name.push(c);
            } else {
                break;
            }

            self.advance();
        }

        name
    }

    /// Pop and return the next token
    fn next(&mut self) -> Option<char> {
        self.chars.pop()
    }

    /// Return the next token without popping it off the stream
    fn peek(&self) -> Option<&char> {
        self.chars.last()
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.next() {
            if c == '\n' || c == '\r' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }

            return Some(c);
        }

        None
    }

    fn skip_blanks(&mut self) {
        if let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                return;
            }
        }
    }
}

/// Parse a doc comment
pub(crate) fn consume_optional_doc_comment(parser: &mut Parser) -> Result<Option<Box<Node>>> {
    if parser.doc_comments.is_empty() {
        return Ok(None);
    }

    let comment = parser.doc_comments.pop().unwrap();

    let mut scanner = DocBlockScanner::new(comment);

    let x = scanner.scan();
    x
}
