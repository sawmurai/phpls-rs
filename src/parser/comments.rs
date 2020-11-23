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

    fn token_type(&self, name: &str) -> TokenType {
        match name {
            "null" => TokenType::Null,
            "mixed" => TokenType::Mixed,
            "bool" | "boolean" => TokenType::TypeBool,
            "int" | "integer" => TokenType::TypeInt,
            "string" | "binary" => TokenType::TypeString,
            "static" => TokenType::Static,
            "self" => TokenType::TypeSelf,
            "array" => TokenType::TypeArray,
            "object" => TokenType::TypeObject,
            "float" | "double" => TokenType::TypeFloat,
            "void" => TokenType::Void,
            _ => TokenType::Identifier,
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
                        let mut type_refs = Vec::new();
                        loop {
                            let identifier = &self.collect_identifer();
                            type_ref_parts.push(Token::named(
                                self.token_type(&identifier),
                                self.line,
                                self.col,
                                identifier,
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
                                '|' => {
                                    type_refs.push(Node::TypeRef(type_ref_parts.clone()));
                                    type_ref_parts.clear();

                                    continue;
                                }
                                '\n' => break,
                                _ => param_descr.push(n),
                            }
                        }

                        type_refs.push(Node::TypeRef(type_ref_parts.clone()));
                        type_ref_parts.clear();

                        params.push(Node::DocCommentParam {
                            name: Token::named(
                                TokenType::Variable,
                                identifer.0,
                                identifer.1,
                                &param_name,
                            ),
                            types: Some(type_refs),
                            description: param_descr,
                        });
                    } else if directive.eq("return") {
                        let mut return_descr = String::new();
                        self.skip_blanks();

                        let mut type_ref_parts = Vec::new();
                        let mut type_refs = Vec::new();
                        'outer: loop {
                            let identifier = &self.collect_identifer();

                            type_ref_parts.push(Token::named(
                                self.token_type(&identifier),
                                self.line,
                                self.col,
                                identifier,
                            ));

                            let n = self.peek();
                            match n {
                                Some('|') => {
                                    type_refs.push(Node::TypeRef(type_ref_parts.clone()));
                                    type_ref_parts.clear();
                                    self.advance();

                                    continue;
                                }
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

                        type_refs.push(Node::TypeRef(type_ref_parts.clone()));
                        type_ref_parts.clear();

                        return_type.push(Node::DocCommentReturn {
                            types: Some(type_refs),
                            description: return_descr,
                        });
                    } else if directive.eq("var") {
                        // /** @var $rofl User */
                        let mut param_descr = String::new();
                        self.skip_blanks();

                        let mut type_ref_parts = Vec::new();
                        let mut type_refs = Vec::new();
                        loop {
                            let identifier = &self.collect_identifer();

                            type_ref_parts.push(Token::named(
                                self.token_type(&identifier),
                                self.line,
                                self.col,
                                identifier,
                            ));

                            let n = self.advance();
                            match n {
                                Some('|') => {
                                    type_refs.push(Node::TypeRef(type_ref_parts.clone()));
                                    type_ref_parts.clear();
                                    self.advance();

                                    continue;
                                }
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

                        type_refs.push(Node::TypeRef(type_ref_parts.clone()));
                        type_ref_parts.clear();

                        var_docs.push(Node::DocCommentVar {
                            name: Token::named(
                                TokenType::Variable,
                                identifer.0,
                                identifer.1,
                                &param_name,
                            ),
                            types: Some(type_refs),
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
            if ('a'..='z').contains(&c)
                || ('A'..='Z').contains(&c)
                || ('0'..='9').contains(&c)
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

    scanner.scan()
}
