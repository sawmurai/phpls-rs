use super::super::node::Node;
use super::super::token::{Token, TokenType};
use super::super::Parser;
use super::super::Result;

struct DocBlockScanner {
    comment: Token,
    col: u32,
    line: u32,

    chars: Vec<char>,
}

impl DocBlockScanner {
    pub fn new(comment: Token) -> Self {
        let chars = comment
            .label
            .as_ref()
            .unwrap()
            .chars()
            .rev()
            .collect::<Vec<char>>();

        DocBlockScanner {
            col: comment.col,
            line: comment.line,
            comment,
            chars,
        }
    }

    fn token_type(&self, name: &str) -> TokenType {
        match name.to_lowercase().as_ref() {
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
            "callable" => TokenType::Callable,
            "generator" => TokenType::Generator,
            _ => TokenType::Identifier,
        }
    }

    pub fn scan(&mut self) -> Result<Option<Box<Node>>> {
        let mut description = String::new();
        let mut is_deprecated = false;
        let mut properties = Vec::new();
        let mut params = Vec::new();
        let mut return_type = Vec::new();
        let mut var_docs = Vec::new();

        while let Some(c) = self.advance() {
            match c {
                '@' => {
                    let directive = self.collect_identifer(false);
                    self.skip_blanks();

                    if directive.eq("param") {
                        // /** @param string|int $param The param is niiiice */
                        self.skip_blanks();

                        let mut type_refs = Vec::new();
                        while let Some(type_ref) = self.collect_type_ref(false) {
                            type_refs.push(type_ref);

                            match self.peek() {
                                Some('|') => {
                                    self.advance();
                                    continue;
                                }
                                _ => break,
                            }
                        }

                        self.skip_blanks();

                        let identifer = (self.line, self.col);
                        let param_name = match self.peek() {
                            Some('$') => {
                                self.advance();
                                self.collect_identifer(false)
                            }
                            _ => "".to_owned(),
                        };

                        let mut param_descr = String::new();
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
                            types: Some(type_refs),
                            description: param_descr,
                        });
                    } else if directive.eq("return") {
                        self.skip_blanks();

                        let mut type_refs = Vec::new();
                        while let Some(type_ref) = self.collect_type_ref(true) {
                            type_refs.push(type_ref);

                            match self.peek() {
                                Some('|') => {
                                    self.advance();
                                    continue;
                                }
                                _ => break,
                            }
                        }

                        self.skip_blanks();

                        let mut return_descr = String::new();
                        while let Some(n) = self.advance() {
                            match n {
                                '\n' => break,
                                _ => return_descr.push(n),
                            }
                        }

                        if type_refs.is_empty() {
                            return_type.push(Node::DocCommentReturn {
                                types: None,
                                description: return_descr,
                            });
                        } else {
                            return_type.push(Node::DocCommentReturn {
                                types: Some(type_refs),
                                description: return_descr,
                            });
                        }
                    } else if directive.eq("var") {
                        // /** @var User $rofl */
                        self.skip_blanks();

                        let mut type_refs = Vec::new();
                        while let Some(type_ref) = self.collect_type_ref(false) {
                            type_refs.push(type_ref);

                            match self.peek() {
                                Some('|') => {
                                    self.advance();
                                    continue;
                                }
                                _ => break,
                            }
                        }

                        self.skip_blanks();

                        let identifier_start = (self.line, self.col);
                        let param_name = match self.peek() {
                            Some('$') => {
                                self.advance();
                                self.collect_identifer(false)
                            }
                            _ => "".to_owned(),
                        };

                        let mut param_descr = String::new();
                        while let Some(n) = self.advance() {
                            match n {
                                '\n' => break,
                                _ => param_descr.push(n),
                            }
                        }

                        var_docs.push(Node::DocCommentVar {
                            name: Token::named(
                                TokenType::Variable,
                                identifier_start.0,
                                identifier_start.1,
                                &param_name,
                            ),
                            types: Some(type_refs),
                            description: param_descr,
                        });
                    } else if directive.eq("deprecated") {
                        is_deprecated = true;
                    } else if directive.eq("property") {
                        self.skip_blanks();

                        let mut type_refs = Vec::new();
                        while let Some(type_ref) = self.collect_type_ref(false) {
                            type_refs.push(type_ref);

                            match self.peek() {
                                Some('|') => {
                                    self.advance();
                                    continue;
                                }
                                _ => break,
                            }
                        }

                        self.skip_blanks();

                        let identifier_start = (self.line, self.col);
                        let param_name = match self.peek() {
                            Some('$') => {
                                self.advance();
                                self.collect_identifer(false)
                            }
                            _ => "".to_owned(),
                        };

                        let mut param_descr = String::new();
                        while let Some(n) = self.advance() {
                            match n {
                                '\n' => break,
                                _ => param_descr.push(n),
                            }
                        }

                        let type_refs = if type_refs.is_empty() {
                            None
                        } else {
                            Some(type_refs)
                        };

                        properties.push(Node::DocCommentProperty {
                            name: Token::named(
                                TokenType::Variable,
                                identifier_start.0,
                                identifier_start.1,
                                &param_name,
                            ),
                            types: type_refs,
                            description: param_descr,
                        })
                    }
                }
                '*' => (),
                _ => description.push(c),
            }
        }

        Ok(Some(Box::new(Node::DocComment {
            comment: self.comment.clone(),
            description,
            is_deprecated,
            params,
            return_type,
            var_docs,
            properties,
        })))
    }

    fn collect_type_ref(&mut self, allow_this: bool) -> Option<Node> {
        let mut type_ref_parts = Vec::new();
        let current_start = (self.line, self.col);

        loop {
            let identifier = &self.collect_identifer(allow_this);
            if !identifier.is_empty() {
                type_ref_parts.push(Token::named(
                    self.token_type(&identifier),
                    current_start.0,
                    current_start.1,
                    identifier,
                ));
            }

            let n = self.peek();
            match n {
                Some('\\') => type_ref_parts.push(Token::new(
                    TokenType::NamespaceSeparator,
                    current_start.0,
                    current_start.1,
                )),
                _ => {
                    if !type_ref_parts.is_empty() {
                        return Some(Node::TypeRef(type_ref_parts.clone()));
                    } else {
                        return None;
                    }
                }
            }

            self.advance();
        }
    }

    fn collect_identifer(&mut self, allow_this: bool) -> String {
        let mut name = String::new();

        while let Some(&c) = self.peek() {
            if ('a'..='z').contains(&c)
                || ('A'..='Z').contains(&c)
                || ('0'..='9').contains(&c)
                || c == '_'
                // Need to support $ as @return $this is possible
                || c == '$' && allow_this
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

    /// Return the next token without popping it off the stream
    fn peek(&self) -> Option<&char> {
        self.chars.last()
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.chars.pop() {
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

pub(crate) fn param_comment_for(doc_comment: &Option<Box<Node>>, p_name: &Token) -> Option<Node> {
    if let Some(doc_comment) = doc_comment {
        if let Node::DocComment { params, .. } = doc_comment.as_ref() {
            for param in params {
                if let Node::DocCommentParam { name, .. } = param {
                    if name.label.is_some() && name.label == p_name.label {
                        return Some(param.clone());
                    }
                }
            }
        }
    }

    None
}
