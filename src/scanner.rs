#[allow(dead_code)]
use crate::token::{Token, TokenType};

use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq)]
enum Context {
    IN_SCRIPT,
    IN_STRING,
    OUT_SCRIPT,
    IN_COMMENT,
}

macro_rules! advance {
    ($m:expr, $( $x:expr ),* ) => {
        true
        $(
            && $m? == $x
        )*
    };
}

pub struct Scanner<'a> {
    col: usize,
    line: usize,
    pos: usize,

    context: Context,
    tokens: Vec<Token>,
    chars: Peekable<Chars<'a>>,
    src: &'a str,
    len: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            col: 0,
            line: 0,
            pos: 0,
            context: Context::OUT_SCRIPT,

            tokens: Vec::new(),
            chars: source.chars().peekable(),
            len: source.len(),
            src: source,
        }
    }

    /// Scans the source file into a token stream `tokens`
    pub fn scan(&mut self) -> Option<()> {
        loop {
            if self.context == Context::OUT_SCRIPT {
                if advance!(self.advance(), '<', '?', 'p', 'h', 'p') {
                    self.context = Context::IN_SCRIPT;
                    self.push_token(TokenType::ScriptStart);
                }
            }

            let c = match self.advance() {
                Some(c) => c,
                _ => {
                    break;
                }
            };

            // Ignore everything until the multiline comment is done
            if self.context == Context::IN_COMMENT {
                if c == '*' && self.chars.peek()? == &'/' {
                    self.context = Context::IN_SCRIPT;
                }

                continue;
            }

            match c {
                ' ' | '\t' | '\r' => {}
                '>' => match self.chars.peek() {
                    Some('>') => {
                        self.advance();

                        match self.chars.peek() {
                            Some('=') => {
                                self.advance();

                                self.push_token(TokenType::RightShiftAssignment);
                            }
                            _ => {
                                self.push_token(TokenType::RightShift);
                            }
                        }
                    }
                    Some('=') => {
                        self.advance();

                        self.push_token(TokenType::SmallerOrEqual);
                    }
                    _ => {
                        self.push_token(TokenType::Greater);
                    }
                },
                '<' => match self.chars.peek() {
                    Some('<') => {
                        self.advance();

                        match self.chars.peek() {
                            Some('=') => {
                                self.advance();

                                self.push_token(TokenType::LeftShiftAssignment);
                            }
                            _ => {
                                self.push_token(TokenType::LeftShift);
                            }
                        }
                    }
                    Some('=') => {
                        self.advance();

                        self.push_token(TokenType::SmallerOrEqual);
                    }
                    _ => {
                        self.push_token(TokenType::Smaller);
                    }
                },
                '?' => match self.chars.peek() {
                    Some('>') => {
                        self.advance();
                        self.push_token(TokenType::ScriptEnd);
                        self.context = Context::OUT_SCRIPT;
                    }
                    Some('?') => {
                        self.advance();
                        self.push_token(TokenType::Coalesce);
                    }
                    _ => {
                        self.push_token(TokenType::QuestionMark);
                    }
                },
                '!' => match self.chars.peek() {
                    Some('=') => match self.chars.peek() {
                        Some('=') => {
                            self.advance();
                            self.push_token(TokenType::IsNotIdentical);
                        }
                        _ => {
                            self.push_token(TokenType::IsNotEqual);
                        }
                    },
                    _ => {
                        self.push_token(TokenType::QuestionMark);
                    }
                },
                '+' => match self.chars.peek() {
                    Some('+') => {
                        self.advance();
                        self.push_token(TokenType::Increment);
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::PlusAssign);
                    }
                    _ => {
                        self.push_token(TokenType::Plus);
                    }
                },
                '-' => match self.chars.peek() {
                    Some('-') => {
                        self.advance();

                        self.push_token(TokenType::Decrement);
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::MinusAssign);
                    }
                    _ => {
                        self.push_token(TokenType::Minus);
                    }
                },
                '*' => match self.chars.peek() {
                    Some('*') => {
                        self.advance();

                        match self.chars.peek() {
                            Some('=') => {
                                self.advance();

                                self.push_token(TokenType::PowerAssignment);
                            }
                            _ => {
                                self.push_token(TokenType::Power);
                            }
                        }
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::MulAssign);
                    }
                    _ => {
                        self.push_token(TokenType::Multiplication);
                    }
                },
                '/' => match self.chars.peek() {
                    Some('/') => {
                        self.advance_until_after('\n');

                        self.push_token(TokenType::LineComment);
                    }
                    Some('*') => {
                        self.advance();
                        self.push_token(TokenType::MultilineComment);

                        self.context = Context::IN_COMMENT;
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::DivAssign);
                    }
                    _ => {
                        self.push_token(TokenType::Division);
                    }
                },
                '=' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();

                        match self.chars.peek() {
                            Some('=') => {
                                self.advance();
                                self.push_token(TokenType::IsIdentical);
                            }
                            _ => {
                                self.push_token(TokenType::IsEqual);
                            }
                        }
                    }
                    _ => {
                        self.push_token(TokenType::Assignment);
                    }
                },
                '$' => match self.chars.peek() {
                    Some('$') => {
                        self.advance();

                        let name = self.collect_identifer();

                        self.push_named_token(TokenType::DynamicVariable, &name);
                    },
                    _ => {
                        self.push_named_token(TokenType::Variable, self.collect_identifer());
                    } 
                },
                _ => {
                    //eprintln!("Unexpected char {} at line {} column {}", c, self.line, self.col);
                }
            }

            self.col += 1;
        }

        println!("Parsed {} tokens", self.tokens.len());

        Some(())
    }

    fn collect_identifer(&mut self) -> &str {
        unimplemented!(":)");
    }

    fn advance(&mut self) -> Option<char> {
        self.pos += 1;

        if let Some(c) = self.chars.next() {
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            }

            return Some(c);
        }

        None
    }

    /// Advance until end of the file or until a specific character is seen
    fn advance_until_after(&mut self, until: char) {
        while let Some(c) = self.advance() {
            if c == until {
                break;
            }
        }
    }

    fn push_token(&mut self, t: TokenType) {
        self.tokens
            .push(Token::new(t, self.line as u16, self.col as u16, None));
    }

    fn push_named_token(&mut self, t: TokenType, name: &str) {
        self.tokens
            .push(Token::new(t, self.line as u16, self.col as u16, Some(name.to_owned())));
    }
}
