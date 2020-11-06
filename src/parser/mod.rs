use crate::node::*;
use crate::token::{Token, TokenType};
use snafu::Snafu;

use tower_lsp::lsp_types::Diagnostic;

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Unexpected token {:?}, expected on of [{:?}] on line {}, col {}", token.t, expected, token.line, token.col))]
    WrongTokenError {
        expected: Vec<TokenType>,
        token: Token,
    },

    #[snafu(display("Unexpected token {:?} on line {}, col {}", token.t, token.line, token.col))]
    UnexpectedTokenError { token: Token },

    #[snafu(display("Illegal offset type on line {}, col {}", (expr.range().0).0, (expr.range().0).1))]
    IllegalOffsetType { expr: Node },

    #[snafu(display("Can not use expression in write context on line {}, col {}", token.line, token.col))]
    RValueInWriteContext { token: Token },
}

impl From<&Error> for Diagnostic {
    fn from(e: &Error) -> Diagnostic {
        match e {
            Error::WrongTokenError { token, expected } => Diagnostic {
                range: get_range(token.range()),
                message: format!("Wrong token {:?}, expected one of {:?}", token.t, expected),
                ..Diagnostic::default()
            },
            Error::UnexpectedTokenError { token, .. } => Diagnostic {
                range: get_range(token.range()),
                message: format!("Unexpected token {:?}", token.t),
                ..Diagnostic::default()
            },
            Error::IllegalOffsetType { expr, .. } => Diagnostic {
                range: get_range(expr.range()),
                message: "Illegal offset type".to_owned(),
                ..Diagnostic::default()
            },
            Error::RValueInWriteContext { token, .. } => Diagnostic {
                range: get_range(token.range()),
                message: "Can not use expression in write context".to_owned(),
                ..Diagnostic::default()
            },
        }
    }
}

// Overwrite result
type Result<T, E = Error> = std::result::Result<T, E>;
type ArgumentListResult = Result<Option<Vec<Node>>>;
type ExpressionResult = Result<Node>;
type ExpressionListResult = Result<Vec<Node>>;
type AstResult = Result<(Vec<Node>, Vec<Error>)>;

pub mod arrays;
pub mod calls;
pub mod classes;
pub mod conditionals;
pub mod exception_handling;
pub mod expressions;
pub mod functions;
pub mod keywords;
pub mod loops;
pub mod namespaces;
pub mod types;
pub mod variables;

/// Inspired by https://craftinginterpreters.com/statements-and-state.html
///
/// Parses a token stream of a `Scanner` and generates an Abstract Syntax Tree
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    errors: Vec<Error>,
}

impl Parser {
    /// Parses the entire token stream and returns an abstract syntax tree representation and a vector of
    /// accumulated parse errors.
    ///
    /// # Example
    /// ```
    /// // Content contains the source code
    /// let mut scanner = Scanner::new(&content);
    /// scanner.scan()?;
    /// let Ok(ast, errors) = Parser::ast(scanner.tokens);
    /// ```
    pub fn ast(mut tokens: Vec<Token>) -> AstResult {
        if tokens.is_empty() {
            return Ok((Vec::new(), Vec::new()));
        }

        tokens.reverse();

        let mut parser = Parser {
            tokens,
            errors: Vec::new(),
        };

        let mut statements: Vec<Node> = Vec::new();

        parser.consume_or_err(TokenType::ScriptStart)?;

        while let Some(next) = parser.peek() {
            if next.t == TokenType::Eof {
                break;
            }

            match parser.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    parser.errors.push(error);
                    parser.error_fast_forward();
                }
            }
        }

        Ok((statements, parser.errors))
    }

    /// Fast forwards to the end of the current statement or block instead of simply aborting parsing.
    /// This way at least a partial ast can be returned in the end
    fn error_fast_forward(&mut self) {
        while self.peek().is_some() {
            if self.next_token_one_of(&[
                TokenType::Semicolon,
                TokenType::ScriptStart,
                TokenType::CloseCurly,
            ]) {
                self.next();

                break;
            }

            self.next();
        }
    }

    /// Parses a code block, which basically is a vector of `Node` / statements.
    ///
    /// # Details
    /// ```php
    /// while (true) /* from here */{
    ///    echo "lol";
    /// } /* to here */
    /// ```
    pub fn block(&mut self) -> ExpressionResult {
        let mut statements: Vec<Node> = Vec::new();

        let oc = self.consume(TokenType::OpenCurly)?;

        // TODO: Make sure namespace etc can not pop up here
        while !self.next_token_one_of(&[TokenType::CloseCurly]) && self.peek().is_some() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    self.errors.push(error);
                    self.error_fast_forward();
                }
            }
        }

        let cc = self.consume(TokenType::CloseCurly)?;

        Ok(Node::Block { oc, statements, cc })
    }

    /// Parses an alternative code block, which basically is a vector of `Node` / statements.
    ///
    /// # Details
    /// ```php
    /// while (true) /* from here */:
    ///    echo "lol";
    /// endwhile /* to here */
    /// ```
    pub fn alternative_block(&mut self, expected_terminator: TokenType) -> ExpressionResult {
        let mut statements: Vec<Node> = Vec::new();

        let colon = self.consume(TokenType::Colon)?;

        // TODO: Make sure namespace etc can not pop up here
        while !self.next_token_one_of(&[
            TokenType::EndFor,
            TokenType::EndForeach,
            TokenType::EndWhile,
            TokenType::EndIf,
        ]) && self.peek().is_some()
        {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    self.errors.push(error);
                    self.error_fast_forward();
                }
            }
        }

        let terminator = self.consume(expected_terminator)?;

        Ok(Node::AlternativeBlock {
            colon,
            statements,
            terminator,
        })
    }

    /// Parses sudden appearances of inline HTML.
    ///
    /// # Example
    /// ```php
    /// echo "This is PHP";
    /// ?><h1>HTML</h1><?php
    /// echo "This is PHP ... again!";
    /// ```
    fn inline_html(&mut self) -> ExpressionResult {
        Ok(Node::InlineHtml {
            start: self.consume(TokenType::ScriptEnd)?,
            end: self.consume_or_ignore(TokenType::ScriptStart),
        })
    }

    /// A helper for wrapping the switch between regular statement or alternative block
    fn alternative_block_or_statement(&mut self, terminator: TokenType) -> ExpressionResult {
        if self.next_token_one_of(&[TokenType::Colon]) {
            return self.alternative_block(terminator);
        }

        return self.statement();
    }

    /// Parses a single statement (offloaded depending on which statement was encountered)
    ///
    /// # Details
    /// ```php
    /// /** from here **/
    /// function my_funy (string $a, int $b): ?int {
    ///     echo "Hello!";
    /// }
    /// /** to here **/
    /// ```
    fn statement(&mut self) -> ExpressionResult {
        if let Some(token) = self.peek() {
            match token.t {
                TokenType::ScriptEnd => return self.inline_html(),
                TokenType::ScriptStart => {
                    return Err(Error::UnexpectedTokenError {
                        token: token.clone(),
                    })
                }
                TokenType::Function => return functions::named_function(self),
                TokenType::Namespace => return namespaces::namespace_statement(self),
                TokenType::Use => {
                    let token = self.consume(TokenType::Use)?;

                    if self.consume_or_ignore(TokenType::Function).is_some() {
                        return namespaces::use_function_statement(self, token);
                    }

                    if self.consume_or_ignore(TokenType::Const).is_some() {
                        return namespaces::use_const_statement(self, token);
                    }

                    return namespaces::use_statement(self, token);
                }
                TokenType::Const => return variables::const_statement(self),
                TokenType::Global => return variables::global_variables(self),
                TokenType::Echo => return keywords::echo_statement(self),
                TokenType::Print => return keywords::print_statement(self),
                TokenType::Goto => return keywords::goto_statement(self),
                TokenType::Return => return functions::return_statement(self),
                TokenType::Throw => return exception_handling::throw_statement(self),
                TokenType::Class => return classes::class_statement(self, None, None),
                TokenType::Trait => return classes::trait_statement(self),
                TokenType::Abstract => return classes::abstract_class_statement(self),
                TokenType::Final => return classes::final_class_statement(self),
                TokenType::Interface => return classes::interface(self),
                TokenType::While => return loops::while_statement(self),
                TokenType::Do => return loops::do_while_statement(self),
                TokenType::For => return loops::for_statement(self),
                TokenType::Foreach => return loops::foreach_statement(self),
                TokenType::If => return conditionals::if_statement(self),
                TokenType::OpenCurly => return self.block(),
                TokenType::Switch => return conditionals::switch_statement(self),
                //TokenType::HaltCompiler | TokenType::Die => {
                // Exit
                //}
                TokenType::Semicolon => {
                    return Ok(Node::TokenStatement {
                        token: self.consume(TokenType::Semicolon)?,
                        expr: None,
                    });
                }
                TokenType::Break | TokenType::Continue => {
                    let token = self.next().unwrap();

                    let expr = if self.next_token_one_of(&[TokenType::Semicolon]) {
                        None
                    } else {
                        Some(Box::new(expressions::expression(self)?))
                    };

                    let statement = Node::TokenStatement { token, expr };

                    self.consume_end_of_statement()?;

                    return Ok(statement);
                }
                TokenType::Try => return exception_handling::try_catch_statement(self),
                TokenType::Declare => return keywords::declare_statement(self),
                TokenType::Unset => return keywords::unset_statement(self),
                _ => {
                    if let Some(static_token) = self.consume_or_ignore(TokenType::Static) {
                        if self.next_token_one_of(&[TokenType::Variable]) {
                            return variables::static_variables(self, static_token);
                        } else {
                            // Back on the stack
                            self.tokens.push(static_token);
                        }

                    // Labels
                    } else if let Some(label) = self.consume_or_ignore(TokenType::Identifier) {
                        if let Some(colon) = self.consume_or_ignore(TokenType::Colon) {
                            return Ok(Node::LabelStatement { label, colon });
                        } else {
                            // Back on the stack
                            self.tokens.push(label);
                        }
                    }
                    let expr = expressions::expression_statement(self)?;

                    self.consume_end_of_statement()?;

                    return Ok(expr);
                }
            }
        }

        panic!("Read too far!");
    }

    /// Pop and return the next token
    fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    /// Return the next token without popping it off the stream
    fn peek(&self) -> Option<&Token> {
        if let Some(last) = self.tokens.last() {
            if last.t == TokenType::Eof {
                return None;
            } else {
                return Some(last);
            }
        }

        None
    }

    /// Consumes the end of a statement. This can either be a semicolon or a script end.
    fn consume_end_of_statement(&mut self) -> Result<()> {
        if let Some(token) = self.peek() {
            if token.t == TokenType::Semicolon {
                self.next();
                return Ok(());
            }

            // Implicit end of statement. Return Ok but do not consume as we need to consume it later to record
            // a proper inline html statement
            if token.t == TokenType::ScriptEnd {
                return Ok(());
            }

            return Err(Error::WrongTokenError {
                expected: vec![TokenType::Semicolon, TokenType::ScriptEnd],
                token: token.clone(),
            });
        }

        Ok(())
    }

    /// Consume a token of type `t` or return an Err
    fn consume_or_err(&mut self, t: TokenType) -> Result<()> {
        if let Some(token) = self.peek() {
            if token.t == t {
                self.next();
                return Ok(());
            }

            return Err(Error::WrongTokenError {
                expected: vec![t],
                token: token.clone(),
            });
        }

        panic!("Read too far!");
    }

    /// Consume a Some of token of type `t` or do nothing.
    fn consume_or_ignore(&mut self, t: TokenType) -> Option<Token> {
        if let Some(token) = self.peek() {
            if token.t != t {
                return None;
            }
        } else {
            return None;
        }

        Some(self.next().unwrap())
    }

    /// Consume a token of type `t` or return an error
    fn consume(&mut self, t: TokenType) -> Result<Token> {
        if let Some(token) = self.next() {
            if token.t == t {
                return Ok(token);
            }

            return Err(Error::WrongTokenError {
                expected: vec![t],
                token,
            });
        }

        panic!("Read too far!");
    }

    /// Consume an identifier or return an error
    fn consume_identifier(&mut self) -> Result<Token> {
        if let Some(mut token) = self.next() {
            if token.is_identifier() {
                if token.label.is_none() {
                    // Make sure to put the correct label into the ... label
                    token.label = Some(format!("{}", token));
                }
                return Ok(token);
            }

            return Err(Error::WrongTokenError {
                expected: vec![TokenType::Identifier],
                token,
            });
        }

        panic!("Read too far!");
    }

    /// Consume a potential member of a class / an object or return an error
    fn consume_member(&mut self) -> Result<Token> {
        if let Some(token) = self.next() {
            if token.is_identifier() || token.t == TokenType::Variable {
                return Ok(token);
            }

            return Err(Error::WrongTokenError {
                expected: vec![TokenType::Identifier, TokenType::Variable],
                token,
            });
        }

        panic!("Read too far!");
    }

    // Consume a token of one of the types of `types` or return an error
    fn consume_one_of(&mut self, types: &[TokenType]) -> Result<Token> {
        if let Some(token) = self.next() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Ok(token);
                }
            }

            return Err(Error::WrongTokenError {
                expected: Vec::from(types),
                token,
            });
        }

        panic!("Read too far!");
    }

    /// Consume a token of one of the types of `types` or do nothing
    fn consume_one_of_or_ignore(&mut self, types: &[TokenType]) -> Option<Token> {
        if let Some(token) = self.peek() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Some(self.next().unwrap());
                }
            }
        }

        None
    }

    /// Returns true if the following token is of one of the types of `types`
    fn next_token_one_of(&mut self, types: &[TokenType]) -> bool {
        if let Some(token) = self.peek() {
            for tt in types {
                if *tt == token.t {
                    return true;
                }
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;

    #[test]
    fn test_creates_ast_for_addition() {
        let mut scanner = Scanner::new("<?php\n1 + 2 == 3;");
        scanner.scan().unwrap();
    }
}
