use crate::expression::*;
use crate::statement::*;
use crate::token::{Token, TokenType};

type StatementResult = Result<Box<dyn Stmt>, String>;
type StatementListResult = Result<Vec<Box<dyn Stmt>>, String>;
type ArgumentListResult = Result<Option<Vec<Node>>, String>;
type ExpressionResult = Result<Node, String>;
type AstResult = Result<(Vec<Box<dyn Stmt>>, Vec<String>), String>;

pub mod arrays;
pub mod calls;
pub mod classes;
pub mod conditionals;
pub mod declarations;
pub mod exception_handling;
pub mod expressions;
pub mod functions;
pub mod keywords;
pub mod loops;
pub mod variables;

/// Inspired by https://craftinginterpreters.com/statements-and-state.html
///
/// Parses a token stream of a `Scanner` and generates an Abstract Syntax Tree
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    errors: Vec<String>,
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
        tokens.reverse();

        let mut parser = Parser {
            tokens,
            errors: Vec::new(),
        };

        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        parser.consume_or_err(TokenType::ScriptStart)?;

        while parser.peek().is_some() {
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
            self.next();

            if self.next_token_one_of(&[TokenType::Semicolon]) {
                self.next();

                break;
            }

            if self.next_token_one_of(&[TokenType::CloseCurly]) {
                self.next();
                break;
            }
        }
    }

    /// Parses a code block, which basically is a vector of `dyn Stmt` / statements.
    /// It expects to already be past the `{` and it will read until it encounters a `}`
    ///
    /// # Details
    /// ```php
    /// while (true) {
    /// // Parse here
    /// }
    /// ```
    pub fn block(&mut self) -> Result<Box<dyn Stmt>, String> {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        self.consume_or_err(TokenType::OpenCurly)?;

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

        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(Block::new(statements)))
    }

    /// Parses sudden appearances of inline HTML.
    ///
    /// # Example
    /// ```php
    /// echo "This is PHP";
    /// ?><h1>HTML</h1><?php
    /// echo "This is PHP ... again!";
    /// ```
    fn inline_html(&mut self) -> StatementResult {
        let start = self.consume(TokenType::ScriptEnd)?;

        Ok(Box::new(InlineHtml::new(
            start,
            self.consume_or_ignore(TokenType::ScriptStart),
        )))
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
    fn statement(&mut self) -> StatementResult {
        if let Some(token) = self.peek() {
            match token.t {
                TokenType::ScriptEnd => {
                    return self.inline_html();
                }
                TokenType::Function => {
                    self.next();

                    return functions::named_function(self);
                }
                TokenType::Namespace => {
                    self.next();

                    return declarations::namespace_statement(self);
                }
                TokenType::Use => {
                    self.next();

                    if self.consume_or_ignore(TokenType::Function).is_some() {
                        return declarations::use_function_statement(self);
                    }

                    if self.consume_or_ignore(TokenType::Const).is_some() {
                        return declarations::use_const_statement(self);
                    }

                    return declarations::use_statement(self);
                }
                TokenType::Const => {
                    self.next();

                    return variables::const_statement(self);
                }
                TokenType::Global => {
                    self.next();

                    return variables::global_variables(self);
                }
                TokenType::Echo => {
                    self.next();

                    return keywords::echo_statement(self);
                }
                TokenType::Print => {
                    self.next();

                    return keywords::print_statement(self);
                }
                TokenType::Goto => {
                    self.next();

                    return keywords::goto_statement(self);
                }
                TokenType::Return => {
                    self.next();

                    return functions::return_statement(self);
                }
                TokenType::Throw => {
                    self.next();

                    return exception_handling::throw_statement(self);
                }
                TokenType::Class => {
                    self.next();
                    return classes::class_statement(self, false, false);
                }
                TokenType::Trait => {
                    self.next();
                    return classes::trait_statement(self);
                }
                TokenType::Abstract => {
                    self.next();
                    return classes::abstract_class_statement(self);
                }
                TokenType::Final => {
                    self.next();
                    return classes::final_class_statement(self);
                }
                TokenType::Interface => {
                    self.next();
                    return classes::interface(self);
                }
                TokenType::While => {
                    self.next();
                    return loops::while_statement(self);
                }
                TokenType::Do => {
                    self.next();
                    return loops::do_while_statement(self);
                }
                TokenType::For => {
                    self.next();
                    return loops::for_statement(self);
                }
                TokenType::Foreach => {
                    self.next();
                    return loops::foreach_statement(self);
                }
                TokenType::If => {
                    self.next();
                    return conditionals::if_statement(self);
                }
                TokenType::OpenCurly => {
                    let block = self.block();

                    return block;
                }
                TokenType::Switch => {
                    self.next();
                    return conditionals::switch_statement(self);
                }
                TokenType::Semicolon => {
                    return Ok(Box::new(TokenStatement::new(
                        self.consume(TokenType::Semicolon)?,
                        None,
                    )));
                }
                TokenType::Break | TokenType::Continue => {
                    let token = self.next().unwrap();

                    let expr = if self.next_token_one_of(&[TokenType::Semicolon]) {
                        None
                    } else {
                        Some(expressions::expression(self)?)
                    };

                    let statement = Box::new(TokenStatement::new(token, expr));

                    self.consume_end_of_statement()?;

                    return Ok(statement);
                }
                TokenType::Try => {
                    self.next();
                    return exception_handling::try_catch_statement(self);
                }
                TokenType::Declare => {
                    return keywords::declare_statement(self);
                }
                TokenType::Unset => {
                    return keywords::unset_statement(self);
                }
                _ => {
                    if let Some(static_token) = self.consume_or_ignore(TokenType::Static) {
                        if self.next_token_one_of(&[TokenType::Variable]) {
                            return variables::static_variables(self);
                        } else {
                            // Back on the stack
                            self.tokens.push(static_token);
                        }

                    // Labels
                    } else if let Some(identifier) = self.consume_or_ignore(TokenType::Identifier) {
                        if let Some(colon) = self.consume_or_ignore(TokenType::Colon) {
                            return Ok(Box::new(LabelStatement::new(identifier, colon)));
                        } else {
                            // Back on the stack
                            self.tokens.push(identifier);
                        }
                    }
                    let expr = expressions::expression_statement(self)?;

                    self.consume_end_of_statement()?;

                    return Ok(expr);
                }
            }
        }

        Err(String::from("Unexpected EOF!"))
    }

    /// Pop and return the next token
    fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    /// Return the next token without popping it off the stream
    fn peek(&self) -> Option<&Token> {
        self.tokens.last()
    }

    /// Consumes the end of a statement. This can either be a semicolon or a script end.
    fn consume_end_of_statement(&mut self) -> Result<(), String> {
        if let Some(token) = self.peek() {
            if token.t == TokenType::Semicolon || token.t == TokenType::ScriptEnd {
                self.next();
                return Ok(());
            }

            return Err(format!(
                "Expected end of statement, found {:?} on line {}, col {}",
                token.t, token.line, token.col
            ));
        }

        Ok(())
    }

    /// Consume a token of type `t` or return an Err
    fn consume_or_err(&mut self, t: TokenType) -> Result<(), String> {
        if let Some(token) = self.peek() {
            if token.t == t {
                self.next();
                return Ok(());
            }

            return Err(format!(
                "Expected {:?}, found {:?} on line {}, col {}",
                t, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected {:?}, found end of file.", t))
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
    fn consume(&mut self, t: TokenType) -> Result<Token, String> {
        if let Some(token) = self.next() {
            if token.t == t {
                return Ok(token);
            }

            return Err(format!(
                "Expected {:?}, found {:?} on line {}, col {}",
                t, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected {:?}, found end of file.", t))
    }

    /// Consume an identifier or return an error
    fn consume_identifier(&mut self) -> Result<Token, String> {
        if let Some(token) = self.next() {
            if token.is_identifier() {
                return Ok(token);
            }

            return Err(format!(
                "Expected Identifier, found {:?} on line {}, col {}",
                token.t, token.line, token.col
            ));
        }

        Err("Expected Identifier, found end of file.".to_string())
    }

    /// Consume a potential member of a class / an object or return an error
    fn consume_member(&mut self) -> Result<Token, String> {
        if let Some(token) = self.next() {
            if token.is_identifier() || token.t == TokenType::Variable {
                return Ok(token);
            }

            return Err(format!(
                "Expected Identifier or Variable, found {:?} on line {}, col {}",
                token.t, token.line, token.col
            ));
        }

        Err("Expected Identifier, found end of file.".to_string())
    }

    // Consume a token of one of the types of `types` or return an error
    fn consume_one_of(&mut self, types: &[TokenType]) -> Result<Token, String> {
        if let Some(token) = self.next() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Ok(token);
                }
            }

            return Err(format!(
                "Expected one of {:?}, found {:?} on line {}, col {}",
                types, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected one of {:?}, found end of file.", types))
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
    use super::*;
    use crate::scanner::Scanner;

    #[test]
    fn test_creates_ast_for_addition() {
        let mut scanner = Scanner::new("<?php\n1 + 2 == 3;");
        scanner.scan().unwrap();

        println!("{:?}", Parser::ast(scanner.tokens));
    }
}
