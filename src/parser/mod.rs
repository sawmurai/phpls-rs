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
    pub fn errors(&self) -> &Vec<String> {
        self.errors.as_ref()
    }

    /// Fast forwards to the end of the current statement or block
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

    fn inline_html(&mut self) -> StatementResult {
        let start = self.consume_cloned(TokenType::ScriptEnd)?;

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
                        self.consume_cloned(TokenType::Semicolon)?,
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

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.last()
    }

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

    fn consume_cloned(&mut self, t: TokenType) -> Result<Token, String> {
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

    fn consume_identifier_cloned(&mut self) -> Result<Token, String> {
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

    fn consume_member_cloned(&mut self) -> Result<Token, String> {
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

    fn consume_one_of_cloned(&mut self, types: &[TokenType]) -> Result<Token, String> {
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

    fn consume_one_of_cloned_or_ignore(&mut self, types: &[TokenType]) -> Option<Token> {
        if let Some(token) = self.peek() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Some(self.next().unwrap());
                }
            }
        }

        None
    }

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
