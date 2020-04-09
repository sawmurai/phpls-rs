use crate::expression::*;
use crate::statement::*;
use crate::token::{Token, TokenType};

use std::iter::Iterator;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

/// Inspired by https://craftinginterpreters.com/statements-and-state.html
impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
        }
    }

    /// Parses the entire source and return an abstract syntax tree representation
    pub fn ast(&mut self) -> Result<Vec<Box<dyn Stmt>>, String> {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        self.consume_or_err(TokenType::ScriptStart)?;

        while self.tokens.peek().is_some() {
            statements.push(self.top_statement()?);
        }

        Ok(statements)
    }

    fn top_statement(&mut self) -> Result<Box<dyn Stmt>, String> {
        if let Some(&token) = self.tokens.peek() {
            match token.t {
                TokenType::Namespace => {
                    self.tokens.next();

                    return self.namespace_statement();
                },
                TokenType::Use => {
                    self.tokens.next();

                    return self.use_statement();
                },
                TokenType::Echo => {
                    self.tokens.next();

                    return self.echo_statement();
                },
                _ => {}
            }       
        }

        return self.expression_statement();
    }

    fn statement(&mut self) -> Result<Box<dyn Stmt>, String> {
        if let Some(&token) = self.tokens.peek() {
            match token.t {
                TokenType::Echo => {
                    self.tokens.next();

                    return self.echo_statement();
                },
                _ => {}
            }       
        }

        return self.expression_statement();
    }

    // namespace -> "namespace" (block | (path (";" | block)))
    fn namespace_statement(&mut self) ->  Result<Box<dyn Stmt>, String> {
        let path = self.path()?;

        // TODO: Implement block
        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(NamespaceStatement::new(path)))
    }

    // use -> "use" ("function" | "const")? ( path ("as" identifier)? )+
    fn use_statement(&mut self) ->  Result<Box<dyn Stmt>, String> {
        let path = self.path()?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(String::from("Unexpected EOF"));
        };

        let stmt = match next.t {
            TokenType::As => {
                UseStatement::aliased(path, self.consume_cloned(TokenType::Identifier))
            },
            TokenType::OpenCurly => {
                UseStatement::grouped(path, self.path_list())
            },
            TokenType::Semicolon => {
                UseStatement::new(path)
            }

            Ok(Box::new(stmt))
        };

        // TODO: Implement block
        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(UseStatement::new(path)))
    }

    fn echo_statement(&mut self) -> Result<Box<dyn Stmt>, String> {
        let value = self.expression()?;

        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(EchoStatement::new(value)))
    }

    fn expression_statement(&mut self) -> Result<Box<dyn Stmt>, String> {
        let value = self.expression()?;

        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(ExpressionStatement::new(value)))
    }

    fn expression(&mut self) -> Result<Box<dyn Expr>, String> {
        self.equality()
    }

    // path -> identifier ("\" identifier)* 
    fn path(&mut self) -> Result<Box<dyn Expr>, String> {
        let mut path = vec![self.consume_cloned(TokenType::Identifier)?];

        let potential_matches = vec![TokenType::NamespaceSeparator];

        while self.next_token_one_of(&potential_matches) { 
            self.tokens.next();
            
            path.push(self.consume_cloned(TokenType::Identifier)?);
        }

        Ok(Box::new(PathExpression::new(path)))
    }

    fn equality(&mut self) -> Result<Box<dyn Expr>, String> {
        let mut expr = self.comparison()?;

        let potential_matches = vec![TokenType::IsNotEqual, TokenType::IsEqual];
        // Todo: Make this an if and force && or || in between. Basically add a new non-terminal
        while self.next_token_one_of(&potential_matches) {
            // Unwrap should be fine since the condition already checks that there is a next element
            let next = self.tokens.next().unwrap();
            let right = self.comparison()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Box<dyn Expr>, String> {
        let mut expr = self.addition()?;

        let potential_matches = vec![
            TokenType::Greater,
            TokenType::GreaterOrEqual,
            TokenType::Smaller,
            TokenType::SmallerOrEqual,
        ];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.addition()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Box<dyn Expr>, String> {
        let mut expr = self.multiplication()?;

        let potential_matches = vec![TokenType::Minus, TokenType::Plus];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.multiplication()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Box<dyn Expr>, String> {
        let mut expr = self.unary()?;

        let potential_matches = vec![TokenType::Multiplication, TokenType::Division];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.unary()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Box<dyn Expr>, String> {
        if self.next_token_one_of(&vec![TokenType::Negation, TokenType::Minus]) {
            let next = self.tokens.next().unwrap();
            let right = self.unary()?;

            return Ok(Box::new(Unary::new(next.clone(), right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Box<dyn Expr>, String> {
        if self.next_token_one_of(&vec![
            TokenType::False,
            TokenType::True,
            TokenType::Null,
            TokenType::LongNumber,
            TokenType::DecimalNumber,
            TokenType::ConstantEncapsedString,
            TokenType::EncapsedAndWhitespaceString
        ]) {
            return Ok(Box::new(Literal::new(self.tokens.next().unwrap().clone())));
        }

        if self.next_token_one_of(&vec![TokenType::OpenParenthesis]) {
            self.consume_or_err(TokenType::OpenParenthesis)?;
            let expr = self.expression()?;
            self.consume_or_err(TokenType::CloseParenthesis)?;

            return Ok(Box::new(Grouping::new(expr)));
        }

        Err(String::from("unsupported primary"))
    }

    fn consume_or_err(&mut self, t: TokenType) -> Result<(), String> {
        if let Some(token) = self.tokens.next() {
            if token.t == t {
                return Ok(());
            }

            return Err(format!(
                "Expected {:?}, found {:?} on line {}, col {}",
                t, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected {:?}, found end of file.", t))
    }

    fn consume_cloned(&mut self, t: TokenType) -> Result<Token, String> {
        if let Some(token) = self.tokens.next() {
            if token.t == t {
                return Ok(token.clone());
            }

            return Err(format!(
                "Expected {:?}, found {:?} on line {}, col {}",
                t, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected {:?}, found end of file.", t))
    }

    fn next_token_one_of(&mut self, types: &Vec<TokenType>) -> bool {
        if let Some(&token) = self.tokens.peek() {
            for tt in types {
                if *tt == token.t {
                    return true;
                }
            }
        }

        return false;
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

        let mut parser = Parser::new(&scanner.tokens);

        println!("{:?}", parser.ast());
    }
}
