use crate::expression::*;
use crate::statement::*;
use crate::token::{Token, TokenType};

use std::iter::Iterator;
use std::iter::Peekable;
use std::slice::Iter;

type StatementResult = Result<Box<dyn Stmt>, String>;
type ClassStatementResult = Result<Box<dyn Stmt>, String>;
type StatementListResult = Result<Vec<Box<dyn Stmt>>, String>;
type PathExpressionResult = Result<Box<PathExpression>, String>;
type ExpressionResult = Result<Box<dyn Expr>, String>;

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
    pub fn ast(&mut self) -> StatementListResult {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        self.consume_or_err(TokenType::ScriptStart)?;

        while self.tokens.peek().is_some() {
            statements.push(self.statement()?);
        }

        Ok(statements)
    }

    fn block(&mut self) -> StatementListResult {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        while !self.next_token_one_of(&vec![TokenType::CloseCurly]) {
            statements.push(self.statement()?);
        }

        Ok(statements)
    }

    fn statement(&mut self) -> StatementResult {
        if let Some(&token) = self.tokens.peek() {
            match token.t {
                TokenType::Namespace => {
                    self.tokens.next();

                    return self.namespace_statement();
                }
                TokenType::Use => {
                    self.tokens.next();

                    return self.use_statement();
                }
                TokenType::Echo => {
                    self.tokens.next();

                    return self.echo_statement();
                }
                TokenType::Class => {
                    self.tokens.next();
                    return self.class_statement(false);
                }
                TokenType::Abstract => {
                    self.tokens.next();
                    return self.abstract_class_statement();
                }
                _ => {}
            }
        }

        return self.expression_statement();
    }

    // namespace -> "namespace" (block | (path (";" | block)))
    fn namespace_statement(&mut self) -> StatementResult {
        let path = self.path()?;

        // TODO: Implement block
        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(NamespaceStatement::new(path)))
    }

    // use -> "use" ("function" | "const")? path (("{" use_group "}") | ("as" identifier))?
    fn use_statement(&mut self) -> StatementResult {
        let path = self.path()?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(format!("Unexpected EOF"));
        };

        let stmt = match next.t {
            TokenType::As => {
                UseStatement::aliased(path, self.consume_cloned(TokenType::Identifier)?)
            }
            TokenType::OpenCurly => {
                self.tokens.next();
                UseStatement::grouped(path, self.use_group()?)
            }
            TokenType::Semicolon => UseStatement::new(path),
            _ => {
                return Err(format!(
                    "Unexpected {:?} on line {}, col {}",
                    next.t, next.line, next.col
                ));
            }
        };

        // TODO: Implement block
        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(stmt))
    }

    // use_group -> (grouped_use ",")* grouped_use
    fn use_group(&mut self) -> StatementListResult {
        let mut group = Vec::new();

        loop {
            group.push(self.grouped_use_statement()?);

            match self.tokens.next() {
                Some(Token {
                    t: TokenType::Comma,
                    ..
                }) => {
                    continue;
                }
                Some(Token {
                    t: TokenType::CloseCurly,
                    ..
                }) => {
                    break;
                }
                Some(Token { t, line, col, .. }) => {
                    return Err(format!("Unexpected {:?} on line {}, col {}", t, line, col));
                }
                _ => {
                    return Err(format!("Unexpected EOF"));
                }
            }
        }

        Ok(group)
    }

    // grouped_use -> path ("as" identifier)?"
    fn grouped_use_statement(&mut self) -> StatementResult {
        let path = self.path()?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(String::from("Unexpected EOF"));
        };

        let stmt = match next.t {
            TokenType::As => {
                self.tokens.next();
                UseStatement::aliased(path, self.consume_cloned(TokenType::Identifier)?)
            }
            _ => UseStatement::new(path),
        };

        Ok(Box::new(stmt))
    }

    fn echo_statement(&mut self) -> StatementResult {
        let value = self.expression()?;

        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(EchoStatement::new(value)))
    }

    // abstract_class -> "abstract" class
    fn abstract_class_statement(&mut self) -> ClassStatementResult {
        self.tokens.next();

        self.class_statement(true)
    }

    // class -> "class" identifier (extends identifier_list)? (implements identifier_list)?
    fn class_statement(&mut self, is_abstract: bool) -> ClassStatementResult {
        let name = self.consume_cloned(TokenType::Identifier)?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(format!("Unexpected EOF"));
        };

        let mut extends = None;
        if next.t == TokenType::Extends {
            self.tokens.next();
            extends = Some(self.identifier_list()?);
        }

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(format!("Unexpected EOF"));
        };

        let mut implements = None;
        if next.t == TokenType::Implements {
            self.tokens.next();
            implements = Some(self.identifier_list()?);
        }

        self.consume_or_err(TokenType::OpenCurly);
        let block = self.block()?;
        self.consume_or_err(TokenType::CloseCurly);

        Ok(Box::new(ClassStatement::new(
            name,
            is_abstract,
            extends,
            implements,
            block,
        )))
    }

    // (("extends" identifier) (, "extends" identifier)*)?
    // (("implements" identifier) (, "implements" identifier)*)?
    fn identifier_list(&mut self) -> Result<Vec<Token>, String> {
        let mut extends = Vec::new();

        loop {
            extends.push(self.consume_cloned(TokenType::Identifier)?);

            if !self.next_token_one_of(&vec![TokenType::Comma]) {
                break;
            }

            self.tokens.next();
        }

        Ok(extends)
    }

    fn expression_statement(&mut self) -> StatementResult {
        let value = self.expression()?;

        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(ExpressionStatement::new(value)))
    }

    fn expression(&mut self) -> ExpressionResult {
        self.equality()
    }

    // path -> identifier ("\" identifier)*
    fn path(&mut self) -> PathExpressionResult {
        let mut path = vec![self.consume_cloned(TokenType::Identifier)?];

        let matches_ns = vec![TokenType::NamespaceSeparator];
        let matches_ident = vec![TokenType::Identifier];

        loop {
            // Next is another namespace separator
            if !self.next_token_one_of(&matches_ns) {
                break;
            }

            self.consume_or_err(TokenType::NamespaceSeparator)?;

            if !self.next_token_one_of(&matches_ident) {
                break;
            }

            path.push(self.consume_cloned(TokenType::Identifier)?);
        }

        Ok(Box::new(PathExpression::new(path)))
    }

    fn equality(&mut self) -> ExpressionResult {
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

    fn comparison(&mut self) -> ExpressionResult {
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

    fn addition(&mut self) -> ExpressionResult {
        let mut expr = self.multiplication()?;

        let potential_matches = vec![TokenType::Minus, TokenType::Plus];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.multiplication()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ExpressionResult {
        let mut expr = self.unary()?;

        let potential_matches = vec![TokenType::Multiplication, TokenType::Division];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.unary()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ExpressionResult {
        if self.next_token_one_of(&vec![TokenType::Negation, TokenType::Minus]) {
            let next = self.tokens.next().unwrap();
            let right = self.unary()?;

            return Ok(Box::new(Unary::new(next.clone(), right)));
        }

        self.primary()
    }

    fn primary(&mut self) -> ExpressionResult {
        if self.next_token_one_of(&vec![
            TokenType::False,
            TokenType::True,
            TokenType::Null,
            TokenType::LongNumber,
            TokenType::DecimalNumber,
            TokenType::ConstantEncapsedString,
            TokenType::EncapsedAndWhitespaceString,
        ]) {
            return Ok(Box::new(Literal::new(self.tokens.next().unwrap().clone())));
        }

        if self.next_token_one_of(&vec![TokenType::OpenParenthesis]) {
            self.consume_or_err(TokenType::OpenParenthesis)?;
            let expr = self.expression()?;
            self.consume_or_err(TokenType::CloseParenthesis)?;

            return Ok(Box::new(Grouping::new(expr)));
        }

        let next = self.tokens.peek().unwrap();
        Err(format!("Unsupported primary {:?}", next))
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
