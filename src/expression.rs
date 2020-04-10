use crate::token::{Token, TokenType};

use std::fmt;

pub trait Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
    fn token_type(&self) -> Option<TokenType>;
}

impl fmt::Debug for dyn Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f)
    }
}

pub struct Unary {
    right: Box<dyn Expr>,
    operator: Token,
}

impl Unary {
    pub fn new(operator: Token, right: Box<dyn Expr>) -> Self {
        Self { operator, right }
    }
}

impl Expr for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Unary")
            .field("operator", &self.operator)
            .field("right", &self.right)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }
}

pub struct PostUnary {
    left: Box<dyn Expr>,
    operator: Token,
}

impl PostUnary {
    pub fn new(operator: Token, left: Box<dyn Expr>) -> Self {
        Self { operator, left }
    }
}

impl Expr for PostUnary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PostUnary")
            .field("operator", &self.operator)
            .field("left", &self.left)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }
}

pub struct Binary {
    left: Box<dyn Expr>,
    right: Box<dyn Expr>,
    operator: Token,
}

impl Binary {
    pub fn new(left: Box<dyn Expr>, operator: Token, right: Box<dyn Expr>) -> Self {
        Self {
            operator,
            left,
            right,
        }
    }
}

impl Expr for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Binary")
            .field("left", &self.left)
            .field("operator", &self.operator)
            .field("right", &self.right)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }
}

pub struct Literal {
    value: Token,
}

impl Literal {
    pub fn new(value: Token) -> Self {
        Self { value }
    }
}

impl Expr for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Literal")
            .field("value", &self.value)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        Some(self.value.t.clone())
    }
}

pub struct Grouping {
    expr: Box<dyn Expr>,
}

impl Grouping {
    pub fn new(expr: Box<dyn Expr>) -> Self {
        Self { expr }
    }
}

impl Expr for Grouping {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Grouping")
            .field("expr", &self.expr)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }
}

pub struct PathExpression {
    absolute: bool,
    path: Vec<Token>,
}

impl PathExpression {
    pub fn new(absolute: bool, path: Vec<Token>) -> Self {
        Self { absolute, path }
    }
}

impl fmt::Debug for PathExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Path")
            .field("path", &self.path)
            .field("absolute", &self.absolute)
            .finish()
    }
}

pub struct Assignment {
    name: Box<dyn Expr>,
    value: Box<dyn Expr>,
}

impl Assignment {
    pub fn new(name: Box<dyn Expr>, value: Box<dyn Expr>) -> Self {
        Self { name, value }
    }
}

impl Expr for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Assignment")
            .field("name", &self.name)
            .field("value", &self.value)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }
}
