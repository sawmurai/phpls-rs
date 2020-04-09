use crate::token::Token;

use std::fmt;

pub trait Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
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
}

pub struct PathExpression {
    path: Vec<Token>,
}

impl PathExpression {
    pub fn new(path: Vec<Token>) -> Self {
        Self { path }
    }
}

impl fmt::Debug for PathExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Path").field("path", &self.path).finish()
    }
}
