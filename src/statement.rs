use crate::expression::Expr;

use std::fmt;

pub trait Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl fmt::Debug for dyn Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f)
    }
}

pub struct ExpressionStatement {
    expression: Box<dyn Expr>
}

impl ExpressionStatement {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Self {
            expression
        }
    }
}

impl Stmt for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Expression")
        .field("expression", &self.expression)
        .finish()
    }
}

pub struct EchoStatement {
    expression: Box<dyn Expr>
}

impl EchoStatement {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Self {
            expression
        }
    }
}

impl Stmt for EchoStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Echo")
        .field("expression", &self.expression)
        .finish()
    }
}
