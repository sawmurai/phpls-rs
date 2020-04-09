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

pub struct NamespaceStatement {
    expression: Box<dyn Expr>
}

impl NamespaceStatement {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Self {
            expression
        }
    }
}

impl Stmt for NamespaceStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Namespace")
        .field("expression", &self.expression)
        .finish()
    }
}

pub struct UseStatement {
    expression: Box<dyn Expr>
}

impl UseStatement {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Self {
            expression
        }
    }
}

impl Stmt for UseStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Use")
        .field("expression", &self.expression)
        .finish()
    }
}
