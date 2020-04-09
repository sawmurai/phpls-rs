use crate::expression::{Expr, PathExpression};
use crate::token::{Token, TokenType};

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
    expression: Box<dyn Expr>,
}

impl ExpressionStatement {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Self { expression }
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
    expression: Box<dyn Expr>,
}

impl EchoStatement {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Self { expression }
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
    expression: Box<PathExpression>,
}

impl NamespaceStatement {
    pub fn new(expression: Box<PathExpression>) -> Self {
        Self { expression }
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
    path: Box<PathExpression>,
    alias: Option<Token>,
    group: Option<Vec<Box<dyn Stmt>>>,
}

impl UseStatement {
    pub fn new(path: Box<PathExpression>) -> Self {
        Self {
            path,
            alias: None,
            group: None,
        }
    }

    pub fn aliased(path: Box<PathExpression>, alias: Token) -> Self {
        Self {
            path,
            alias: Some(alias),
            group: None,
        }
    }

    pub fn grouped(path: Box<PathExpression>, group: Vec<Box<dyn Stmt>>) -> Self {
        Self {
            path,
            alias: None,
            group: Some(group),
        }
    }
}

impl Stmt for UseStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Use")
            .field("path", &self.path)
            .field("alias", &self.alias)
            .field("group", &self.group)
            .finish()
    }
}

pub struct ClassStatement {
    name: Token,
    is_abstract: bool,
    implements: Option<Vec<Token>>,
    extends: Option<Vec<Token>>,
    body: Vec<Box<dyn Stmt>>,
}

impl ClassStatement {
    pub fn new(
        name: Token,
        is_abstract: bool,
        implements: Option<Vec<Token>>,
        extends: Option<Vec<Token>>,
        body: Vec<Box<dyn Stmt>>,
    ) -> Self {
        Self {
            name,
            is_abstract,
            implements,
            extends,
            body,
        }
    }
}

impl Stmt for ClassStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            .field("is_abstract", &self.is_abstract)
            .field("implements", &self.implements)
            .field("extends", &self.extends)
            .field("body", &self.body)
            .finish()
    }
}

pub struct ClassConstantDefinitionStatement {
    name: Token,
    visibility: Option<TokenType>,
    value: Box<dyn Expr>,
}

impl ClassConstantDefinitionStatement {
    pub fn new(name: Token, visibility: Option<TokenType>, value: Box<dyn Expr>) -> Self {
        Self {
            name,
            visibility,
            value,
        }
    }
}

impl Stmt for ClassConstantDefinitionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("class constant")
            .field("name", &self.name)
            .field("visibility", &self.visibility)
            .field("value", &self.value)
            .finish()
    }
}

pub struct PropertyDefinitionStatement {
    name: Token,
    visibility: Option<TokenType>,
    value: Option<Box<dyn Expr>>,
    is_static: bool,
}

impl PropertyDefinitionStatement {
    pub fn new(
        name: Token,
        visibility: Option<TokenType>,
        value: Option<Box<dyn Expr>>,
        is_static: bool,
    ) -> Self {
        Self {
            name,
            visibility,
            value,
            is_static,
        }
    }
}

impl Stmt for PropertyDefinitionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("property")
            .field("name", &self.name)
            .field("visibility", &self.visibility)
            .field("value", &self.value)
            .field("is_static", &self.is_static)
            .finish()
    }
}

pub struct MethodDefinitionStatement {
    name: Token,
    visibility: Option<TokenType>,
    function: Box<dyn Stmt>,
    is_static: bool,
}

impl MethodDefinitionStatement {
    pub fn new(
        name: Token,
        visibility: Option<TokenType>,
        function: Box<FunctionDefinitionStatement>,
        is_static: bool,
    ) -> Self {
        Self {
            name,
            visibility,
            function,
            is_static,
        }
    }
}

impl Stmt for MethodDefinitionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("method")
            .field("name", &self.name)
            .field("visibility", &self.visibility)
            .field("function", &self.function)
            .field("is_static", &self.is_static)
            .finish()
    }
}

pub struct FunctionDefinitionStatement {
    arguments: Option<Vec<FunctionArgument>>,
    return_type: Option<ReturnType>,
    body: Vec<Box<dyn Stmt>>,
}

impl FunctionDefinitionStatement {
    pub fn new(
        arguments: Option<Vec<FunctionArgument>>,
        return_type: Option<ReturnType>,
        body: Vec<Box<dyn Stmt>>,
    ) -> Self {
        Self {
            arguments,
            return_type,
            body,
        }
    }
}

impl Stmt for FunctionDefinitionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("function")
            .field("arguments", &self.arguments)
            .field("return_type", &self.return_type)
            .field("body", &self.body)
            .finish()
    }
}

#[derive(Debug)]
pub struct FunctionArgument {
    t: Option<Token>,
    name: Token,
    nullable: bool,
    default: Option<Token>,
}

impl FunctionArgument {
    pub fn new(t: Option<Token>, name: Token, nullable: bool, default: Option<Token>) -> Self {
        Self {
            t,
            name,
            nullable,
            default,
        }
    }
}

#[derive(Debug)]
pub struct ReturnType {
    t: Token,
    nullable: bool,
}

impl ReturnType {
    pub fn new(t: Token, nullable: bool) -> Self {
        Self { t, nullable }
    }
}
