use crate::token::{Token, TokenType};

use std::fmt;

pub trait Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
    fn token_type(&self) -> Option<TokenType>;

    /// Line in which the expression starts, usually the line of its first token
    fn line(&self) -> u16;

    /// Column in which the expression starts, usually the col of its first token
    fn col(&self) -> u16;

    /// Tells if the expression can be used as offset
    fn is_offset(&self) -> bool;
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

    fn line(&self) -> u16 {
        self.operator.line
    }

    fn col(&self) -> u16 {
        self.operator.col
    }

    fn is_offset(&self) -> bool {
        true
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

    fn line(&self) -> u16 {
        self.operator.line
    }

    fn col(&self) -> u16 {
        self.operator.col
    }

    fn is_offset(&self) -> bool {
        true
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

    fn line(&self) -> u16 {
        self.operator.line
    }

    fn col(&self) -> u16 {
        self.operator.col
    }

    fn is_offset(&self) -> bool {
        true
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

    fn line(&self) -> u16 {
        self.value.line
    }

    fn col(&self) -> u16 {
        self.value.col
    }

    fn is_offset(&self) -> bool {
        true
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

    fn line(&self) -> u16 {
        self.expr.line()
    }

    fn col(&self) -> u16 {
        self.expr.col()
    }

    fn is_offset(&self) -> bool {
        self.expr.is_offset()
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
    // TODO: Think about having the operator here to be able to provide its location
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

    fn line(&self) -> u16 {
        self.name.line()
    }

    fn col(&self) -> u16 {
        self.name.col()
    }

    fn is_offset(&self) -> bool {
        self.value.is_offset()
    }
}

pub struct ArrayPair {
    key: Option<Box<dyn Expr>>,
    value: Box<dyn Expr>,
}

impl ArrayPair {
    pub fn new(key: Option<Box<dyn Expr>>, value: Box<dyn Expr>) -> Self {
        Self { key, value }
    }
}

impl Expr for ArrayPair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("array-pair")
            .field("key", &self.key)
            .field("value", &self.value)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        if self.key.is_some() {
            self.key.as_ref().unwrap().line()
        } else {
            self.value.line()
        }
    }

    fn col(&self) -> u16 {
        if self.key.is_some() {
            self.key.as_ref().unwrap().col()
        } else {
            self.value.col()
        }
    }

    fn is_offset(&self) -> bool {
        false
    }
}

pub struct Array {
    start: Token,
    elements: Vec<Box<dyn Expr>>,
    end: Token,
}

impl Array {
    pub fn new(start: Token, elements: Vec<Box<dyn Expr>>, end: Token) -> Self {
        Self {
            start,
            elements,
            end,
        }
    }
}

impl Expr for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("array")
            .field("start", &self.start)
            .field("elements", &self.elements)
            .field("end", &self.end)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.start.line
    }

    fn col(&self) -> u16 {
        self.start.col
    }
    fn is_offset(&self) -> bool {
        false
    }
}
