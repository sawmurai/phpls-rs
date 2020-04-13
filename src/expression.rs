use crate::statement::{FunctionArgument, ReturnType, Stmt};
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

    /// Tells if the expression can have values assigned
    fn is_lvalue(&self) -> bool;
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
        false
    }

    fn is_lvalue(&self) -> bool {
        false
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

    fn is_lvalue(&self) -> bool {
        false
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

    fn is_lvalue(&self) -> bool {
        false
    }
}

pub struct Ternary {
    condition: Box<dyn Expr>,
    true_case: Option<Box<dyn Expr>>,
    false_case: Box<dyn Expr>,
}

impl Ternary {
    pub fn new(
        condition: Box<dyn Expr>,
        true_case: Option<Box<dyn Expr>>,
        false_case: Box<dyn Expr>,
    ) -> Self {
        Self {
            condition,
            true_case,
            false_case,
        }
    }
}

impl Expr for Ternary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Ternary")
            .field("condition", &self.condition)
            .field("true_case", &self.true_case)
            .field("false_case", &self.false_case)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.condition.line()
    }

    fn col(&self) -> u16 {
        self.condition.col()
    }

    fn is_offset(&self) -> bool {
        true
    }

    fn is_lvalue(&self) -> bool {
        false
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

    fn is_lvalue(&self) -> bool {
        self.value.t == TokenType::Variable
    }
}

pub struct LexicalVariable {
    by_ref: Option<Token>,
    value: Token,
}

impl LexicalVariable {
    pub fn new(by_ref: Option<Token>, value: Token) -> Self {
        Self { by_ref, value }
    }
}

impl Expr for LexicalVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("lexical variable")
            .field("by_ref", &self.by_ref)
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

    fn is_lvalue(&self) -> bool {
        false
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

    fn is_lvalue(&self) -> bool {
        false
    }
}

pub struct PathExpression {
    start: Token,
    absolute: bool,
    path: Vec<Token>,
}

impl PathExpression {
    pub fn new(start: Token, absolute: bool, path: Vec<Token>) -> Self {
        Self {
            start,
            absolute,
            path,
        }
    }
}

impl Expr for PathExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Path")
            .field("path", &self.path)
            .field("absolute", &self.absolute)
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

    fn is_lvalue(&self) -> bool {
        false
    }
}

pub struct Assignment {
    name: Box<dyn Expr>,
    operator: Token,
    value: Box<dyn Expr>,
    // TODO: Think about having the operator here to be able to provide its location
}

impl Assignment {
    pub fn new(name: Box<dyn Expr>, operator: Token, value: Box<dyn Expr>) -> Self {
        Self {
            name,
            operator,
            value,
        }
    }
}

impl Expr for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Assignment")
            .field("name", &self.name)
            .field("operator", &self.operator)
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

    fn is_lvalue(&self) -> bool {
        false
    }
}

pub struct ArrayPair {
    key: Option<Box<dyn Expr>>,
    value: Box<dyn Expr>,
    by_ref: Option<Token>,
}

impl ArrayPair {
    pub fn new(key: Option<Box<dyn Expr>>, value: Box<dyn Expr>, by_ref: Option<Token>) -> Self {
        Self { key, value, by_ref }
    }
}

impl Expr for ArrayPair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("array-pair")
            .field("key", &self.key)
            .field("value", &self.value)
            .field("by_ref", &self.by_ref)
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

    fn is_lvalue(&self) -> bool {
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

    // True for destructuring
    fn is_lvalue(&self) -> bool {
        true
    }
}

pub struct List {
    start: Token,
    elements: Vec<Box<dyn Expr>>,
    end: Token,
}

impl List {
    pub fn new(start: Token, elements: Vec<Box<dyn Expr>>, end: Token) -> Self {
        Self {
            start,
            elements,
            end,
        }
    }
}

impl Expr for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("list")
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
        true
    }

    // True for destructuring
    fn is_lvalue(&self) -> bool {
        true
    }
}

pub struct OldArray {
    start: Token,
    elements: Vec<Box<dyn Expr>>,
    end: Token,
}

impl OldArray {
    pub fn new(start: Token, elements: Vec<Box<dyn Expr>>, end: Token) -> Self {
        Self {
            start,
            elements,
            end,
        }
    }
}

impl Expr for OldArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("old-array")
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

    // True for destructuring
    fn is_lvalue(&self) -> bool {
        true
    }
}

pub struct Call {
    callee: Box<dyn Expr>,
    arguments: Vec<Box<dyn Expr>>,
}

impl Call {
    pub fn new(callee: Box<dyn Expr>, arguments: Vec<Box<dyn Expr>>) -> Self {
        Self { callee, arguments }
    }
}

impl Expr for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("call")
            .field("callee", &self.callee)
            .field("arguments", &self.arguments)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.callee.line()
    }

    fn col(&self) -> u16 {
        self.callee.col()
    }

    fn is_offset(&self) -> bool {
        true
    }

    fn is_lvalue(&self) -> bool {
        false
    }
}

pub struct Instantiation {
    token: Token,
    call: Box<dyn Expr>,
}

impl Instantiation {
    pub fn new(token: Token, call: Box<dyn Expr>) -> Self {
        Self { token, call }
    }
}

impl Expr for Instantiation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("instantiation")
            .field("token", &self.token)
            .field("call", &self.call)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.token.line
    }

    fn col(&self) -> u16 {
        self.token.col
    }
    fn is_offset(&self) -> bool {
        false
    }

    fn is_lvalue(&self) -> bool {
        false
    }
}

pub struct CloneExpression {
    token: Token,
    object: Box<dyn Expr>,
}

impl CloneExpression {
    pub fn new(token: Token, object: Box<dyn Expr>) -> Self {
        Self { token, object }
    }
}

impl Expr for CloneExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("clone")
            .field("token", &self.token)
            .field("object", &self.object)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.token.line
    }

    fn col(&self) -> u16 {
        self.token.col
    }
    fn is_offset(&self) -> bool {
        false
    }

    fn is_lvalue(&self) -> bool {
        false
    }
}
pub struct Member {
    parent: Box<dyn Expr>,
    member: Box<dyn Expr>,
    is_static: bool,
}

impl Member {
    pub fn new(parent: Box<dyn Expr>, member: Box<dyn Expr>, is_static: bool) -> Self {
        Self {
            parent,
            member,
            is_static,
        }
    }
}

impl Expr for Member {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("member")
            .field("parent", &self.parent)
            .field("member", &self.member)
            .field("is_static", &self.is_static)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.member.line()
    }

    fn col(&self) -> u16 {
        self.member.col()
    }

    fn is_offset(&self) -> bool {
        true
    }

    fn is_lvalue(&self) -> bool {
        true
    }
}

pub struct Field {
    array: Box<dyn Expr>,
    field: Option<Box<dyn Expr>>,
}

impl Field {
    pub fn new(array: Box<dyn Expr>, field: Option<Box<dyn Expr>>) -> Self {
        Self { array, field }
    }
}

impl Expr for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("field / array access")
            .field("array", &self.array)
            .field("field", &self.field)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.array.line()
    }

    fn col(&self) -> u16 {
        self.array.col()
    }

    fn is_offset(&self) -> bool {
        true
    }

    fn is_lvalue(&self) -> bool {
        true
    }
}

pub struct FunctionExpression {
    is_static: bool,
    token: Token,
    arguments: Option<Vec<FunctionArgument>>,
    return_type: Option<ReturnType>,
    uses: Option<Vec<Box<dyn Expr>>>,
    body: Box<dyn Stmt>,
}

impl FunctionExpression {
    pub fn new(
        is_static: bool,
        token: Token,
        arguments: Option<Vec<FunctionArgument>>,
        return_type: Option<ReturnType>,
        uses: Option<Vec<Box<dyn Expr>>>,
        body: Box<dyn Stmt>,
    ) -> Self {
        Self {
            is_static,
            token,
            arguments,
            return_type,
            uses,
            body,
        }
    }
}

impl Expr for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("function")
            .field("is_static", &self.is_static)
            .field("token", &self.token)
            .field("arguments", &self.arguments)
            .field("return_type", &self.return_type)
            .field("uses", &self.uses)
            .field("body", &self.body)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.token.line
    }

    fn col(&self) -> u16 {
        self.token.col
    }

    fn is_offset(&self) -> bool {
        true
    }

    fn is_lvalue(&self) -> bool {
        false
    }
}

pub struct AnonymousClassExpression {
    token: Token,
    constructor_args: Option<Vec<Box<dyn Expr>>>,
    implements: Option<Vec<Box<dyn Expr>>>,
    extends: Option<Vec<Box<dyn Expr>>>,
    body: Vec<Box<dyn Stmt>>,
}

impl AnonymousClassExpression {
    pub fn new(
        token: Token,
        constructor_args: Option<Vec<Box<dyn Expr>>>,
        implements: Option<Vec<Box<dyn Expr>>>,
        extends: Option<Vec<Box<dyn Expr>>>,
        body: Vec<Box<dyn Stmt>>,
    ) -> Self {
        Self {
            token,
            constructor_args,
            implements,
            extends,
            body,
        }
    }
}

impl Expr for AnonymousClassExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("anonymous class")
            .field("token", &self.token)
            .field("constructor_args", &self.constructor_args)
            .field("implements", &self.implements)
            .field("extends", &self.extends)
            .field("body", &self.body)
            .finish()
    }

    fn token_type(&self) -> Option<TokenType> {
        None
    }

    fn line(&self) -> u16 {
        self.token.line
    }

    fn col(&self) -> u16 {
        self.token.col
    }

    fn is_offset(&self) -> bool {
        true
    }

    fn is_lvalue(&self) -> bool {
        false
    }
}
