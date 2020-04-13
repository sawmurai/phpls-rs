use crate::expression::Expr;
use crate::token::Token;

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

pub struct ThrowStatement {
    expression: Box<dyn Expr>,
}

impl ThrowStatement {
    pub fn new(expression: Box<dyn Expr>) -> Self {
        Self { expression }
    }
}

impl Stmt for ThrowStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("throw")
            .field("expression", &self.expression)
            .finish()
    }
}

pub struct DeclareStatement {
    directive: Token,
    value: Token,
}

impl DeclareStatement {
    pub fn new(directive: Token, value: Token) -> Self {
        Self { directive, value }
    }
}

impl Stmt for DeclareStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("declare")
            .field("directive", &self.directive)
            .field("value", &self.value)
            .finish()
    }
}

pub struct UnsetStatement {
    parameters: Vec<Box<dyn Expr>>,
}

impl UnsetStatement {
    pub fn new(parameters: Vec<Box<dyn Expr>>) -> Self {
        Self { parameters }
    }
}

impl Stmt for UnsetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("unset")
            .field("parameters", &self.parameters)
            .finish()
    }
}

pub struct ReturnStatement {
    expression: Option<Box<dyn Expr>>,
}

impl ReturnStatement {
    pub fn new(expression: Option<Box<dyn Expr>>) -> Self {
        Self { expression }
    }
}

impl Stmt for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("return")
            .field("expression", &self.expression)
            .finish()
    }
}

pub struct NamespaceStatement {
    expression: Box<dyn Expr>,
}

impl NamespaceStatement {
    pub fn new(expression: Box<dyn Expr>) -> Self {
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
    path: Box<dyn Expr>,
    alias: Option<Token>,
    group: Option<Vec<Box<dyn Stmt>>>,
}

impl UseStatement {
    pub fn new(path: Box<dyn Expr>) -> Self {
        Self {
            path,
            alias: None,
            group: None,
        }
    }

    pub fn aliased(path: Box<dyn Expr>, alias: Token) -> Self {
        Self {
            path,
            alias: Some(alias),
            group: None,
        }
    }

    pub fn grouped(path: Box<dyn Expr>, group: Vec<Box<dyn Stmt>>) -> Self {
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
    is_final: bool,
    implements: Option<Vec<Token>>,
    extends: Option<Vec<Token>>,
    body: Vec<Box<dyn Stmt>>,
}

impl ClassStatement {
    pub fn new(
        name: Token,
        is_abstract: bool,
        is_final: bool,
        implements: Option<Vec<Token>>,
        extends: Option<Vec<Token>>,
        body: Vec<Box<dyn Stmt>>,
    ) -> Self {
        Self {
            name,
            is_abstract,
            is_final,
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
            .field("is_final", &self.is_final)
            .field("implements", &self.implements)
            .field("extends", &self.extends)
            .field("body", &self.body)
            .finish()
    }
}

pub struct TraitStatement {
    name: Token,
    body: Vec<Box<dyn Stmt>>,
}

impl TraitStatement {
    pub fn new(name: Token, body: Vec<Box<dyn Stmt>>) -> Self {
        Self { name, body }
    }
}

impl Stmt for TraitStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Trait")
            .field("name", &self.name)
            .field("body", &self.body)
            .finish()
    }
}

pub struct Interface {
    name: Token,
    extends: Option<Vec<Token>>,
    body: Vec<Box<dyn Stmt>>,
}

impl Interface {
    pub fn new(name: Token, extends: Option<Vec<Token>>, body: Vec<Box<dyn Stmt>>) -> Self {
        Self {
            name,
            extends,
            body,
        }
    }
}

impl Stmt for Interface {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Interface")
            .field("name", &self.name)
            .field("extends", &self.extends)
            .field("body", &self.body)
            .finish()
    }
}

pub struct ClassConstantDefinitionStatement {
    name: Token,
    visibility: Option<Token>,
    value: Box<dyn Expr>,
}

impl ClassConstantDefinitionStatement {
    pub fn new(name: Token, visibility: Option<Token>, value: Box<dyn Expr>) -> Self {
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
    visibility: Option<Token>,
    is_abstract: bool,
    value: Option<Box<dyn Expr>>,
    is_static: bool,
}

impl PropertyDefinitionStatement {
    pub fn new(
        name: Token,
        visibility: Option<Token>,
        is_abstract: bool,
        value: Option<Box<dyn Expr>>,
        is_static: bool,
    ) -> Self {
        Self {
            name,
            visibility,
            is_abstract,
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
            .field("is_abstract", &self.is_abstract)
            .field("value", &self.value)
            .field("is_static", &self.is_static)
            .finish()
    }
}

pub struct MethodDeclarationStatement {
    name: Token,
    visibility: Option<Token>,
    function: Box<dyn Stmt>,
    is_static: bool,
}

impl MethodDeclarationStatement {
    pub fn new(
        name: Token,
        visibility: Option<Token>,
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

impl Stmt for MethodDeclarationStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("method declaration")
            .field("name", &self.name)
            .field("visibility", &self.visibility)
            .field("function", &self.function)
            .field("is_static", &self.is_static)
            .finish()
    }
}

pub struct MethodDefinitionStatement {
    name: Token,
    visibility: Option<Token>,
    is_abstract: bool,
    function: Box<dyn Stmt>,
    is_static: bool,
}

impl MethodDefinitionStatement {
    pub fn new(
        name: Token,
        visibility: Option<Token>,
        is_abstract: bool,
        function: Box<dyn Stmt>,
        is_static: bool,
    ) -> Self {
        Self {
            name,
            visibility,
            is_abstract,
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
            .field("is_abstract", &self.is_abstract)
            .field("function", &self.function)
            .field("is_static", &self.is_static)
            .finish()
    }
}

pub struct FunctionDefinitionStatement {
    arguments: Option<Vec<FunctionArgument>>,
    return_type: Option<ReturnType>,
    body: Option<Box<dyn Stmt>>,
}

impl FunctionDefinitionStatement {
    pub fn new(
        arguments: Option<Vec<FunctionArgument>>,
        return_type: Option<ReturnType>,
        body: Option<Box<dyn Stmt>>,
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

pub struct NamedFunctionDefinitionStatement {
    name: Token,
    function: Box<dyn Stmt>,
}

impl NamedFunctionDefinitionStatement {
    pub fn new(name: Token, function: Box<dyn Stmt>) -> Self {
        Self { name, function }
    }
}

impl Stmt for NamedFunctionDefinitionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("named-function")
            .field("name", &self.name)
            .field("function", &self.function)
            .finish()
    }
}

#[derive(Debug)]
pub struct FunctionArgument {
    t: Option<ArgumentType>,
    name: Token,
    default: Option<Box<dyn Expr>>,
}

impl FunctionArgument {
    pub fn new(t: Option<ArgumentType>, name: Token, default: Option<Box<dyn Expr>>) -> Self {
        Self { t, name, default }
    }
}

#[derive(Debug)]
pub struct ReturnType {
    token: Option<Token>,
    path: Option<Box<dyn Expr>>,
    nullable: bool,
}

impl ReturnType {
    pub fn path(path: Box<dyn Expr>, nullable: bool) -> Self {
        Self {
            token: None,
            path: Some(path),
            nullable,
        }
    }

    pub fn primitive(token: Token, nullable: bool) -> Self {
        Self {
            path: None,
            token: Some(token),
            nullable,
        }
    }
}

#[derive(Debug)]
pub struct ArgumentType {
    token: Option<Token>,
    path: Option<Box<dyn Expr>>,
    nullable: bool,
}

impl ArgumentType {
    pub fn path(path: Box<dyn Expr>, nullable: bool) -> Self {
        Self {
            token: None,
            path: Some(path),
            nullable,
        }
    }

    pub fn primitive(token: Token, nullable: bool) -> Self {
        Self {
            path: None,
            token: Some(token),
            nullable,
        }
    }
}

pub struct WhileStatement {
    condition: Box<dyn Expr>,
    body: Box<dyn Stmt>,
}

impl WhileStatement {
    pub fn new(condition: Box<dyn Expr>, body: Box<dyn Stmt>) -> Self {
        Self { condition, body }
    }
}

impl Stmt for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("while")
            .field("condition", &self.condition)
            .field("body", &self.body)
            .finish()
    }
}

pub struct DoWhileStatement {
    condition: Box<dyn Expr>,
    body: Box<dyn Stmt>,
}

impl DoWhileStatement {
    pub fn new(condition: Box<dyn Expr>, body: Box<dyn Stmt>) -> Self {
        Self { condition, body }
    }
}

impl Stmt for DoWhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("do-while")
            .field("condition", &self.condition)
            .field("body", &self.body)
            .finish()
    }
}

pub struct ForStatement {
    init: Vec<Box<dyn Stmt>>,
    condition: Vec<Box<dyn Stmt>>,
    step: Vec<Box<dyn Stmt>>,
    body: Box<dyn Stmt>,
}

impl ForStatement {
    pub fn new(
        init: Vec<Box<dyn Stmt>>,
        condition: Vec<Box<dyn Stmt>>,
        step: Vec<Box<dyn Stmt>>,
        body: Box<dyn Stmt>,
    ) -> Self {
        Self {
            init,
            condition,
            step,
            body,
        }
    }
}

impl Stmt for ForStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("for")
            .field("init", &self.init)
            .field("condition", &self.condition)
            .field("step", &self.step)
            .field("body", &self.body)
            .finish()
    }
}

pub struct ForEachStatement {
    array: Box<dyn Expr>,
    kv: Box<dyn Expr>,
    body: Box<dyn Stmt>,
}

impl ForEachStatement {
    pub fn new(array: Box<dyn Expr>, kv: Box<dyn Expr>, body: Box<dyn Stmt>) -> Self {
        Self { array, kv, body }
    }
}

impl Stmt for ForEachStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("foreach")
            .field("array", &self.array)
            .field("kv", &self.kv)
            .field("body", &self.body)
            .finish()
    }
}

#[derive(Debug)]
pub struct Block {
    body: Vec<Box<dyn Stmt>>,
}

impl Block {
    pub fn new(body: Vec<Box<dyn Stmt>>) -> Self {
        Self { body }
    }
}

impl Stmt for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("block").field("body", &self.body).finish()
    }
}

#[derive(Debug)]
pub struct IfBranch {
    condition: Box<dyn Expr>,
    body: Box<dyn Stmt>,
}

impl IfBranch {
    pub fn new(condition: Box<dyn Expr>, body: Box<dyn Stmt>) -> Self {
        Self { condition, body }
    }
}

impl Stmt for IfBranch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("if-branch")
            .field("condition", &self.condition)
            .field("body", &self.body)
            .finish()
    }
}

pub struct ElseBranch {
    body: Box<dyn Stmt>,
}

impl ElseBranch {
    pub fn new(body: Box<dyn Stmt>) -> Self {
        Self { body }
    }
}

impl Stmt for ElseBranch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("else-branch")
            .field("body", &self.body)
            .finish()
    }
}

pub struct IfStatement {
    if_branch: Box<IfBranch>,
    else_if_branches: Vec<Box<IfBranch>>,
    else_branch: Option<Box<dyn Stmt>>,
}

impl IfStatement {
    pub fn new(
        if_branch: Box<IfBranch>,
        else_if_branches: Vec<Box<IfBranch>>,
        else_branch: Option<Box<dyn Stmt>>,
    ) -> Self {
        Self {
            if_branch,
            else_if_branches,
            else_branch,
        }
    }
}

impl Stmt for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("if")
            .field("if_branch", &self.if_branch)
            .field("else_if_branches", &self.else_if_branches)
            .field("else_branch", &self.else_branch)
            .finish()
    }
}

#[derive(Debug)]
pub struct SwitchBranch {
    /// The one without an expression is the default branch
    cases: Vec<Option<Box<dyn Expr>>>,
    body: Vec<Box<dyn Stmt>>,
}

impl SwitchBranch {
    pub fn new(cases: Vec<Option<Box<dyn Expr>>>, body: Vec<Box<dyn Stmt>>) -> Self {
        Self { cases, body }
    }
}

impl Stmt for SwitchBranch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("switch-branch")
            .field("cases", &self.cases)
            .field("body", &self.body)
            .finish()
    }
}

pub struct SwitchCase {
    expr: Box<dyn Expr>,
    branches: Vec<Box<SwitchBranch>>,
}

impl SwitchCase {
    pub fn new(expr: Box<dyn Expr>, branches: Vec<Box<SwitchBranch>>) -> Self {
        Self { expr, branches }
    }
}

impl Stmt for SwitchCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("switch")
            .field("expr", &self.expr)
            .field("branches", &self.branches)
            .finish()
    }
}

pub struct TokenStatement {
    token: Token,
}

impl TokenStatement {
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

impl Stmt for TokenStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("(token)")
            .field("token", &self.token)
            .finish()
    }
}

pub struct CatchBlock {
    types: Vec<Box<dyn Expr>>,
    var: Token,
    body: Box<dyn Stmt>,
}

impl CatchBlock {
    pub fn new(types: Vec<Box<dyn Expr>>, var: Token, body: Box<dyn Stmt>) -> Self {
        Self { types, var, body }
    }
}

impl Stmt for CatchBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("catch")
            .field("types", &self.types)
            .field("var", &self.var)
            .field("body", &self.body)
            .finish()
    }
}

pub struct TryCatch {
    try_block: Box<dyn Stmt>,
    catch_blocks: Vec<Box<dyn Stmt>>,
    finally_block: Option<Box<dyn Stmt>>,
}

impl TryCatch {
    pub fn new(
        try_block: Box<dyn Stmt>,
        catch_blocks: Vec<Box<dyn Stmt>>,
        finally_block: Option<Box<dyn Stmt>>,
    ) -> Self {
        Self {
            try_block,
            catch_blocks,
            finally_block,
        }
    }
}

impl Stmt for TryCatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("catch")
            .field("try_block", &self.try_block)
            .field("catch_blocks", &self.catch_blocks)
            .field("finally_block", &self.finally_block)
            .finish()
    }
}
