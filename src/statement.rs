use crate::expression::Node;
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
    expression: Box<Node>,
}

impl ExpressionStatement {
    pub fn new(expression: Box<Node>) -> Self {
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
    expressions: Vec<Box<Node>>,
}

impl EchoStatement {
    pub fn new(expressions: Vec<Box<Node>>) -> Self {
        Self { expressions }
    }
}

impl Stmt for EchoStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Echo")
            .field("expressions", &self.expressions)
            .finish()
    }
}

pub struct ConstStatement {
    constants: Vec<Node>,
}

impl ConstStatement {
    pub fn new(constants: Vec<Node>) -> Self {
        Self { constants }
    }
}

impl Stmt for ConstStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("const")
            .field("constants", &self.constants)
            .finish()
    }
}

pub struct PrintStatement {
    expressions: Vec<Box<Node>>,
}

impl PrintStatement {
    pub fn new(expressions: Vec<Box<Node>>) -> Self {
        Self { expressions }
    }
}

impl Stmt for PrintStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("print")
            .field("expressions", &self.expressions)
            .finish()
    }
}

pub struct GotoStatement {
    label: Token,
}

impl GotoStatement {
    pub fn new(label: Token) -> Self {
        Self { label }
    }
}

impl Stmt for GotoStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("goto").field("label", &self.label).finish()
    }
}

pub struct LabelStatement {
    label: Token,
    colon: Token,
}

impl LabelStatement {
    pub fn new(label: Token, colon: Token) -> Self {
        Self { label, colon }
    }
}

impl Stmt for LabelStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("label")
            .field("label", &self.label)
            .field("colon", &self.colon)
            .finish()
    }
}
pub struct ThrowStatement {
    expression: Box<Node>,
}

impl ThrowStatement {
    pub fn new(expression: Box<Node>) -> Self {
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
    parameters: Vec<Box<Node>>,
}

impl UnsetStatement {
    pub fn new(parameters: Vec<Box<Node>>) -> Self {
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
    expression: Option<Box<Node>>,
}

impl ReturnStatement {
    pub fn new(expression: Option<Box<Node>>) -> Self {
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
    expression: Box<Node>,
}

impl NamespaceStatement {
    pub fn new(expression: Box<Node>) -> Self {
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
    imports: Vec<Node>,
}

impl UseStatement {
    pub fn new(imports: Vec<Node>) -> Self {
        Self { imports }
    }
}

impl Stmt for UseStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Use")
            .field("imports", &self.imports)
            .finish()
    }
}

pub struct UseFunctionStatement {
    imports: Vec<Node>,
}

impl UseFunctionStatement {
    pub fn new(imports: Vec<Node>) -> Self {
        Self { imports }
    }
}

impl Stmt for UseFunctionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Use function")
            .field("imports", &self.imports)
            .finish()
    }
}

pub struct UseConstStatement {
    imports: Vec<Node>,
}

impl UseConstStatement {
    pub fn new(imports: Vec<Node>) -> Self {
        Self { imports }
    }
}

impl Stmt for UseConstStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Use const")
            .field("imports", &self.imports)
            .finish()
    }
}

pub struct UseTraitStatement {
    token: Token,
    type_refs: Vec<Box<Node>>,
}

impl UseTraitStatement {
    pub fn new(token: Token, type_refs: Vec<Box<Node>>) -> Self {
        Self { token, type_refs }
    }
}

impl Stmt for UseTraitStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Use trait")
            .field("token", &self.token)
            .field("type_refs", &self.type_refs)
            .finish()
    }
}
pub struct ClassStatement {
    name: Token,
    is_abstract: bool,
    is_final: bool,
    implements: Option<Vec<Box<Node>>>,
    extends: Option<Vec<Box<Node>>>,
    body: Vec<Box<dyn Stmt>>,
}

impl ClassStatement {
    pub fn new(
        name: Token,
        is_abstract: bool,
        is_final: bool,
        implements: Option<Vec<Box<Node>>>,
        extends: Option<Vec<Box<Node>>>,
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
    extends: Option<Vec<Box<Node>>>,
    body: Vec<Box<dyn Stmt>>,
}

impl Interface {
    pub fn new(name: Token, extends: Option<Vec<Box<Node>>>, body: Vec<Box<dyn Stmt>>) -> Self {
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
    value: Box<Node>,
}

impl ClassConstantDefinitionStatement {
    pub fn new(name: Token, visibility: Option<Token>, value: Box<Node>) -> Self {
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
    is_abstract: Option<Token>,
    value: Option<Box<Node>>,
    is_static: Option<Token>,
}

impl PropertyDefinitionStatement {
    pub fn new(
        name: Token,
        visibility: Option<Token>,
        is_abstract: Option<Token>,
        value: Option<Box<Node>>,
        is_static: Option<Token>,
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
    is_final: Option<Token>,
    by_ref: Option<Token>,
    name: Token,
    visibility: Option<Token>,
    is_abstract: Option<Token>,
    function: Box<dyn Stmt>,
    is_static: Option<Token>,
}

impl MethodDefinitionStatement {
    pub fn new(
        is_final: Option<Token>,
        by_ref: Option<Token>,
        name: Token,
        visibility: Option<Token>,
        is_abstract: Option<Token>,
        function: Box<dyn Stmt>,
        is_static: Option<Token>,
    ) -> Self {
        Self {
            is_final,
            by_ref,
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
            .field("is_final", &self.is_final)
            .field("by_ref", &self.by_ref)
            .field("name", &self.name)
            .field("visibility", &self.visibility)
            .field("is_abstract", &self.is_abstract)
            .field("function", &self.function)
            .field("is_static", &self.is_static)
            .finish()
    }
}

pub struct FunctionDefinitionStatement {
    arguments: Option<Vec<Node>>,
    return_type: Option<Box<Node>>,
    body: Option<Box<dyn Stmt>>,
}

impl FunctionDefinitionStatement {
    pub fn new(
        arguments: Option<Vec<Node>>,
        return_type: Option<Box<Node>>,
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
    by_ref: Option<Token>,
    name: Token,
    function: Box<dyn Stmt>,
}

impl NamedFunctionDefinitionStatement {
    pub fn new(by_ref: Option<Token>, name: Token, function: Box<dyn Stmt>) -> Self {
        Self {
            by_ref,
            name,
            function,
        }
    }
}

impl Stmt for NamedFunctionDefinitionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("named-function")
            .field("by_ref", &self.by_ref)
            .field("name", &self.name)
            .field("function", &self.function)
            .finish()
    }
}

pub struct WhileStatement {
    condition: Box<Node>,
    body: Box<dyn Stmt>,
}

impl WhileStatement {
    pub fn new(condition: Box<Node>, body: Box<dyn Stmt>) -> Self {
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
    condition: Box<Node>,
    body: Box<dyn Stmt>,
}

impl DoWhileStatement {
    pub fn new(condition: Box<Node>, body: Box<dyn Stmt>) -> Self {
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
    array: Box<Node>,
    kv: Box<Node>,
    body: Box<dyn Stmt>,
}

impl ForEachStatement {
    pub fn new(array: Box<Node>, kv: Box<Node>, body: Box<dyn Stmt>) -> Self {
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
    condition: Box<Node>,
    body: Box<dyn Stmt>,
}

impl IfBranch {
    pub fn new(condition: Box<Node>, body: Box<dyn Stmt>) -> Self {
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
    cases: Vec<Option<Box<Node>>>,
    body: Vec<Box<dyn Stmt>>,
}

impl SwitchBranch {
    pub fn new(cases: Vec<Option<Box<Node>>>, body: Vec<Box<dyn Stmt>>) -> Self {
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
    expr: Box<Node>,
    branches: Vec<Box<SwitchBranch>>,
}

impl SwitchCase {
    pub fn new(expr: Box<Node>, branches: Vec<Box<SwitchBranch>>) -> Self {
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
    expr: Option<Box<Node>>,
}

impl TokenStatement {
    pub fn new(token: Token, expr: Option<Box<Node>>) -> Self {
        Self { token, expr }
    }
}

impl Stmt for TokenStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("(token)")
            .field("token", &self.token)
            .field("expr", &self.expr)
            .finish()
    }
}

pub struct CatchBlock {
    types: Vec<Box<Node>>,
    var: Token,
    body: Box<dyn Stmt>,
}

impl CatchBlock {
    pub fn new(types: Vec<Box<Node>>, var: Token, body: Box<dyn Stmt>) -> Self {
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

pub struct StaticVariablesStatement {
    assignments: Vec<Node>,
}

impl StaticVariablesStatement {
    pub fn new(assignments: Vec<Node>) -> Self {
        Self { assignments }
    }
}

impl Stmt for StaticVariablesStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("static vars")
            .field("assignments", &self.assignments)
            .finish()
    }
}

pub struct GlobalVariablesStatement {
    vars: Vec<Box<Node>>,
}

impl GlobalVariablesStatement {
    pub fn new(vars: Vec<Box<Node>>) -> Self {
        Self { vars }
    }
}

impl Stmt for GlobalVariablesStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("global vars")
            .field("vars", &self.vars)
            .finish()
    }
}

pub struct InlineHtml {
    start: Token,
    end: Option<Token>,
}

impl InlineHtml {
    pub fn new(start: Token, end: Option<Token>) -> Self {
        Self { start, end }
    }
}

impl Stmt for InlineHtml {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("inline-html")
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}
