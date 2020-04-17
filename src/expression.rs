use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Node {
    // LexicalVariable -> Unary(Variable, &?)
    Unary {
        expr: Box<Node>,
        token: Token,
    },
    PostUnary {
        expr: Box<Node>,
        token: Token,
    },
    Const {
        name: Token,
        token: Token,
        value: Box<Node>,
    },
    Binary {
        left: Box<Node>,
        token: Token,
        right: Box<Node>,
    },
    Ternary {
        check: Box<Node>,
        qm: Token,
        true_arm: Option<Box<Node>>,
        colon: Token,
        false_arm: Box<Node>,
    },
    Literal(Token),
    Variable(Token),
    LexicalVariable {
        reference: Option<Token>,
        variable: Token,
    },
    AliasedVariable {
        variable: Token,
        expr: Box<Node>,
    },
    DynamicVariable {
        variable: Token,
        oc: Token,
        expr: Box<Node>,
        cc: Token,
    },
    StaticVariable {
        variable: Token,
        assignment: Option<Token>,
        value: Option<Box<Node>>,
    },
    Operator(Token),
    Identifier(Token),

    // Formerly known path
    QualifiedNamespaceName(Vec<Node>),
    Grouping(Box<Node>),

    Array {
        ob: Token,
        elements: Vec<Node>,
        cb: Token,
    },
    OldArray {
        token: Token,
        op: Token,
        elements: Vec<Node>,
        cp: Token,
    },
    ArrayElement {
        key: Option<Box<Node>>,
        arrow: Option<Token>,
        value: Box<Node>,
    },
    List {
        token: Token,
        op: Token,
        elements: Vec<Node>,
        cp: Token,
    },
    Call {
        callee: Box<Node>,
        op: Token,
        parameters: Vec<Node>,
        cp: Token,
    },
    Isset {
        isset: Token,
        op: Token,
        parameters: Vec<Node>,
        cp: Token,
    },
    Empty {
        empty: Token,
        op: Token,
        parameters: Vec<Node>,
        cp: Token,
    },
    Exit {
        exit: Token,
        op: Option<Token>,
        parameters: Option<Vec<Node>>,
        cp: Option<Token>,
    },
    New {
        token: Token,
        class: Box<Node>,
    },
    Clone {
        token: Token,
        object: Box<Node>,
    },
    Member {
        object: Box<Node>,
        arrow: Token,
        member: Box<Node>,
    },
    StaticMember {
        class: Box<Node>,
        pn: Token,
        member: Box<Node>,
    },
    StaticMethod {
        class: Box<Node>,
        pn: Token,
        oc: Token,
        method: Box<Node>,
        cc: Token,
    },
    Field {
        array: Box<Node>,
        ob: Token,
        index: Option<Box<Node>>,
        cb: Token,
    },
    Static {
        token: Token,
        expr: Vec<Node>,
    },
    Function {
        is_static: Option<Token>,
        token: Token,
        op: Token,
        arguments: Option<Vec<Node>>,
        cp: Token,
        uses: Option<Vec<Node>>,
        return_type: Box<Option<Node>>,
        body: Box<Node>,
    },
    FunctionArgument {
        argument_type: Box<Option<Node>>,
        name: Token,
        has_default: Option<Token>,
        default_value: Option<Box<Node>>,
        spread: Option<Token>,
        reference: Option<Token>,
    },
    ArgumentType {
        nullable: Option<Token>,
        type_ref: Box<Node>,
    },
    ReturnType {
        token: Token,
        nullable: Option<Token>,
        type_ref: Box<Node>,
    },
    TypeRef(Vec<Token>),
    Class {
        token: Token,
        arguments: Option<Vec<Node>>,
        extends: Option<Vec<Node>>,
        implements: Option<Vec<Node>>,
        body: Box<Node>,
    },
    Yield {
        token: Token,
        expr: Option<Box<Node>>,
    },
    YieldFrom {
        token: Token,
        expr: Box<Node>,
    },
    FileInclude {
        token: Token,
        resource: Box<Node>,
    },
    UseDeclaration {
        token: Option<Token>,
        declaration: Box<Node>,
        aliased: Option<Token>,
        alias: Option<Token>,
    },
    UseConst {
        token: Option<Token>,
        constant: Box<Node>,
        aliased: Option<Token>,
        alias: Option<Token>,
    },
    UseFunction {
        token: Option<Token>,
        function: Box<Node>,
        aliased: Option<Token>,
        alias: Option<Token>,
    },
    GroupedUse {
        token: Token,
        parent: Box<Node>,
        oc: Token,
        uses: Vec<Node>,
        cc: Token,
    },

    // Statements
    ExpressionStatement {
        expression: Box<Node>,
    },
    EchoStatement {
        token: Token,
        expressions: Vec<Node>,
    },
    ConstStatement {
        token: Token,
        constants: Vec<Node>,
    },
    PrintStatement {
        token: Token,
        expressions: Vec<Node>,
    },
    GotoStatement {
        token: Token,
        label: Token,
    },
    LabelStatement {
        label: Token,
        colon: Token,
    },
    ThrowStatement {
        token: Token,
        expression: Box<Node>,
    },
    DeclareStatement {
        directive: Token,
        value: Token,
        assignment: Token,
        op: Token,
        cp: Token,
        token: Token,
    },
    UnsetStatement {
        vars: Vec<Node>,
    },
    ReturnStatement {
        token: Token,
        expression: Option<Box<Node>>,
    },
    NamespaceStatement {
        token: Token,
        type_ref: Box<Node>,
    },
    NamespaceBlock {
        token: Token,
        type_ref: Box<Option<Node>>,
        block: Box<Node>,
    },
    UseStatement {
        imports: Vec<Node>,
    },
    UseFunctionStatement {
        token: Token,
        imports: Vec<Node>,
    },
    UseConstStatement {
        token: Token,
        imports: Vec<Node>,
    },
    UseTraitStatement {
        token: Token,
        type_refs: Vec<Node>,
    },
    // Merge with Class?
    ClassStatement {
        token: Token,
        name: Token,
        is_abstract: Option<Token>,
        is_final: Option<Token>,
        implements: Option<Vec<Node>>,
        extends: Option<Vec<Node>>,
        body: Box<Node>,
    },
    TraitStatement {
        token: Token,
        name: Token,
        body: Box<Node>,
    },
    Interface {
        token: Token,
        name: Token,
        extends: Option<Vec<Node>>,
        body: Box<Node>,
    },
    ClassConstantDefinitionStatement {
        name: Token,
        visibility: Option<Token>,
        value: Box<Node>,
    },
    PropertyDefinitionStatement {
        name: Token,
        visibility: Option<Token>,
        is_abstract: Option<Token>,
        value: Option<Box<Node>>,
        is_static: Option<Token>,
    },
    MethodDeclarationStatement {
        name: Token,
        visibility: Option<Token>,
        function: Box<Node>,
        is_static: bool,
    },
    MethodDefinitionStatement {
        is_final: Option<Token>,
        by_ref: Option<Token>,
        name: Token,
        visibility: Option<Token>,
        is_abstract: Option<Token>,
        function: Box<Node>,
        is_static: Option<Token>,
    },
    FunctionDefinitionStatement {
        op: Token,
        arguments: Option<Vec<Node>>,
        cp: Token,
        return_type: Box<Option<Node>>,
        body: Option<Box<Node>>,
    },
    NamedFunctionDefinitionStatement {
        token: Token,
        by_ref: Option<Token>,
        name: Token,
        function: Box<Node>,
    },
    WhileStatement {
        token: Token,
        op: Token,
        condition: Box<Node>,
        cp: Token,
        body: Box<Node>,
    },
    DoWhileStatement {
        do_token: Token,
        op: Token,
        cp: Token,
        while_token: Token,
        condition: Box<Node>,
        body: Box<Node>,
    },
    ForStatement {
        token: Token,
        init: Vec<Node>,
        condition: Vec<Node>,
        step: Vec<Node>,
        body: Box<Node>,
    },
    ForEachStatement {
        token: Token,
        op: Token,
        collection: Box<Node>,
        as_token: Token,
        kv: Box<Node>,
        cp: Token,
        body: Box<Node>,
    },
    Block {
        oc: Token,
        statements: Vec<Node>,
        cc: Token,
    },
    SingleStatementBlock(Box<Node>),
    IfBranch {
        token: Token,
        op: Token,
        condition: Box<Node>,
        cp: Token,
        body: Box<Node>,
    },
    ElseBranch {
        token: Token,
        body: Box<Node>,
    },
    IfStatement {
        if_branch: Box<Node>,
        elseif_branches: Vec<Node>,
        else_branch: Option<Box<Node>>,
    },
    SwitchBranch {
        /// The one without an expression is the default branch
        cases: Vec<Option<Node>>,
        body: Vec<Node>,
    },
    SwitchCase {
        token: Token,
        op: Token,
        expr: Box<Node>,
        cp: Token,
        oc: Token,
        branches: Vec<Node>,
        cc: Token,
    },
    TokenStatement {
        token: Token,
        expr: Option<Box<Node>>,
    },
    CatchBlock {
        token: Token,
        op: Token,
        types: Vec<Node>,
        var: Token,
        cp: Token,
        body: Box<Node>,
    },
    FinallyBlock {
        token: Token,
        body: Box<Node>,
    },
    TryCatch {
        token: Token,
        try_block: Box<Node>,
        catch_blocks: Vec<Node>,
        finally_block: Option<Box<Node>>,
    },
    StaticVariablesStatement {
        token: Token,
        assignments: Vec<Node>,
    },
    GlobalVariablesStatement {
        token: Token,
        vars: Vec<Node>,
    },
    InlineHtml {
        start: Token,
        end: Option<Token>,
    },
}

pub trait Expr {
    /// Tells if the expression can be used as offset
    fn is_offset(&self) -> bool;

    /// Tells if the expression can have values assigned
    fn is_lvalue(&self) -> bool;
}

impl Expr for Node {
    fn is_offset(&self) -> bool {
        true
    }

    fn is_lvalue(&self) -> bool {
        true
    }
}
