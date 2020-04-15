use crate::statement::Stmt;
use crate::token::Token;

#[derive(Debug)]
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
    Operator(Token),
    Identifier(Token),

    // Formerly known path
    QualifiedNamespaceName(Vec<Node>),
    Grouping(Box<Node>),

    Array {
        ob: Token,
        elements: Vec<Box<Node>>,
        cb: Token,
    },
    OldArray {
        token: Token,
        op: Token,
        elements: Vec<Box<Node>>,
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
        elements: Vec<Box<Node>>,
        cp: Token,
    },
    Call {
        callee: Box<Node>,
        op: Token,
        parameters: Vec<Box<Node>>,
        cp: Token,
    },
    Isset {
        isset: Token,
        op: Token,
        parameters: Vec<Box<Node>>,
        cp: Token,
    },
    Empty {
        empty: Token,
        op: Token,
        parameters: Vec<Box<Node>>,
        cp: Token,
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
    Field {
        array: Box<Node>,
        ob: Token,
        index: Option<Box<Node>>,
        cb: Token,
    },
    Static {
        token: Token,
        expr: Box<Node>,
    },
    Function {
        is_static: Option<Token>,
        token: Token,
        arguments: Option<Vec<Node>>,
        uses: Option<Vec<Box<Node>>>,
        return_type: Option<Box<Node>>,
        body: Box<dyn Stmt>,
    },
    FunctionArgument {
        argument_type: Option<Box<Node>>,
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
        arguments: Option<Vec<Box<Node>>>,
        extends: Option<Vec<Box<Node>>>,
        implements: Option<Vec<Box<Node>>>,
        body: Vec<Box<dyn Stmt>>,
    },
    Yield {
        token: Token,
        expr: Box<Node>,
    },
    YieldFrom {
        token: Token,
        expr: Box<Node>,
    },
    FileInclude {
        token: Token,
        resource: Box<Node>,
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
