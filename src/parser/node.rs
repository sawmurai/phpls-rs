use lsp_types::Range;

use super::token::{Token, TokenType};
use std::{iter::Skip, ops::Deref, slice::Iter};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct TypeRef {
    pub(crate) kind: Vec<Token>,

    /// Indicator that we are dealing with an array of that type
    /// This is used when parsing phpDoc comments a la Class[] or Array<Class>
    multiple: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassStatement {
    pub(crate) token: Token,
    pub(crate) name: Token,
    pub(crate) is_abstract: Option<Token>,
    pub(crate) is_final: Option<Token>,
    pub(crate) implements: Option<Vec<Node>>,
    pub(crate) extends: Option<Box<Node>>,
    pub(crate) body: Box<Node>,
    pub(crate) doc_comment: Option<Box<Node>>,
    pub(crate) attributes: Vec<Node>,
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
pub struct NodeRange {
    /// Start column
    pub(crate) start_col: u32,

    /// Start line
    pub(crate) start_line: u32,

    /// End column
    pub(crate) end_col: u32,

    /// End line
    pub(crate) end_line: u32,
}

impl From<Range> for NodeRange {
    fn from(lsp_range: Range) -> Self {
        NodeRange::new(
            lsp_range.start.character,
            lsp_range.start.line,
            lsp_range.end.character,
            lsp_range.end.line,
        )
    }
}

impl From<&Token> for NodeRange {
    fn from(token: &Token) -> Self {
        NodeRange::new(token.col, token.line, token.end().0, token.end().1)
    }
}

impl From<((u32, u32), (u32, u32))> for NodeRange {
    fn from(((a, b), (c, d)): ((u32, u32), (u32, u32))) -> NodeRange {
        NodeRange::new(a, b, c, d)
    }
}

impl From<(&Token, &Node)> for NodeRange {
    fn from((token, node): (&Token, &Node)) -> Self {
        let node_range = node.range();
        NodeRange::new(
            token.col,
            token.line,
            node_range.end_col,
            node_range.end_line,
        )
    }
}

impl From<(&Node, &Node)> for NodeRange {
    fn from((node_start, node_end): (&Node, &Node)) -> Self {
        let node_range_start = node_start.range();
        let node_range_end = node_end.range();

        NodeRange::new(
            node_range_start.start_col,
            node_range_start.start_line,
            node_range_end.end_col,
            node_range_end.end_line,
        )
    }
}

impl From<(&Token, &Token)> for NodeRange {
    fn from((start, end): (&Token, &Token)) -> Self {
        let token_end = end.end();

        NodeRange::new(start.col, start.line, token_end.0, token_end.1)
    }
}

impl From<&TypeRef> for NodeRange {
    fn from(tr: &TypeRef) -> Self {
        let first = tr.kind.first().unwrap();
        let last = tr.kind.last().unwrap();

        (first, last).into()
    }
}

impl From<(&Token, &TypeRef)> for NodeRange {
    fn from((token, tr): (&Token, &TypeRef)) -> Self {
        let last = tr.kind.last().unwrap();

        (token, last).into()
    }
}

impl Into<NodeRange> for (&Node, &Token) {
    fn into(self) -> NodeRange {
        let node_range = self.0.range();
        let token_end = self.1.end();

        NodeRange::new(
            node_range.start_col,
            node_range.start_line,
            token_end.0,
            token_end.1,
        )
    }
}

impl NodeRange {
    pub fn new(start_col: u32, start_line: u32, end_col: u32, end_line: u32) -> NodeRange {
        NodeRange {
            start_col,
            start_line,
            end_col,
            end_line,
        }
    }

    pub fn from_range(from: &Self, to: &Self) -> Self {
        NodeRange::new(from.start_col, from.start_line, to.end_col, to.end_line)
    }

    pub fn end(&self) -> (u32, u32) {
        (self.end_col, self.end_line)
    }

    pub fn empty() -> Self {
        Self::new(0, 0, 0, 0)
    }
}

impl TypeRef {
    pub fn one(kind: Vec<Token>) -> Self {
        Self {
            kind,
            multiple: false,
        }
    }

    pub fn many(kind: Vec<Token>) -> Self {
        Self {
            kind,
            multiple: true,
        }
    }

    pub fn to_collection_item(&self) -> Self {
        Self {
            kind: self.kind.clone(),
            multiple: false,
        }
    }

    pub fn append(orig: &Self, appendix: &Self) -> Self {
        let combined = orig
            .kind
            .iter()
            .chain(appendix.kind.iter())
            .cloned()
            .collect();
        Self {
            kind: combined,
            ..*orig
        }
    }

    pub fn is_multiple(&self) -> bool {
        self.multiple
    }

    pub fn range(&self) -> NodeRange {
        NodeRange::from_range(
            &self.kind.first().unwrap().range(),
            &self.kind.last().unwrap().range(),
        )
    }

    pub fn is_fully_qualified(&self) -> bool {
        if let Some(first) = self.kind.first() {
            return first.t == TokenType::NamespaceSeparator;
        }

        false
    }

    /// Return true of the token is an identifier of a built in type
    pub fn is_builtin(&self) -> bool {
        if self.kind.len() == 1 {
            match self.kind[0].t {
                TokenType::TypeString
                | TokenType::TypeSelf
                | TokenType::Static
                | TokenType::Mixed
                | TokenType::TypeArray
                | TokenType::TypeBool
                | TokenType::TypeInt
                | TokenType::TypeFloat
                | TokenType::Null
                | TokenType::TypeObject
                | TokenType::ConstFile
                | TokenType::ConstDir
                | TokenType::ConstClass
                | TokenType::ConstFunction
                | TokenType::ConstMethod
                | TokenType::Callable
                | TokenType::ConstLine
                | TokenType::ConstTrait
                | TokenType::BinaryNumber
                | TokenType::DecimalNumber
                | TokenType::ExponentialNumber
                | TokenType::HexNumber
                | TokenType::LongNumber
                | TokenType::Generator
                | TokenType::Resource
                | TokenType::Void => return true,
                _ => return false,
            }
        }

        false
    }

    /// Ns1\Ns2\Class
    /// This returns Ns1
    pub fn root(&self) -> Option<String> {
        if self.is_empty() {
            return None;
        }

        if self.is_fully_qualified() {
            Some(self.kind[1].to_string())
        } else {
            Some(self.kind[0].to_string())
        }
    }

    /// Ns1\Ns2\Class
    /// This returns Ns2\Class
    pub fn stem(&self) -> Skip<Iter<'_, Token>> {
        if self.is_fully_qualified() {
            self.kind.iter().skip(2)
        } else {
            self.kind.iter().skip(1)
        }
    }

    pub fn root_token_type(&self) -> TokenType {
        self.kind.first().unwrap().t.clone()
    }

    pub fn root_token(&self) -> &Token {
        self.kind.first().unwrap()
    }

    pub fn tip(&self) -> Option<&str> {
        if let Some(last) = self.kind.iter().last() {
            if let Some(label) = last.label.as_ref() {
                return Some(label);
            }
        }

        None
    }

    pub fn to_fqdn(&self) -> String {
        self.kind
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("")
    }

    pub fn len(&self) -> usize {
        self.kind.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0 || self.is_fully_qualified() && self.len() == 1
    }
}

impl From<&TypeRef> for String {
    fn from(s: &TypeRef) -> Self {
        s.kind
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

impl From<Vec<Token>> for TypeRef {
    fn from(tokens: Vec<Token>) -> TypeRef {
        TypeRef {
            kind: tokens,
            multiple: false,
        }
    }
}

impl From<[Token; 0]> for TypeRef {
    fn from(tokens: [Token; 0]) -> TypeRef {
        TypeRef {
            kind: tokens.into(),
            multiple: false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Represents a node in the AST
pub enum Node {
    /// Represents a doc comment
    DocComment {
        comment: Token,
        return_type: Vec<Node>,
        description: String,
        is_deprecated: bool,
        params: Vec<Node>,
        var_docs: Vec<Node>,
        properties: Vec<Node>,
    },
    /// Represents a @param inside of a doc comment
    DocCommentParam {
        name: Token,

        types: Option<Vec<TypeRef>>,

        description: String,
    },
    DocCommentProperty {
        name: Token,
        types: Option<Vec<TypeRef>>,
        description: String,
    },
    DocCommentVar {
        name: Token,

        types: Option<Vec<TypeRef>>,

        description: String,
    },
    DocCommentReturn {
        types: Option<Vec<TypeRef>>,

        description: String,
    },
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
    /// Used as a placeholder for missing nodes. The token acts as an anchor
    Missing(Token),
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
    Identifier(Token),

    // Formerly known path
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
    HaltCompiler {
        hc: Token,
        op: Option<Token>,
        parameters: Option<Vec<Node>>,
        cp: Option<Token>,
    },
    Die {
        die: Token,
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
        oc: Option<Token>,
        member: Box<Node>,
        cc: Option<Token>,
    },
    StaticMember {
        object: Box<Node>,
        pn: Token,
        oc: Option<Token>,
        member: Box<Node>,
        cc: Option<Token>,
    },
    Field {
        array: Box<Node>,
        ob: Token,
        index: Option<Box<Node>>,
        cb: Token,
    },
    Match {
        mtch: Token,
        oc: Token,
        op: Token,
        condition: Box<Node>,
        cp: Token,
        body: Vec<Node>,
        cc: Token,
    },
    MatchArm {
        patterns: Option<Vec<Node>>,
        arrow: Token,
        expression: Box<Node>,
    },
    Static {
        token: Token,
        expr: Vec<Node>,
    },
    // Anonymous function
    Function {
        is_static: Option<Token>,
        by_ref: Option<Token>,
        token: Token,
        op: Token,
        arguments: Option<Vec<Node>>,
        cp: Token,
        uses: Option<Vec<Node>>,
        return_type: Option<Box<Node>>,
        body: Box<Node>,
        attributes: Vec<Node>,
    },
    ArrowFunction {
        is_static: Option<Token>,
        by_ref: Option<Token>,
        token: Token,
        op: Token,
        arguments: Option<Vec<Node>>,
        cp: Token,
        arrow: Token,
        return_type: Option<Box<Node>>,
        body: Box<Node>,
        attributes: Vec<Node>,
    },
    NamedParameter {
        name: Token,
        colon: Token,
        expr: Box<Node>,
    },
    FunctionArgument {
        argument_type: Option<Box<Node>>,
        name: Token,
        has_default: Option<Token>,
        default_value: Option<Box<Node>>,
        spread: Option<Token>,
        reference: Option<Token>,
        doc_comment: Option<Box<Node>>,
        attributes: Vec<Node>,
    },
    DataType {
        nullable: Option<Token>,
        type_refs: Vec<Node>,
    },
    ReturnType {
        token: Token,
        /// Node::DataType
        data_type: Box<Node>,
    },
    TypeRef(TypeRef),
    Class {
        token: Token,
        arguments: Option<Vec<Node>>,
        extends: Option<Box<Node>>,
        implements: Option<Vec<Node>>,
        body: Box<Node>,
        attributes: Vec<Node>,
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
    DefineStatement {
        name: Box<Node>,
        value: Box<Node>,
        op: Token,
        cp: Token,
        token: Token,
        is_caseinsensitive: Option<Token>,
    },
    UnsetStatement {
        token: Token,
        op: Token,
        vars: Vec<Node>,
        cp: Token,
    },
    DieStatement {
        token: Token,
        op: Token,
        expr: Option<Box<Node>>,
        cp: Token,
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
        type_ref: Option<Box<Node>>,
        block: Box<Node>,
    },
    UseStatement {
        token: Token,
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
        traits_usages: Vec<Node>,
    },
    UseTrait {
        type_ref: Box<Node>,
    },
    UseTraitAlterationBlock {
        alteration_group_type_refs: Vec<Node>,
        oc: Token,
        alterations: Vec<Node>,
        cc: Token,
    },
    UseTraitInsteadOf {
        left: Option<Box<Node>>,
        paa: Option<Token>,
        member: Box<Node>,
        insteadof: Token,
        insteadof_list: Vec<Node>,
    },
    UseTraitAs {
        left: Option<Box<Node>>,
        paa: Option<Token>,
        member: Box<Node>,
        as_token: Token,
        visibility: Option<Token>,
        as_name: Option<Token>,
    },
    // Merge with Class?
    ClassStatement(ClassStatement),
    TraitStatement {
        token: Token,
        name: Token,
        body: Box<Node>,
        doc_comment: Option<Box<Node>>,
    },
    Interface {
        token: Token,
        name: Token,
        extends: Option<Vec<Node>>,
        body: Box<Node>,
        doc_comment: Option<Box<Node>>,
    },
    ClassConstantDefinitionStatement {
        token: Token,
        consts: Vec<Node>,
        doc_comment: Option<Box<Node>>,
        // The same token as below in the ClassConstant, replicated for the formatter
        visibility: Option<Token>,
        attributes: Vec<Node>,
    },
    ClassConstant {
        visibility: Option<Token>,
        name: Token,
        value: Box<Node>,
    },
    Property {
        name: Token,
        value: Option<Box<Node>>,
    },
    PropertyDefinitionStatement {
        properties: Vec<Node>,
        doc_comment: Option<Box<Node>>,
        visibility: Option<Token>,
        data_type: Option<Box<Node>>,
        is_static: Option<Token>,
        is_abstract: Option<Token>,
        attributes: Vec<Node>,
    },
    /// Method definition inside a class, interface or trait
    MethodDefinitionStatement {
        token: Token,
        is_final: Option<Token>,
        by_ref: Option<Token>,
        name: Token,
        visibility: Option<Token>,
        is_abstract: Option<Token>,
        function: Box<Node>,
        is_static: Option<Token>,
        doc_comment: Option<Box<Node>>,
        attributes: Vec<Node>,
    },
    /// Anonymous function
    FunctionDefinitionStatement {
        op: Token,
        arguments: Option<Vec<Node>>,
        cp: Token,
        return_type: Option<Box<Node>>,
        body: Option<Box<Node>>,
        doc_comment: Option<Box<Node>>,
    },
    /// Named function
    NamedFunctionDefinitionStatement {
        token: Token,
        by_ref: Option<Token>,
        name: Token,
        function: Box<Node>,
        attributes: Vec<Node>,
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
    AlternativeBlock {
        colon: Token,
        statements: Vec<Node>,
        terminator: Token,
    },
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
        body: Box<Node>,
    },
    SwitchBody {
        start: Token,
        branches: Vec<Node>,
        end: Token,
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
    Attribute {
        ats: Token,
        expressions: Vec<Node>,
        cb: Token,
    },
    EmptyScript {},
}

impl Node {
    pub fn is_offset(&self) -> bool {
        true
    }

    pub fn is_lvalue(&self) -> bool {
        true
    }

    /// Return a Vec of all the child nodes of this node.
    pub fn children(&self) -> Vec<&Node> {
        match self {
            Node::Binary { left, right, .. } => vec![left, right],
            Node::Unary { expr, .. }
            | Node::PostUnary { expr, .. }
            | Node::AliasedVariable { expr, .. }
            | Node::DynamicVariable { expr, .. }
            | Node::NamedParameter { expr, .. }
            | Node::YieldFrom { expr, .. } => vec![expr],
            Node::Ternary {
                check,
                true_arm,
                false_arm,
                ..
            } => {
                if let Some(true_arm) = true_arm {
                    vec![check, true_arm, false_arm]
                } else {
                    vec![check, false_arm]
                }
            }
            Node::UnsetStatement { vars: expr, .. }
            | Node::GlobalVariablesStatement { vars: expr, .. }
            | Node::Array { elements: expr, .. }
            | Node::OldArray { elements: expr, .. }
            | Node::List { elements: expr, .. }
            | Node::EchoStatement {
                expressions: expr, ..
            }
            | Node::PrintStatement {
                expressions: expr, ..
            }
            | Node::ConstStatement {
                constants: expr, ..
            }
            | Node::Static { expr, .. } => (*expr).iter().collect(),
            Node::ArrayElement { key, value, .. } => {
                if let Some(key) = key {
                    vec![key, value]
                } else {
                    vec![value]
                }
            }
            Node::Call {
                callee, parameters, ..
            } => {
                let mut children: Vec<&Node> = (*parameters).iter().collect();

                children.push(callee);

                children
            }
            Node::Isset { parameters, .. } | Node::Empty { parameters, .. } => {
                (*parameters).iter().collect()
            }
            Node::Exit { parameters, .. } | Node::Die { parameters, .. } => {
                if let Some(parameters) = parameters {
                    (*parameters).iter().collect()
                } else {
                    Vec::new()
                }
            }
            Node::Clone { object: class, .. } | Node::New { class, .. } => vec![class],
            Node::Member {
                object: class,
                member,
                ..
            }
            | Node::StaticMember {
                object: class,
                member,
                ..
            } => vec![class, member],
            Node::Field { array, index, .. } => {
                if let Some(index) = index {
                    vec![array, index]
                } else {
                    vec![array]
                }
            }
            Node::ArrowFunction {
                arguments, body, ..
            } => {
                if let Some(arguments) = arguments {
                    let mut children: Vec<&Node> = (*arguments).iter().collect();

                    children.push(body);

                    children
                } else {
                    vec![body]
                }
            }
            Node::Yield { expr, .. } => {
                if let Some(expr) = expr {
                    vec![expr]
                } else {
                    Vec::new()
                }
            }
            Node::FileInclude { resource, .. } => vec![resource],
            Node::ExpressionStatement { expression } | Node::ThrowStatement { expression, .. } => {
                vec![expression]
            }
            Node::DieStatement { expr, .. } => {
                if let Some(expr) = expr.as_ref() {
                    expr.children()
                } else {
                    vec![]
                }
            }
            Node::ReturnStatement { expression, .. } => {
                if let Some(expression) = expression {
                    vec![expression]
                } else {
                    vec![]
                }
            }
            Node::NamespaceStatement { type_ref, .. } => vec![type_ref],
            Node::StaticVariable { value, .. } => {
                if let Some(value) = value {
                    vec![value]
                } else {
                    vec![]
                }
            }
            Node::Function {
                arguments,
                uses,
                return_type,
                body,
                ..
            } => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(arguments) = arguments {
                    children.extend((*arguments).iter().collect::<Vec<&Node>>());
                }
                if let Some(uses) = uses {
                    children.extend((*uses).iter().collect::<Vec<&Node>>());
                }
                if let Some(return_type) = return_type {
                    children.push(return_type);
                }

                children.push(body);

                children
            }
            Node::FunctionArgument {
                argument_type,
                default_value,
                ..
            } => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(argument_type) = argument_type {
                    children.push(argument_type);
                }

                if let Some(default_value) = default_value {
                    children.push(default_value);
                }

                children
            }
            Node::Class {
                arguments,
                extends,
                implements,
                body,
                ..
            } => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(arguments) = arguments {
                    children.extend((*arguments).iter().collect::<Vec<&Node>>());
                }

                if let Some(extends) = extends.as_ref() {
                    children.push(extends);
                }

                if let Some(implements) = implements {
                    children.extend((*implements).iter().collect::<Vec<&Node>>());
                }

                children.push(body);

                children
            }
            Node::NamespaceBlock {
                type_ref, block, ..
            } => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(type_ref) = type_ref {
                    children.push(type_ref);
                }

                children.push(block);

                children
            }
            Node::ClassStatement(stmt) => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(doc_comment) = stmt.doc_comment.as_ref() {
                    children.push(doc_comment);
                }

                if let Some(extends) = stmt.extends.as_ref() {
                    children.push(extends);
                }

                if let Some(implements) = stmt.implements.as_ref() {
                    children.extend((*implements).iter().collect::<Vec<&Node>>());
                }

                children.push(stmt.body.as_ref());

                children
            }
            Node::TraitStatement { body, .. } => vec![body],
            Node::ClassConstantDefinitionStatement {
                attributes, consts, ..
            } => consts.iter().chain(attributes.iter()).collect(),
            Node::Const { value, .. } | Node::ClassConstant { value, .. } => {
                vec![value]
            }
            Node::Property { value, .. } => {
                if let Some(value) = value {
                    vec![value]
                } else {
                    vec![]
                }
            }
            Node::PropertyDefinitionStatement {
                data_type,
                properties,
                attributes,
                ..
            } => {
                let mut children: Vec<&Node> = Vec::with_capacity(properties.len() + 1);
                if let Some(data_type) = data_type.as_ref() {
                    children.push(data_type);
                }

                children.extend(
                    properties
                        .iter()
                        .chain(attributes.iter())
                        .collect::<Vec<&Node>>(),
                );

                children
            }
            Node::MethodDefinitionStatement {
                function,
                attributes,
                ..
            }
            | Node::NamedFunctionDefinitionStatement {
                function,
                attributes,
                ..
            } => {
                let mut children = attributes.iter().collect::<Vec<&Node>>();

                children.push(function);

                children
            }
            Node::FunctionDefinitionStatement {
                arguments,
                return_type,
                body,
                ..
            } => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(arguments) = arguments {
                    children.extend((*arguments).iter().collect::<Vec<&Node>>());
                }

                if let Some(return_type) = return_type {
                    children.push(return_type);
                }

                if let Some(body) = body {
                    children.push(body);
                }

                children
            }
            Node::Interface { extends, body, .. } => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(extends) = extends {
                    children.extend((*extends).iter().collect::<Vec<&Node>>());
                }

                children.push(body);

                children
            }
            Node::WhileStatement {
                condition, body, ..
            }
            | Node::DoWhileStatement {
                condition, body, ..
            } => vec![condition, body],
            Node::ForStatement {
                init,
                condition,
                step,
                body,
                ..
            } => {
                let mut children: Vec<&Node> = Vec::new();

                children.extend((*init).iter().collect::<Vec<&Node>>());
                children.extend((*condition).iter().collect::<Vec<&Node>>());
                children.extend((*step).iter().collect::<Vec<&Node>>());
                children.push(body);

                children
            }
            Node::ForEachStatement {
                collection,
                kv,
                body,
                ..
            } => vec![collection, kv, body],
            Node::Block { statements, .. } | Node::AlternativeBlock { statements, .. } => {
                (*statements).iter().collect()
            }
            Node::IfBranch {
                condition, body, ..
            } => vec![condition, body],
            Node::ElseBranch { body, .. } => vec![body],
            Node::IfStatement {
                if_branch,
                elseif_branches,
                else_branch,
            } => {
                let mut children: Vec<&Node> = vec![if_branch];
                children.extend((*elseif_branches).iter().collect::<Vec<&Node>>());
                if let Some(else_branch) = else_branch {
                    children.push(else_branch);
                }

                children
            }
            Node::SwitchBranch { cases, body } => {
                let mut children: Vec<&Node> = Vec::new();
                children.extend(
                    cases
                        .iter()
                        .filter(|c| c.is_some())
                        .map(|n| n.as_ref().unwrap())
                        .collect::<Vec<&Node>>(),
                );
                children.extend((*body).iter().collect::<Vec<&Node>>());

                children
            }
            Node::SwitchCase { expr, body, .. } => vec![expr, body],
            Node::SwitchBody { branches, .. } => (*branches).iter().collect::<Vec<&Node>>(),
            Node::TokenStatement { expr, .. } => {
                if let Some(expr) = expr {
                    vec![expr]
                } else {
                    Vec::new()
                }
            }
            Node::CatchBlock { body, .. } | Node::FinallyBlock { body, .. } => vec![body],
            Node::TryCatch {
                try_block,
                catch_blocks,
                finally_block,
                ..
            } => {
                let mut children: Vec<&Node> = vec![try_block];
                children.extend((*catch_blocks).iter().collect::<Vec<&Node>>());
                if let Some(finally_block) = finally_block {
                    children.push(finally_block);
                }

                children
            }
            Node::StaticVariablesStatement { assignments, .. } => {
                (*assignments).iter().collect::<Vec<&Node>>()
            }
            Node::UseDeclaration { declaration, .. } => vec![declaration],
            Node::UseStatement { imports, .. }
            | Node::UseFunctionStatement { imports, .. }
            | Node::UseConstStatement { imports, .. } => (*imports).iter().collect::<Vec<&Node>>(),
            Node::UseTraitStatement { traits_usages, .. } => {
                (*traits_usages).iter().collect::<Vec<&Node>>()
            }
            Node::DocComment {
                var_docs,
                properties,
                ..
            } => {
                let mut children: Vec<&Node> = (*var_docs).iter().collect::<Vec<&Node>>();

                children.extend((*properties).iter().collect::<Vec<&Node>>());

                children
            }
            Node::Grouping(node) => vec![node],
            Node::Attribute { expressions, .. } => (*expressions).iter().collect::<Vec<&Node>>(),
            Node::DataType { type_refs, .. } => (*type_refs).iter().collect::<Vec<&Node>>(),
            Node::Match {
                condition, body, ..
            } => {
                let mut children: Vec<&Node> = vec![condition];

                children.extend((*body).iter().collect::<Vec<&Node>>());

                children
            }
            Node::MatchArm {
                patterns,
                expression,
                ..
            } => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(patterns) = patterns {
                    children.extend(patterns.iter());
                }

                children.push(expression);
                children
            }
            _ => Vec::new(),
        }
    }
}
impl Node {
    pub fn end(&self) -> Token {
        match self {
            Node::Block { cc, .. } => cc.clone(),
            Node::FunctionDefinitionStatement { body, cp, .. } => {
                if let Some(body) = body {
                    return body.end();
                }

                cp.clone()
            }
            _ => unimplemented!("Implement end for token type {:?}", self),
        }
    }

    pub fn range(&self) -> NodeRange {
        match self {
            Node::Unary { token, expr } => (token, expr.as_ref()).into(),
            Node::PostUnary { token, expr } => (expr.as_ref(), token).into(),
            Node::Const { token, value, .. } => (token, value.as_ref()).into(),
            Node::Binary { left, right, .. } => (left.as_ref(), right.as_ref()).into(),
            Node::Ternary {
                check, false_arm, ..
            } => (check.as_ref(), false_arm.as_ref()).into(),
            Node::LexicalVariable {
                reference,
                variable,
            } => {
                if let Some(reference) = reference {
                    (reference, variable).into()
                } else {
                    variable.into()
                }
            }
            Node::AliasedVariable { variable, expr } => (variable, expr.as_ref()).into(),
            Node::DynamicVariable { variable, cc, .. } => (variable, cc).into(),
            Node::StaticVariable {
                variable, value, ..
            } => {
                if let Some(value) = value {
                    (variable, value.as_ref()).into()
                } else {
                    variable.into()
                }
            }
            Node::Array { ob, cb, .. } => (ob, cb).into(),
            Node::OldArray { token, cp, .. } => (token, cp).into(),
            Node::ArrayElement { key, value, .. } => {
                if let Some(key) = key {
                    (key.as_ref(), value.as_ref()).into()
                } else {
                    value.range()
                }
            }
            Node::List { token, cp, .. } => (token, cp).into(),
            Node::Call { callee, cp, .. } => (callee.as_ref(), cp).into(),
            Node::Isset { isset, cp, .. } => (isset, cp).into(),
            Node::Empty { empty, cp, .. } => (empty, cp).into(),
            Node::Exit { exit, cp, .. } => {
                if let Some(cp) = cp {
                    (exit, cp).into()
                } else {
                    exit.range()
                }
            }
            Node::HaltCompiler { hc, cp, .. } => {
                if let Some(cp) = cp {
                    (hc, cp).into()
                } else {
                    hc.range()
                }
            }
            Node::Die { die, cp, .. } => {
                if let Some(cp) = cp {
                    (die, cp).into()
                } else {
                    die.range()
                }
            }
            Node::New { token, class } => (token, class.as_ref()).into(),
            Node::Clone { token, object } => (token, object.as_ref()).into(),
            Node::Member { object, member, .. } => (object.as_ref(), member.as_ref()).into(),
            Node::StaticMember {
                object: class,
                member,
                cc,
                ..
            } => {
                if let Some(cc) = cc {
                    (class.as_ref(), cc).into()
                } else {
                    (class.as_ref(), member.as_ref()).into()
                }
            }
            Node::Field { array, cb, .. } => (array.as_ref(), cb).into(),
            Node::Static { token, expr } => (token, expr.last().unwrap()).into(),
            Node::Function {
                is_static,
                by_ref,
                token,
                body,
                ..
            } => {
                if let Some(is_static) = is_static {
                    (is_static, body.as_ref()).into()
                } else if let Some(by_ref) = by_ref {
                    (by_ref, body.as_ref()).into()
                } else {
                    (token, body.as_ref()).into()
                }
            }
            Node::ArrowFunction {
                is_static,
                by_ref,
                token,
                body,
                ..
            } => {
                if let Some(is_static) = is_static {
                    (is_static, body.as_ref()).into()
                } else if let Some(by_ref) = by_ref {
                    (by_ref, body.as_ref()).into()
                } else {
                    (token, body.as_ref()).into()
                }
            }
            Node::FunctionArgument {
                argument_type,
                name,
                spread,
                default_value,
                reference,
                ..
            } => {
                let start: NodeRange = if let Some(reference) = reference {
                    reference.into()
                } else if let Some(argument_type) = argument_type {
                    argument_type.as_ref().range()
                } else if let Some(spread) = spread {
                    spread.into()
                } else {
                    name.into()
                };

                if let Some(default_value) = default_value {
                    NodeRange::from_range(&start, &default_value.as_ref().range())
                } else {
                    NodeRange::from_range(&start, &name.into())
                }
            }
            Node::DataType {
                nullable,
                type_refs,
            } => {
                if let Some(nullable) = nullable {
                    (nullable, type_refs.last().unwrap()).into()
                } else {
                    (type_refs.first().unwrap(), type_refs.last().unwrap()).into()
                }
            }
            Node::ReturnType { data_type, .. } => data_type.range(),
            Node::Class { token, body, .. } => (token, body.as_ref()).into(),
            Node::Yield { token, expr } => {
                if let Some(expr) = expr {
                    (token, expr.as_ref()).into()
                } else {
                    token.range()
                }
            }
            Node::YieldFrom { token, expr } => (token, expr.as_ref()).into(),
            Node::FileInclude { token, resource } => (token, resource.as_ref()).into(),
            Node::UseDeclaration {
                token,
                declaration,
                alias,
                ..
            } => {
                let start: NodeRange = if let Some(token) = token {
                    token.into()
                } else {
                    declaration.as_ref().range()
                };

                if let Some(alias) = alias {
                    NodeRange::from_range(&start, &alias.into())
                } else {
                    NodeRange::from_range(&start, &declaration.as_ref().range())
                }
            }
            Node::UseConst {
                token,
                constant,
                alias,
                ..
            } => {
                let start: NodeRange = if let Some(token) = token {
                    token.into()
                } else {
                    constant.range()
                };

                if let Some(alias) = alias {
                    NodeRange::from_range(&start, &alias.into())
                } else {
                    constant.range()
                }
            }
            Node::UseFunction {
                token,
                function,
                alias,
                ..
            } => {
                let start: NodeRange = if let Some(token) = token {
                    token.into()
                } else {
                    function.range()
                };

                if let Some(alias) = alias {
                    NodeRange::from_range(&start, &alias.into())
                } else {
                    function.range()
                }
            }
            Node::GroupedUse { token, cc, .. } => (token, cc).into(),
            Node::ExpressionStatement { expression } => expression.range(),
            Node::EchoStatement { token, expressions } => {
                (token, expressions.last().unwrap()).into()
            }
            Node::ConstStatement { token, constants } => (token, constants.last().unwrap()).into(),
            Node::PrintStatement { token, expressions } => {
                (token, expressions.last().unwrap()).into()
            }
            Node::GotoStatement { token, label } => (token, label).into(),
            Node::LabelStatement { label, colon } => (label, colon).into(),
            Node::ThrowStatement {
                token, expression, ..
            } => (token, expression.as_ref()).into(),
            Node::DeclareStatement { token, cp, .. } => (token, cp).into(),
            Node::UnsetStatement { token, cp, .. } => (token, cp).into(),
            Node::DieStatement { token, cp, .. } => (token, cp).into(),
            Node::ReturnStatement { token, expression } => {
                if let Some(expr) = expression {
                    (token, expr.as_ref()).into()
                } else {
                    token.range()
                }
            }
            Node::NamespaceStatement { token, type_ref } => (token, type_ref.as_ref()).into(),
            Node::NamespaceBlock { token, block, .. } => (token, block.as_ref()).into(),
            Node::UseStatement { token, imports } => (token, imports.last().unwrap()).into(),
            Node::UseFunctionStatement { token, imports } => {
                (token, imports.last().unwrap()).into()
            }
            Node::UseConstStatement { token, imports } => (token, imports.last().unwrap()).into(),
            Node::UseTraitStatement {
                token,
                traits_usages,
            } => (token, traits_usages.last().unwrap()).into(),
            Node::UseTrait { type_ref } => type_ref.range(),
            Node::UseTraitAlterationBlock { oc, cc, .. } => (oc, cc).into(),
            Node::UseTraitInsteadOf {
                left,
                member,
                insteadof_list,
                ..
            } => {
                if let Some(left) = left {
                    (left.as_ref(), insteadof_list.last().unwrap()).into()
                } else {
                    (member.as_ref(), insteadof_list.last().unwrap()).into()
                }
            }
            Node::UseTraitAs {
                left,
                member,
                as_name,
                visibility,
                ..
            } => {
                if let Some(left) = left {
                    if let Some(as_name) = as_name {
                        (left.as_ref(), as_name).into()
                    } else if let Some(visibility) = visibility {
                        (left.as_ref(), visibility).into()
                    } else {
                        left.range()
                    }
                } else if let Some(as_name) = as_name {
                    (member.as_ref(), as_name).into()
                } else if let Some(visibility) = visibility {
                    (member.as_ref(), visibility).into()
                } else {
                    member.range()
                }
            }
            Node::ClassStatement(stmt) => {
                if let Some(doc_comment) = stmt.doc_comment.as_ref() {
                    (doc_comment.as_ref(), stmt.body.as_ref()).into()
                } else if let Some(is_abstract) = stmt.is_abstract.as_ref() {
                    (is_abstract, stmt.body.as_ref()).into()
                } else if let Some(is_final) = stmt.is_final.as_ref() {
                    (is_final, stmt.body.as_ref()).into()
                } else {
                    (&stmt.token, stmt.body.as_ref()).into()
                }
            }
            Node::TraitStatement { token, body, .. } => (token, body.as_ref()).into(),
            Node::Interface { token, body, .. } => (token, body.as_ref()).into(),
            Node::ClassConstant { name, value, .. } => (name, value.as_ref()).into(),
            Node::ClassConstantDefinitionStatement { consts, .. } => {
                (consts.first().unwrap(), consts.last().unwrap()).into()
            }
            Node::Property { name, value } => {
                if let Some(value) = value {
                    (name, value.as_ref()).into()
                } else {
                    name.range()
                }
            }
            Node::PropertyDefinitionStatement {
                data_type,
                properties,
                ..
            } => {
                if let Some(data_type) = data_type {
                    (data_type.as_ref(), properties.last().unwrap()).into()
                } else {
                    (properties.first().unwrap(), properties.last().unwrap()).into()
                }
            }
            Node::MethodDefinitionStatement {
                is_abstract,
                is_final,
                visibility,
                function,
                token,
                ..
            } => {
                if let Some(is_abstract) = is_abstract {
                    (is_abstract, function.as_ref()).into()
                } else if let Some(is_final) = is_final {
                    (is_final, function.as_ref()).into()
                } else if let Some(visibility) = visibility {
                    (visibility, function.as_ref()).into()
                } else {
                    (token, function.as_ref()).into()
                }
            }
            Node::FunctionDefinitionStatement {
                op,
                cp,
                return_type,
                body,
                ..
            } => {
                if let Some(body) = body {
                    (op, body.as_ref()).into()
                } else if let Some(return_type) = return_type {
                    (op, return_type.as_ref()).into()
                } else {
                    (op, cp).into()
                }
            }
            Node::NamedFunctionDefinitionStatement {
                token,
                function,
                by_ref,
                ..
            } => {
                if let Some(by_ref) = by_ref {
                    (by_ref, function.as_ref()).into()
                } else {
                    (token, function.as_ref()).into()
                }
            }
            Node::WhileStatement { token, body, .. } => (token, body.as_ref()).into(),
            Node::DoWhileStatement { do_token, cp, .. } => (do_token, cp).into(),
            Node::ForStatement { token, body, .. } => (token, body.as_ref()).into(),
            Node::ForEachStatement { token, body, .. } => (token, body.as_ref()).into(),
            Node::Block { oc, cc, .. } => (oc, cc).into(),
            Node::AlternativeBlock {
                colon, terminator, ..
            } => (colon, terminator).into(),
            Node::IfBranch { token, body, .. } => (token, body.as_ref()).into(),
            Node::ElseBranch { token, body, .. } => (token, body.as_ref()).into(),
            Node::IfStatement {
                if_branch,
                elseif_branches,
                else_branch,
            } => {
                if let Some(else_branch) = else_branch {
                    (if_branch.as_ref(), else_branch.as_ref()).into()
                } else if !elseif_branches.is_empty() {
                    (if_branch.as_ref(), elseif_branches.last().unwrap()).into()
                } else {
                    if_branch.range()
                }
            }
            Node::SwitchBranch { body, .. } => (body.first().unwrap(), body.last().unwrap()).into(),
            Node::SwitchCase { token, body, .. } => (token, body.as_ref()).into(),
            Node::SwitchBody { start, end, .. } => (start, end).into(),
            Node::TokenStatement { token, expr } => {
                if let Some(expr) = expr {
                    (token, expr.as_ref()).into()
                } else {
                    token.into()
                }
            }
            Node::CatchBlock { token, body, .. } => (token, body.as_ref()).into(),
            Node::FinallyBlock { token, body, .. } => (token, body.as_ref()).into(),
            Node::TryCatch {
                token,
                try_block,
                catch_blocks,
                finally_block,
            } => {
                if let Some(finally_block) = finally_block {
                    (token, finally_block.as_ref()).into()
                } else if !catch_blocks.is_empty() {
                    (token, catch_blocks.last().unwrap()).into()
                } else {
                    (token, try_block.as_ref()).into()
                }
            }
            Node::StaticVariablesStatement { token, assignments } => {
                (token, assignments.last().unwrap()).into()
            }
            Node::GlobalVariablesStatement { token, vars } => (token, vars.last().unwrap()).into(),
            Node::TypeRef(tokens) => tokens.range(),
            Node::Literal(token) | Node::Variable(token) => token.range(),
            Node::Missing(token) => token.range(),
            Node::DefineStatement { token, cp, .. } => (token, cp).into(),
            Node::DocComment { comment, .. } => comment.range(),
            Node::DocCommentProperty { name, types, .. } => {
                if let Some(types) = types {
                    (name, types.last().unwrap()).into()
                } else {
                    name.into()
                }
            }
            Node::Grouping(content) => content.range(),
            Node::Match { mtch, oc, cc, .. } => (oc, cc).into(),
            Node::MatchArm {
                patterns,
                expression,
                ..
            } => {
                if let Some(patterns) = patterns {
                    (patterns.first().unwrap(), expression.as_ref()).into()
                } else {
                    expression.range()
                }
            }
            _ => {
                eprintln!("Implement range for {:?}!", self);
                ((1, 1), (1, 1)).into()
            }
        }
    }

    pub fn name(&self) -> String {
        match self {
            Node::NamedFunctionDefinitionStatement { name, .. } => name.to_string(),
            Node::Variable(token) | Node::Literal(token) => token.to_string(),
            Node::TypeRef(type_ref) => type_ref.into(),
            _ => String::new(),
        }
    }

    pub fn normalized_name(&self) -> String {
        self.name().to_lowercase()
    }

    // TODO: Add other boundaries
    pub fn scope_boundary(&self) -> bool {
        matches!(self, Node::Function { .. })
    }

    /// Returns all descendants of this node in a DFS manner
    pub fn descendants(&self) -> Vec<&Node> {
        let mut descendants = Vec::new();

        for c in self.children() {
            descendants.push(c);
            descendants.extend(c.descendants());
        }

        descendants
    }
}

impl From<&Node> for String {
    fn from(node: &Node) -> Self {
        match node {
            Node::ClassStatement(stmt) => {
                format!(
                    "{} {}{}",
                    stmt.token,
                    stmt.name,
                    String::from(stmt.body.as_ref())
                )
            }
            Node::Block { oc, cc, statements } => {
                format!(
                    "{}{}{}",
                    oc,
                    statements.iter().map(String::from).collect::<String>(),
                    cc
                )
            }
            _ => String::new(),
        }
    }
}
