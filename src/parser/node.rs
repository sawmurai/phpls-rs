use std::{iter::Skip, slice::Iter};

use super::token::{Token, TokenType};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct TypeRef {
    kind: Vec<Token>,

    /// Indicator that we are dealing with an array of that type
    /// This is used when parsing phpDoc comments a la Class[] or Array<Class>
    multiple: bool,
}

pub type NodeRange = ((u32, u32), (u32, u32));

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
        (
            self.kind.first().unwrap().range().0,
            self.kind.last().unwrap().range().1,
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
    ClassStatement {
        token: Token,
        name: Token,
        is_abstract: Option<Token>,
        is_final: Option<Token>,
        implements: Option<Vec<Node>>,
        extends: Option<Box<Node>>,
        body: Box<Node>,
        doc_comment: Option<Box<Node>>,
        attributes: Vec<Node>,
    },
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
            Node::ClassStatement {
                extends,
                implements,
                body,
                doc_comment,
                ..
            } => {
                let mut children: Vec<&Node> = Vec::new();

                if let Some(doc_comment) = doc_comment {
                    children.push(doc_comment);
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
            Node::Unary { token, expr } => (token.start(), expr.range().1),
            Node::PostUnary { token, expr } => (expr.range().0, token.end()),
            Node::Const { token, value, .. } => (token.start(), value.range().1),
            Node::Binary { left, right, .. } => (left.range().0, right.range().1),
            Node::Ternary {
                check, false_arm, ..
            } => (check.range().0, false_arm.range().1),
            Node::LexicalVariable {
                reference,
                variable,
            } => {
                if let Some(reference) = reference {
                    (reference.start(), variable.end())
                } else {
                    variable.range()
                }
            }
            Node::AliasedVariable { variable, expr } => (variable.start(), expr.range().1),
            Node::DynamicVariable { variable, cc, .. } => (variable.start(), cc.end()),
            Node::StaticVariable {
                variable, value, ..
            } => {
                if let Some(value) = value {
                    (variable.start(), value.range().1)
                } else {
                    variable.range()
                }
            }
            Node::Array { ob, cb, .. } => (ob.start(), cb.end()),
            Node::OldArray { token, cp, .. } => (token.start(), cp.end()),
            Node::ArrayElement { key, value, .. } => {
                if let Some(key) = key {
                    (key.range().0, value.range().0)
                } else {
                    value.range()
                }
            }
            Node::List { token, cp, .. } => (token.start(), cp.end()),
            Node::Call { callee, cp, .. } => (callee.range().0, cp.end()),
            Node::Isset { isset, cp, .. } => (isset.start(), cp.end()),
            Node::Empty { empty, cp, .. } => (empty.start(), cp.end()),
            Node::Exit { exit, cp, .. } => {
                if let Some(cp) = cp {
                    (exit.start(), cp.end())
                } else {
                    exit.range()
                }
            }
            Node::HaltCompiler { hc, cp, .. } => {
                if let Some(cp) = cp {
                    (hc.start(), cp.end())
                } else {
                    hc.range()
                }
            }
            Node::Die { die, cp, .. } => {
                if let Some(cp) = cp {
                    (die.start(), cp.end())
                } else {
                    die.range()
                }
            }
            Node::New { token, class } => (token.start(), class.range().1),
            Node::Clone { token, object } => (token.start(), object.range().1),
            Node::Member { object, member, .. } => (object.range().0, member.range().1),
            Node::StaticMember {
                object: class,
                member,
                cc,
                ..
            } => {
                if let Some(cc) = cc {
                    (class.range().0, cc.range().1)
                } else {
                    (class.range().0, member.range().1)
                }
            }
            Node::Field { array, cb, .. } => (array.range().0, cb.end()),
            Node::Static { token, expr } => (token.start(), expr.last().unwrap().range().1),
            Node::Function {
                is_static,
                by_ref,
                token,
                body,
                ..
            } => {
                if let Some(is_static) = is_static {
                    (is_static.start(), body.range().1)
                } else if let Some(by_ref) = by_ref {
                    (by_ref.start(), body.range().1)
                } else {
                    (token.start(), body.range().1)
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
                    (is_static.start(), body.range().1)
                } else if let Some(by_ref) = by_ref {
                    (by_ref.start(), body.range().1)
                } else {
                    (token.start(), body.range().1)
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
                let start = if let Some(reference) = reference {
                    reference.start()
                } else if let Some(argument_type) = argument_type {
                    argument_type.range().0
                } else if let Some(spread) = spread {
                    spread.start()
                } else {
                    name.start()
                };

                if let Some(default_value) = default_value {
                    (start, default_value.range().1)
                } else {
                    (start, name.end())
                }
            }
            Node::DataType {
                nullable,
                type_refs,
            } => {
                if let Some(nullable) = nullable {
                    (nullable.start(), type_refs.last().unwrap().range().1)
                } else {
                    (
                        type_refs.first().unwrap().range().0,
                        type_refs.last().unwrap().range().1,
                    )
                }
            }
            Node::ReturnType { data_type, .. } => data_type.range(),
            Node::Class { token, body, .. } => (token.start(), body.range().1),
            Node::Yield { token, expr } => {
                if let Some(expr) = expr {
                    (token.start(), expr.range().1)
                } else {
                    token.range()
                }
            }
            Node::YieldFrom { token, expr } => (token.start(), expr.range().1),
            Node::FileInclude { token, resource } => (token.start(), resource.range().1),
            Node::UseDeclaration {
                token,
                declaration,
                alias,
                ..
            } => {
                let start = if let Some(token) = token {
                    token.start()
                } else {
                    declaration.range().0
                };

                if let Some(alias) = alias {
                    (start, alias.range().1)
                } else {
                    (start, declaration.range().1)
                }
            }
            Node::UseConst {
                token,
                constant,
                alias,
                ..
            } => {
                let start = if let Some(token) = token {
                    token.start()
                } else {
                    constant.range().0
                };

                if let Some(alias) = alias {
                    (start, alias.range().1)
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
                let start = if let Some(token) = token {
                    token.start()
                } else {
                    function.range().0
                };

                if let Some(alias) = alias {
                    (start, alias.range().1)
                } else {
                    function.range()
                }
            }
            Node::GroupedUse { token, cc, .. } => (token.start(), cc.end()),
            Node::ExpressionStatement { expression } => expression.range(),
            Node::EchoStatement { token, expressions } => {
                (token.start(), expressions.last().unwrap().range().1)
            }
            Node::ConstStatement { token, constants } => {
                (token.start(), constants.last().unwrap().range().1)
            }
            Node::PrintStatement { token, expressions } => {
                (token.start(), expressions.last().unwrap().range().1)
            }
            Node::GotoStatement { token, label } => (token.start(), label.end()),
            Node::LabelStatement { label, colon } => (label.start(), colon.end()),
            Node::ThrowStatement {
                token, expression, ..
            } => (token.start(), expression.range().1),
            Node::DeclareStatement { token, cp, .. } => (token.start(), cp.end()),
            Node::UnsetStatement { token, cp, .. } => (token.start(), cp.end()),
            Node::DieStatement { token, cp, .. } => (token.start(), cp.end()),
            Node::ReturnStatement { token, expression } => {
                if let Some(expr) = expression {
                    (token.start(), expr.range().1)
                } else {
                    token.range()
                }
            }
            Node::NamespaceStatement { token, type_ref } => (token.start(), type_ref.range().1),
            Node::NamespaceBlock { token, block, .. } => (token.start(), block.range().1),
            Node::UseStatement { token, imports } => {
                (token.start(), imports.last().unwrap().range().1)
            }
            Node::UseFunctionStatement { token, imports } => {
                (token.start(), imports.last().unwrap().range().1)
            }
            Node::UseConstStatement { token, imports } => {
                (token.start(), imports.last().unwrap().range().1)
            }
            Node::UseTraitStatement {
                token,
                traits_usages,
            } => (token.start(), traits_usages.last().unwrap().range().1),
            Node::UseTrait { type_ref } => type_ref.range(),
            Node::UseTraitAlterationBlock { oc, cc, .. } => (oc.start(), cc.end()),
            Node::UseTraitInsteadOf {
                left,
                member,
                insteadof_list,
                ..
            } => {
                if let Some(left) = left {
                    (left.range().0, insteadof_list.last().unwrap().range().1)
                } else {
                    (member.range().0, insteadof_list.last().unwrap().range().1)
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
                        (left.range().0, as_name.end())
                    } else if let Some(visibility) = visibility {
                        (left.range().0, visibility.end())
                    } else {
                        left.range()
                    }
                } else if let Some(as_name) = as_name {
                    (member.range().0, as_name.end())
                } else if let Some(visibility) = visibility {
                    (member.range().0, visibility.end())
                } else {
                    member.range()
                }
            }
            Node::ClassStatement {
                doc_comment,
                is_abstract,
                is_final,
                token,
                body,
                ..
            } => {
                if let Some(doc_comment) = doc_comment {
                    (doc_comment.range().0, body.range().1)
                } else if let Some(is_abstract) = is_abstract {
                    (is_abstract.start(), body.range().1)
                } else if let Some(is_final) = is_final {
                    (is_final.start(), body.range().1)
                } else {
                    (token.start(), body.range().1)
                }
            }
            Node::TraitStatement { token, body, .. } => (token.start(), body.range().1),
            Node::Interface { token, body, .. } => (token.start(), body.range().1),
            Node::ClassConstant { name, value, .. } => (name.start(), value.range().1),
            Node::ClassConstantDefinitionStatement { consts, .. } => (
                consts.first().unwrap().range().0,
                consts.last().unwrap().range().1,
            ),
            Node::Property { name, value } => {
                if let Some(value) = value {
                    (name.range().0, value.range().1)
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
                    (data_type.range().0, properties.last().unwrap().range().1)
                } else {
                    (
                        properties.first().unwrap().range().0,
                        properties.last().unwrap().range().1,
                    )
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
                    (is_abstract.start(), function.range().1)
                } else if let Some(is_final) = is_final {
                    (is_final.start(), function.range().1)
                } else if let Some(visibility) = visibility {
                    (visibility.start(), function.range().1)
                } else {
                    (token.start(), function.range().1)
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
                    (op.start(), body.range().1)
                } else if let Some(return_type) = return_type {
                    (op.start(), return_type.range().1)
                } else {
                    (op.start(), cp.end())
                }
            }
            Node::NamedFunctionDefinitionStatement {
                token,
                function,
                by_ref,
                ..
            } => {
                if let Some(by_ref) = by_ref {
                    (by_ref.start(), function.range().1)
                } else {
                    (token.start(), function.range().1)
                }
            }
            Node::WhileStatement { token, body, .. } => (token.start(), body.range().1),
            Node::DoWhileStatement { do_token, cp, .. } => (do_token.start(), cp.end()),
            Node::ForStatement { token, body, .. } => (token.start(), body.range().1),
            Node::ForEachStatement { token, body, .. } => (token.start(), body.range().1),
            Node::Block { oc, cc, .. } => (oc.start(), cc.range().1),
            Node::AlternativeBlock {
                colon, terminator, ..
            } => (colon.start(), terminator.range().1),
            Node::IfBranch { token, body, .. } => (token.start(), body.range().1),
            Node::ElseBranch { token, body, .. } => (token.start(), body.range().1),
            Node::IfStatement {
                if_branch,
                elseif_branches,
                else_branch,
            } => {
                if let Some(else_branch) = else_branch {
                    (if_branch.range().0, else_branch.range().1)
                } else if !elseif_branches.is_empty() {
                    (
                        if_branch.range().0,
                        elseif_branches.last().unwrap().range().1,
                    )
                } else {
                    if_branch.range()
                }
            }
            Node::SwitchBranch { body, .. } => (
                body.first().unwrap().range().0,
                body.last().unwrap().range().1,
            ),
            Node::SwitchCase { token, body, .. } => (token.start(), body.range().1),
            Node::SwitchBody { start, end, .. } => (start.start(), end.end()),
            Node::TokenStatement { token, expr } => {
                if let Some(expr) = expr {
                    (token.start(), expr.range().1)
                } else {
                    token.range()
                }
            }
            Node::CatchBlock { token, body, .. } => (token.start(), body.range().1),
            Node::FinallyBlock { token, body, .. } => (token.start(), body.range().1),
            Node::TryCatch {
                token,
                try_block,
                catch_blocks,
                finally_block,
            } => {
                if let Some(finally_block) = finally_block {
                    (token.start(), finally_block.range().1)
                } else if !catch_blocks.is_empty() {
                    (token.start(), catch_blocks.last().unwrap().range().1)
                } else {
                    (token.start(), try_block.range().1)
                }
            }
            Node::StaticVariablesStatement { token, assignments } => {
                (token.start(), assignments.last().unwrap().range().1)
            }
            Node::GlobalVariablesStatement { token, vars } => {
                (token.start(), vars.last().unwrap().range().1)
            }
            Node::TypeRef(tokens) => tokens.range(),
            Node::Literal(token) | Node::Variable(token) => token.range(),
            Node::Missing(token) => token.range(),
            Node::DefineStatement { token, cp, .. } => (token.range().0, cp.range().1),
            Node::DocComment { comment, .. } => comment.range(),
            Node::DocCommentProperty { name, types, .. } => {
                if let Some(types) = types {
                    (name.range().0, types.last().unwrap().range().1)
                } else {
                    name.range()
                }
            }
            Node::Grouping(content) => content.range(),
            _ => {
                eprintln!("Implement range for {:?}!", self);
                ((1, 1), (1, 1))
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
            Node::ClassStatement {
                token, name, body, ..
            } => format!("{} {}{}", token, name, String::from(body.as_ref())),
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
