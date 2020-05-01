use crate::token::Token;
use std::sync::Mutex;

use std::sync::{Arc, Weak};
use tower_lsp::lsp_types::{DocumentSymbol, Position, Range, SymbolKind};

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
        by_ref: Option<Token>,
        token: Token,
        op: Token,
        arguments: Option<Vec<Node>>,
        cp: Token,
        uses: Option<Vec<Node>>,
        return_type: Box<Option<Node>>,
        body: Box<Node>,
    },
    ArrowFunction {
        is_static: Option<Token>,
        by_ref: Option<Token>,
        token: Token,
        op: Token,
        arguments: Option<Vec<Node>>,
        cp: Token,
        arrow: Token,
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
    DataType {
        nullable: Option<Token>,
        type_ref: Box<Node>,
    },
    ReturnType {
        token: Token,
        data_type: Box<Node>,
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
        token: Token,
        op: Token,
        vars: Vec<Node>,
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
        type_ref: Box<Option<Node>>,
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
        as_name: Token,
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
        data_type: Option<std::sync::Arc<Node>>,
        visibility: Option<Token>,
        is_abstract: Option<Token>,
        value: Option<Box<Node>>,
        is_static: Option<Token>,
    },
    MethodDefinitionStatement {
        token: Token,
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

pub type NodeRange = ((u16, u16), (u16, u16));

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
            Node::StaticMember { class, member, .. } => (class.range().0, member.range().1),
            Node::StaticMethod { class, method, .. } => (class.range().0, method.range().1),
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
                } else if let Some(argument_type) = &**argument_type {
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
            Node::ArgumentType { nullable, type_ref } => {
                if let Some(nullable) = nullable {
                    (nullable.start(), type_ref.range().1)
                } else {
                    type_ref.range()
                }
            }
            Node::DataType { nullable, type_ref } => {
                if let Some(nullable) = nullable {
                    (nullable.start(), type_ref.range().1)
                } else {
                    type_ref.range()
                }
            }
            Node::ReturnType { token, data_type } => (token.start(), data_type.range().1),
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
                    declaration.range()
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
                ..
            } => {
                if let Some(left) = left {
                    (left.range().0, as_name.end())
                } else {
                    (member.range().0, as_name.end())
                }
            }
            Node::ClassStatement {
                is_abstract,
                is_final,
                token,
                body,
                ..
            } => {
                if let Some(is_abstract) = is_abstract {
                    (is_abstract.start(), body.range().1)
                } else if let Some(is_final) = is_final {
                    (is_final.start(), body.range().1)
                } else {
                    (token.start(), body.range().1)
                }
            }
            Node::TraitStatement { token, body, .. } => (token.start(), body.range().1),
            Node::Interface { token, body, .. } => (token.start(), body.range().1),
            Node::ClassConstantDefinitionStatement { name, value, .. } => {
                (name.start(), value.range().1)
            }
            // TODO: Extend
            Node::PropertyDefinitionStatement { name, .. } => name.range(),
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
                } else if let Some(return_type) = &**return_type {
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
                } else if elseif_branches.len() > 0 {
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
                } else if catch_blocks.len() > 0 {
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
            _ => ((1, 1), (1, 1)),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct SymbolImport {
    pub path: Vec<Token>,
    alias: Option<Token>,
}

/// Collect symbol imports underneath the current node
pub fn collect_uses(node: &Node, prefix: &Vec<Token>) -> Vec<SymbolImport> {
    let mut collected_uses = Vec::new();

    match node {
        Node::UseStatement { imports, .. } => {
            imports
                .iter()
                .for_each(|n| collected_uses.extend(collect_uses(n, prefix)));
        }
        Node::UseFunctionStatement { imports, .. } => {
            imports
                .iter()
                .for_each(|n| collected_uses.extend(collect_uses(n, prefix)));
        }
        Node::UseConstStatement { imports, .. } => {
            imports
                .iter()
                .for_each(|n| collected_uses.extend(collect_uses(n, prefix)));
        }
        Node::GroupedUse { parent, uses, .. } => {
            uses.iter().for_each(|n| {
                collected_uses.extend(collect_uses(n, &collect_uses(parent, prefix)[0].path))
            });
        }
        Node::UseDeclaration {
            declaration, alias, ..
        } => {
            let import = &collect_uses(declaration, prefix)[0];

            collected_uses.push(SymbolImport {
                alias: alias.clone(),
                ..import.clone()
            });
        }
        Node::UseFunction {
            function, alias, ..
        } => {
            let import = &collect_uses(function, prefix)[0];

            collected_uses.push(SymbolImport {
                alias: alias.clone(),
                ..import.clone()
            });
        }
        Node::UseConst {
            constant, alias, ..
        } => {
            let import = &collect_uses(constant, prefix)[0];

            collected_uses.push(SymbolImport {
                alias: alias.clone(),
                ..import.clone()
            });
        }
        Node::TypeRef(tokens) => {
            let mut ns = prefix.clone();
            ns.extend(tokens.clone());
            collected_uses.push(SymbolImport {
                path: ns,
                alias: None,
            });
        }
        _ => {}
    }

    collected_uses
}

pub struct Usage {
    range: Range,
    symbol: usize,
}

#[derive(Default)]
pub struct Scope {
    pub definitions: Vec<DocumentSymbol>,
    usages: Arc<Mutex<Vec<Usage>>>,

    // We own our parents and write usages into them
    parent: Option<Arc<Mutex<Scope>>>,

    // Weak reference, as we never write into our children
    children: Vec<Weak<Mutex<Scope>>>,
}

impl Scope {
    pub fn within(parent: Arc<Mutex<Self>>) -> Arc<Mutex<Self>> {
        let new = Self {
            parent: Some(parent.clone()),
            ..Default::default()
        };

        let new = Arc::new(Mutex::new(new));

        parent.lock().unwrap().children.push(Arc::downgrade(&new));

        new
    }

    pub fn definition(&mut self, symbol: DocumentSymbol) {
        self.definitions.push(symbol);
    }

    pub fn get_definitions(&self) -> Option<Vec<DocumentSymbol>> {
        if self.definitions.is_empty() {
            None
        } else {
            Some(self.definitions.clone())
        }
    }

    pub fn usage(&mut self, symbol: &DocumentSymbol) -> Result<(), String> {
        for (i, def) in self.definitions.iter().enumerate() {
            if def.kind == symbol.kind && def.name == symbol.name {
                self.usages.lock().unwrap().push(Usage {
                    range: symbol.range,
                    symbol: i,
                });

                return Ok(());
            }
        }

        if let Some(parent) = self.parent.as_ref() {
            return parent.lock().unwrap().usage(symbol);
        }

        Err("Use of undefined symbol".to_owned())
    }
}

pub fn collect_symbols(node: &Node, scope: Arc<Mutex<Scope>>) -> Result<(), String> {
    match node {
        Node::Binary { left, right, .. } => {
            collect_symbols(left, scope.clone())?;
            collect_symbols(right, scope.clone())?;
        }
        Node::Unary { expr, .. } => collect_symbols(expr, scope.clone())?,
        Node::PostUnary { expr, .. } => collect_symbols(expr, scope.clone())?,
        Node::Ternary {
            check,
            true_arm,
            false_arm,
            ..
        } => {
            collect_symbols(check, scope.clone())?;
            if let Some(true_arm) = true_arm {
                collect_symbols(true_arm, scope.clone())?;
            }
            collect_symbols(false_arm, scope.clone())?;
        }
        Node::AliasedVariable { expr, .. } => collect_symbols(expr, scope.clone())?,
        Node::DynamicVariable { expr, .. } => collect_symbols(expr, scope.clone())?,
        Node::Array { elements, .. } => {
            elements
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::OldArray { elements, .. } => {
            elements
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::ArrayElement { key, value, .. } => {
            if let Some(key) = key {
                collect_symbols(key, scope.clone())?;
            }
            collect_symbols(value, scope.clone())?;
        }
        Node::List { elements, .. } => {
            elements
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::Call {
            callee, parameters, ..
        } => {
            collect_symbols(callee, scope.clone())?;
            parameters
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::Isset { parameters, .. } => {
            parameters
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::Empty { parameters, .. } => {
            parameters
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::Exit { parameters, .. } => {
            if let Some(parameters) = parameters {
                parameters
                    .iter()
                    .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            }
        }
        Node::Die { parameters, .. } => {
            if let Some(parameters) = parameters {
                parameters
                    .iter()
                    .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            }
        }
        Node::New { class, .. } => collect_symbols(class, scope.clone())?,
        Node::Clone { object, .. } => collect_symbols(object, scope.clone())?,
        Node::Member { object, member, .. } => {
            collect_symbols(object, scope.clone())?;
            collect_symbols(member, scope.clone())?;
        }
        Node::StaticMember { class, member, .. } => {
            collect_symbols(class, scope.clone())?;
            collect_symbols(member, scope.clone())?;
        }
        Node::StaticMethod { class, method, .. } => {
            collect_symbols(class, scope.clone())?;
            collect_symbols(method, scope.clone())?;
        }
        Node::Field { array, index, .. } => {
            collect_symbols(array, scope.clone())?;

            if let Some(index) = index {
                collect_symbols(index, scope.clone())?;
            }
        }
        Node::Static { expr, .. } => {
            expr.iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::ArrowFunction {
            arguments, body, ..
        } => {
            if let Some(arguments) = arguments {
                arguments
                    .iter()
                    .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            }

            collect_symbols(body, scope.clone())?;
        }
        Node::Yield { expr, .. } => {
            if let Some(expr) = expr {
                collect_symbols(expr, scope.clone())?;
            }
        }
        Node::YieldFrom { expr, .. } => collect_symbols(expr, scope.clone())?,
        Node::FileInclude { resource, .. } => collect_symbols(resource, scope.clone())?,
        Node::ExpressionStatement { expression } => collect_symbols(expression, scope.clone())?,
        Node::EchoStatement { expressions, .. } => {
            expressions
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::ConstStatement { constants, .. } => {
            constants
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::PrintStatement { expressions, .. } => {
            expressions
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::ThrowStatement { expression, .. } => collect_symbols(expression, scope.clone())?,
        Node::UnsetStatement { vars, .. } => {
            vars.iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::ReturnStatement { expression, .. } => {
            if let Some(expression) = expression {
                collect_symbols(expression, scope.clone())?;
            }
        }
        Node::NamespaceStatement { .. }
        | Node::LexicalVariable { .. }
        | Node::Variable { .. }
        | Node::StaticVariable { .. }
        | Node::Function { .. }
        | Node::FunctionArgument { .. }
        | Node::Class { .. }
        | Node::NamespaceBlock { .. }
        | Node::ClassStatement { .. }
        | Node::TraitStatement { .. }
        | Node::ClassConstantDefinitionStatement { .. }
        | Node::PropertyDefinitionStatement { .. }
        | Node::MethodDefinitionStatement { .. }
        | Node::FunctionDefinitionStatement { .. }
        | Node::NamedFunctionDefinitionStatement { .. }
        | Node::Const { .. }
        | Node::Interface { .. } => {
            let def = document_symbol(node, scope.clone())?;

            scope.lock().unwrap().definition(def);
        }
        Node::WhileStatement {
            condition, body, ..
        } => {
            collect_symbols(condition, scope.clone())?;
            collect_symbols(body, scope.clone())?;
        }
        Node::DoWhileStatement {
            condition, body, ..
        } => {
            collect_symbols(condition, scope.clone())?;
            collect_symbols(body, scope.clone())?;
        }
        Node::ForStatement {
            init,
            condition,
            step,
            body,
            ..
        } => {
            init.iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            condition
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            step.iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            collect_symbols(body, scope.clone())?;
        }
        Node::ForEachStatement {
            collection,
            kv,
            body,
            ..
        } => {
            collect_symbols(collection, scope.clone())?;
            collect_symbols(kv, scope.clone())?;
            collect_symbols(body, scope.clone())?;
        }
        Node::Block { statements, .. } => {
            statements
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::AlternativeBlock { statements, .. } => {
            statements
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::IfBranch {
            condition, body, ..
        } => {
            collect_symbols(condition, scope.clone())?;
            collect_symbols(body, scope.clone())?;
        }
        Node::ElseBranch { body, .. } => {
            collect_symbols(body, scope.clone())?;
        }
        Node::IfStatement {
            if_branch,
            elseif_branches,
            else_branch,
        } => {
            collect_symbols(if_branch, scope.clone())?;
            elseif_branches
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());

            if let Some(else_branch) = else_branch {
                collect_symbols(else_branch, scope.clone())?;
            }
        }
        Node::SwitchBranch { cases, body } => {
            cases
                .iter()
                .filter(|c| c.is_some())
                .for_each(|n| collect_symbols(n.as_ref().unwrap(), scope.clone()).unwrap());

            body.iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::SwitchCase { expr, body, .. } => {
            collect_symbols(expr, scope.clone())?;
            collect_symbols(body, scope.clone())?;
        }
        Node::SwitchBody { branches, .. } => {
            branches
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::TokenStatement { expr, .. } => {
            if let Some(expr) = expr {
                collect_symbols(expr, scope.clone())?;
            }
        }
        Node::CatchBlock { var, body, .. } => {
            scope.lock().unwrap().definition(DocumentSymbol::from(var));
            collect_symbols(body, scope.clone())?;
        }
        Node::FinallyBlock { body, .. } => {
            collect_symbols(body, scope.clone())?;
        }
        Node::TryCatch {
            try_block,
            catch_blocks,
            finally_block,
            ..
        } => {
            collect_symbols(try_block, scope.clone())?;
            catch_blocks
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());

            if let Some(finally_block) = finally_block {
                collect_symbols(finally_block, scope.clone())?;
            }
        }
        Node::StaticVariablesStatement { assignments, .. } => {
            assignments
                .iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::GlobalVariablesStatement { vars, .. } => {
            vars.iter()
                .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
        }
        Node::Identifier(token) => scope
            .lock()
            .unwrap()
            .definition(DocumentSymbol::from(token)),
        Node::Literal(token) => {
            if token.is_identifier() {
                scope
                    .lock()
                    .unwrap()
                    .definition(DocumentSymbol::from(token));
            }
        }
        _ => {}
    };

    Ok(())
}

pub fn document_symbol(node: &Node, scope: Arc<Mutex<Scope>>) -> Result<DocumentSymbol, String> {
    match node {
        Node::Const { name, .. } => Ok(DocumentSymbol {
            name: name.clone().label.unwrap(),
            kind: SymbolKind::Constant,
            range: get_range(node.range()),
            selection_range: get_range(node.range()),
            detail: None,
            children: None,
            deprecated: None,
        }),
        Node::LexicalVariable { variable, .. } => Ok(DocumentSymbol::from(variable)),
        Node::StaticVariable { variable, .. } => Ok(DocumentSymbol::from(variable)),
        Node::Function {
            token,
            body,
            arguments,
            ..
        } => {
            let range = get_range(node.range());

            let scope = Scope::within(scope);

            collect_symbols(body, scope.clone())?;

            if let Some(arguments) = arguments {
                arguments
                    .iter()
                    .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            }

            let children = scope.lock().unwrap().get_definitions();

            Ok(DocumentSymbol {
                name: String::from("Anonymous function"),
                kind: SymbolKind::Function,
                range,
                selection_range: get_range(token.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::FunctionArgument { name, .. } => Ok(DocumentSymbol::from(name)),
        Node::Class {
            token,
            body,
            arguments,
            ..
        } => {
            let range = get_range(node.range());
            let scope = Scope::within(scope);
            collect_symbols(body, scope.clone())?;

            if let Some(arguments) = arguments {
                arguments
                    .iter()
                    .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            }

            let children = scope.lock().unwrap().get_definitions();

            Ok(DocumentSymbol {
                name: String::from("Anonymous class"),
                kind: SymbolKind::Class,
                range,
                selection_range: get_range(token.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::NamespaceBlock {
            token,
            type_ref,
            block,
            ..
        } => {
            let range = get_range(node.range());
            let name = if let Some(name) = &**type_ref {
                match name {
                    Node::TypeRef(tokens) => tokens
                        .iter()
                        .map(|n| n.clone().label.unwrap_or_else(|| "n to string".to_owned()))
                        .collect::<Vec<String>>()
                        .join(""),
                    _ => panic!("This should not happen"),
                }
            } else {
                "Anonymous namespace".to_string()
            };

            let scope = Scope::within(scope);
            collect_symbols(block, scope.clone())?;

            let children = scope.lock().unwrap().get_definitions();
            Ok(DocumentSymbol {
                name,
                kind: SymbolKind::Class,
                range,
                selection_range: get_range(token.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::ClassStatement { name, body, .. } => {
            let range = get_range(node.range());
            let scope = Scope::within(scope);
            collect_symbols(body, scope.clone())?;

            let children = scope.lock().unwrap().get_definitions();

            Ok(DocumentSymbol {
                name: name.clone().label.unwrap(),
                kind: SymbolKind::Class,
                range,
                selection_range: get_range(name.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::TraitStatement { name, body, .. } => {
            let range = get_range(node.range());
            let scope = Scope::within(scope);
            collect_symbols(body, scope.clone())?;

            let children = scope.lock().unwrap().get_definitions();
            Ok(DocumentSymbol {
                name: name.clone().label.unwrap(),
                kind: SymbolKind::Class,
                range,
                selection_range: get_range(name.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::Interface { name, body, .. } => {
            let range = get_range(node.range());
            let scope = Scope::within(scope);
            collect_symbols(body, scope.clone())?;

            let children = scope.lock().unwrap().get_definitions();
            Ok(DocumentSymbol {
                name: name.clone().label.unwrap(),
                kind: SymbolKind::Interface,
                range,
                selection_range: get_range(name.range()),
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::ClassConstantDefinitionStatement { name, .. } => {
            let range = get_range(node.range());
            Ok(DocumentSymbol {
                name: name.clone().label.unwrap(),
                kind: SymbolKind::Constant,
                range,
                selection_range: range,
                detail: None,
                children: None,
                deprecated: None,
            })
        }
        Node::PropertyDefinitionStatement { name, .. } => {
            let range = get_range(node.range());

            Ok(DocumentSymbol {
                name: name.clone().label.unwrap(),
                kind: SymbolKind::Property,
                range,
                selection_range: range,
                detail: None,
                children: None,
                deprecated: None,
            })
        }
        Node::MethodDefinitionStatement { name, function, .. } => {
            let range = get_range(node.range());
            // From the start of the declaration to the end of the method
            let function = document_symbol(function.as_ref(), scope);

            Ok(DocumentSymbol {
                name: name.clone().label.unwrap(),
                range,
                selection_range: get_range(name.range()),
                kind: SymbolKind::Method,
                ..function?
            })
        }
        Node::FunctionDefinitionStatement {
            arguments, body, ..
        } => {
            let range = get_range(node.range());

            let scope = Scope::within(scope);

            if let Some(arguments) = arguments {
                arguments
                    .iter()
                    .for_each(|n| collect_symbols(n, scope.clone()).unwrap());
            }

            if let Some(body) = body {
                collect_symbols(body, scope.clone())?;
            }

            let children = scope.lock().unwrap().get_definitions();
            Ok(DocumentSymbol {
                name: "Anonymous function".to_owned(),
                kind: SymbolKind::Function,
                range,
                selection_range: range,
                detail: None,
                children,
                deprecated: None,
            })
        }
        Node::NamedFunctionDefinitionStatement { name, function, .. } => {
            let range = get_range(node.range());

            // From the start of the declaration to the end of the method
            let function = document_symbol(function.as_ref(), scope);

            Ok(DocumentSymbol {
                name: name.clone().label.unwrap(),
                range,
                selection_range: get_range(name.range()),
                ..function?
            })
        }
        Node::Variable(token) => Ok(DocumentSymbol::from(token)),
        Node::NamespaceStatement { type_ref, .. } => {
            let range = get_range(node.range());
            let name = match &**type_ref {
                Node::TypeRef(tokens) => tokens
                    .iter()
                    .map(|n| n.clone().label.unwrap_or_else(|| "\\".to_owned()))
                    .collect::<Vec<String>>()
                    .join(""),
                _ => panic!("This should not happen"),
            };

            Ok(DocumentSymbol {
                name,
                kind: SymbolKind::Namespace,
                range,
                selection_range: range,
                detail: None,
                children: None,
                deprecated: None,
            })
        }
        _ => unimplemented!("Unexpected {:?}", node),
    }
}

pub fn get_range(coords: NodeRange) -> Range {
    let start = coords.0;
    let end = coords.1;

    Range {
        start: Position {
            line: start.0 as u64,
            character: start.1 as u64,
        },
        end: Position {
            line: end.0 as u64,
            character: end.1 as u64,
        },
    }
}
