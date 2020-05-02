use crate::environment::import::SymbolImport;
use crate::environment::symbol::{document_symbol, Symbol};
use crate::node::Node;
use async_recursion::async_recursion;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

pub enum ScopeType {
    Function,
    Class,
    Method,
    Global,
    Namespace,
    File,
}

impl Default for ScopeType {
    fn default() -> ScopeType {
        ScopeType::Global
    }
}

#[derive(Default)]
pub struct Scope {
    /// Type of this scope
    pub scope_type: ScopeType,

    /// Symbols defined in this scope
    pub symbols: HashMap<String, Symbol>,

    /// Symbols imported into this scope via use-statements
    pub imports: Vec<SymbolImport>,

    /// Access to this scope's parent
    pub parent: Option<Arc<Mutex<Scope>>>,

    /// All children. The key is the function-, class- or namespace-name
    pub children: HashMap<String, Arc<Mutex<Scope>>>,
}

impl Scope {
    pub async fn within(
        name: &str,
        parent: Arc<Mutex<Self>>,
        scope_type: ScopeType,
    ) -> Arc<Mutex<Self>> {
        let new = Self {
            parent: Some(parent.clone()),
            scope_type,
            ..Default::default()
        };

        let new = Arc::new(Mutex::new(new));

        parent
            .lock()
            .await
            .children
            .insert(name.to_owned(), Arc::clone(&new));

        new
    }

    pub fn definition(&mut self, symbol: Symbol) {
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    pub fn get_definitions(&self) -> Option<Vec<Symbol>> {
        if self.symbols.is_empty() {
            None
        } else {
            Some(
                self.symbols
                    .values()
                    .map(|s| s.clone())
                    .collect::<Vec<Symbol>>(),
            )
        }
    }
}

#[async_recursion]
pub async fn collect_symbols(node: &Node, scope: Arc<Mutex<Scope>>) -> Result<(), String> {
    match node {
        Node::Binary { left, right, .. } => {
            collect_symbols(left, scope.clone()).await?;
            collect_symbols(right, scope.clone()).await?;
        }
        Node::Unary { expr, .. } => collect_symbols(expr, scope.clone()).await?,
        Node::PostUnary { expr, .. } => collect_symbols(expr, scope.clone()).await?,
        Node::Ternary {
            check,
            true_arm,
            false_arm,
            ..
        } => {
            collect_symbols(check, scope.clone()).await?;
            if let Some(true_arm) = true_arm {
                collect_symbols(true_arm, scope.clone()).await?;
            }
            collect_symbols(false_arm, scope.clone()).await?;
        }
        Node::AliasedVariable { expr, .. } => collect_symbols(expr, scope.clone()).await?,
        Node::DynamicVariable { expr, .. } => collect_symbols(expr, scope.clone()).await?,
        Node::Array { elements, .. }
        | Node::OldArray { elements, .. }
        | Node::List { elements, .. } => {
            for n in elements {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::ArrayElement { key, value, .. } => {
            if let Some(key) = key {
                collect_symbols(key, scope.clone()).await?;
            }
            collect_symbols(value, scope.clone()).await?;
        }
        Node::Call {
            callee, parameters, ..
        } => {
            collect_symbols(callee, scope.clone()).await?;
            for n in parameters {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::Isset { parameters, .. } | Node::Empty { parameters, .. } => {
            for n in parameters {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::Exit { parameters, .. } | Node::Die { parameters, .. } => {
            if let Some(parameters) = parameters {
                for n in parameters {
                    collect_symbols(n, scope.clone()).await?;
                }
            }
        }
        Node::New { class, .. } => collect_symbols(class, scope.clone()).await?,
        Node::Clone { object, .. } => collect_symbols(object, scope.clone()).await?,
        Node::Member { object, member, .. } => {
            collect_symbols(object, scope.clone()).await?;
            collect_symbols(member, scope.clone()).await?;
        }
        Node::StaticMember { class, member, .. } => {
            collect_symbols(class, scope.clone()).await?;
            collect_symbols(member, scope.clone()).await?;
        }
        Node::StaticMethod { class, method, .. } => {
            collect_symbols(class, scope.clone()).await?;
            collect_symbols(method, scope.clone()).await?;
        }
        Node::Field { array, index, .. } => {
            collect_symbols(array, scope.clone()).await?;

            if let Some(index) = index {
                collect_symbols(index, scope.clone()).await?;
            }
        }
        Node::Static { expr, .. } => {
            for n in expr {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::ArrowFunction {
            arguments, body, ..
        } => {
            if let Some(arguments) = arguments {
                for n in arguments {
                    collect_symbols(n, scope.clone()).await?;
                }
            }

            collect_symbols(body, scope.clone()).await?;
        }
        Node::Yield { expr, .. } => {
            if let Some(expr) = expr {
                collect_symbols(expr, scope.clone()).await?;
            }
        }
        Node::YieldFrom { expr, .. } => collect_symbols(expr, scope.clone()).await?,
        Node::FileInclude { resource, .. } => collect_symbols(resource, scope.clone()).await?,
        Node::ExpressionStatement { expression } => {
            collect_symbols(expression, scope.clone()).await?
        }
        Node::EchoStatement { expressions, .. } => {
            for n in expressions {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::ConstStatement { constants, .. } => {
            for n in constants {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::PrintStatement { expressions, .. } => {
            for n in expressions {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::ThrowStatement { expression, .. } => {
            collect_symbols(expression, scope.clone()).await?
        }
        Node::UnsetStatement { vars, .. } => {
            for n in vars {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::ReturnStatement { expression, .. } => {
            if let Some(expression) = expression {
                collect_symbols(expression, scope.clone()).await?;
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
            let def = document_symbol(node, scope.clone()).await?;

            scope.lock().await.definition(def);
        }
        Node::WhileStatement {
            condition, body, ..
        } => {
            collect_symbols(condition, scope.clone()).await?;
            collect_symbols(body, scope.clone()).await?;
        }
        Node::DoWhileStatement {
            condition, body, ..
        } => {
            collect_symbols(condition, scope.clone()).await?;
            collect_symbols(body, scope.clone()).await?;
        }
        Node::ForStatement {
            init,
            condition,
            step,
            body,
            ..
        } => {
            for n in init {
                collect_symbols(n, scope.clone()).await?;
            }
            for n in condition {
                collect_symbols(n, scope.clone()).await?;
            }
            for n in step {
                collect_symbols(n, scope.clone()).await?;
            }
            collect_symbols(body, scope.clone()).await?;
        }
        Node::ForEachStatement {
            collection,
            kv,
            body,
            ..
        } => {
            collect_symbols(collection, scope.clone()).await?;
            collect_symbols(kv, scope.clone()).await?;
            collect_symbols(body, scope.clone()).await?;
        }
        Node::Block { statements, .. } => {
            for n in statements {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::AlternativeBlock { statements, .. } => {
            for n in statements {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::IfBranch {
            condition, body, ..
        } => {
            collect_symbols(condition, scope.clone()).await?;
            collect_symbols(body, scope.clone()).await?;
        }
        Node::ElseBranch { body, .. } => {
            collect_symbols(body, scope.clone()).await?;
        }
        Node::IfStatement {
            if_branch,
            elseif_branches,
            else_branch,
        } => {
            collect_symbols(if_branch, scope.clone()).await?;
            for n in elseif_branches {
                collect_symbols(n, scope.clone()).await?;
            }

            if let Some(else_branch) = else_branch {
                collect_symbols(else_branch, scope.clone()).await?;
            }
        }
        Node::SwitchBranch { cases, body } => {
            for n in cases.iter().filter(|n| n.is_some()) {
                collect_symbols(&n.clone().unwrap(), scope.clone()).await?;
            }

            for n in body {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::SwitchCase { expr, body, .. } => {
            collect_symbols(expr, scope.clone()).await?;
            collect_symbols(body, scope.clone()).await?;
        }
        Node::SwitchBody { branches, .. } => {
            for n in branches {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::TokenStatement { expr, .. } => {
            if let Some(expr) = expr {
                collect_symbols(expr, scope.clone()).await?;
            }
        }
        Node::CatchBlock { var, body, .. } => {
            scope.lock().await.definition(Symbol::from(var));
            collect_symbols(body, scope.clone()).await?;
        }
        Node::FinallyBlock { body, .. } => {
            collect_symbols(body, scope.clone()).await?;
        }
        Node::TryCatch {
            try_block,
            catch_blocks,
            finally_block,
            ..
        } => {
            collect_symbols(try_block, scope.clone()).await?;
            for n in catch_blocks {
                collect_symbols(n, scope.clone()).await?;
            }

            if let Some(finally_block) = finally_block {
                collect_symbols(finally_block, scope.clone()).await?;
            }
        }
        Node::StaticVariablesStatement { assignments, .. } => {
            for n in assignments {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::GlobalVariablesStatement { vars, .. } => {
            for n in vars {
                collect_symbols(n, scope.clone()).await?;
            }
        }
        Node::Identifier(token) => scope.lock().await.definition(Symbol::from(token)),
        Node::Literal(_token) => {
            //if token.is_identifier() {
            //    scope.lock().unwrap().usage(DocumentSymbol::from(token))?;
            //}
        }
        _ => {}
    };

    Ok(())
}
