use crate::environment::import::collect_uses;
use crate::environment::symbol::{document_symbol, Symbol};
use crate::node::{get_range, Node};
use crate::token::{Token, TokenType};
use indextree::{Arena, NodeId};
use tower_lsp::lsp_types::Range;

#[derive(Clone, Debug)]
pub struct Reference {
    /// Used for variables
    pub token: Option<Token>,

    /// The type_ref if applicable
    pub type_ref: Option<Vec<Token>>,

    /// Selection range of the usage
    pub range: Range,
}

impl Reference {
    pub fn is_builtin(&self) -> bool {
        if let Some(type_ref) = self.type_ref.as_ref() {
            if type_ref.len() == 1 {
                match type_ref[0].t {
                    TokenType::TypeString
                    | TokenType::TypeArray
                    | TokenType::TypeBool
                    | TokenType::TypeSelf
                    | TokenType::TypeInt => return true,
                    _ => return false,
                }
            }
        }

        return false;
    }

    /// Reference to a variable
    pub fn variable(token: &Token) -> Self {
        Self {
            token: Some(token.clone()),
            type_ref: None,
            range: get_range(token.range()),
        }
    }

    /// Reference to an identifier, for example a function or a member
    pub fn identifier(token: &Token) -> Self {
        Self {
            token: Some(token.clone()),
            type_ref: None,
            range: get_range(token.range()),
        }
    }

    /// Reference to a type
    pub fn type_ref(type_ref: Vec<Token>) -> Self {
        let range = get_range((
            type_ref.first().unwrap().range().0,
            type_ref.last().unwrap().range().1,
        ));

        Self {
            token: None,
            type_ref: Some(type_ref),
            range,
        }
    }
}

pub fn collect_symbols(
    arena: &mut Arena<Symbol>,
    symbol: &NodeId,
    node: &Node,
) -> Result<(), String> {
    match node {
        Node::NamespaceStatement { .. }
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
        | Node::Interface { .. }
        | Node::Identifier(..)
        | Node::TypeRef { .. }
        | Node::LexicalVariable { .. }
        | Node::Variable(..)
        | Node::StaticVariable { .. }
        | Node::Literal(..)
        | Node::StaticMember { .. }
        | Node::Member { .. } => {
            //if !token.is_identifier() {
            document_symbol(arena, symbol, node, None)?;
            //}
        }
        Node::GroupedUse { .. }
        | Node::UseDeclaration { .. }
        | Node::UseFunction { .. }
        | Node::UseConst { .. } => {
            let symbol = arena[*symbol].get_mut();

            if let Some(imports) = symbol.imports.as_mut() {
                imports.extend(collect_uses(node, &Vec::new()));
            } else {
                symbol.imports = Some(collect_uses(node, &Vec::new()));
            }
        }
        Node::Call {
            callee, parameters, ..
        } => {
            document_symbol(arena, symbol, callee, None)?;

            for child in parameters {
                collect_symbols(arena, symbol, child)?;
            }
        }
        _ => {
            for child in node.children() {
                collect_symbols(arena, symbol, child)?;
            }
        }
    };

    Ok(())
}
