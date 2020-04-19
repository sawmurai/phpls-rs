use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Scope {
    /// HashMap where the key is the name of the symbol and the value is a
    /// vector of all the tokens of its usage within the scope
    pub(crate) usages: HashMap<String, Vec<Token>>,

    /// HashMap where the key is the name of the symbol and the value is a
    /// definition of a class, property, constant, method or function
    pub(crate) definitions: HashMap<String, Token>,
}

impl Scope {
    pub fn symbols(&self) -> Vec<String> {
        self.usages.keys().cloned().collect::<Vec<String>>()
    }

    /// Return a list of usages of a token and append the usages of the parent scopes as well.
    pub fn get_references(&self, label: &String) -> Vec<Token> {
        if let Some(mine) = self.usages.get(label) {
            return mine.clone();
        }

        return Vec::new();
    }

    pub fn has_symbol(&self, symbol: &str) -> bool {
        self.usages.contains_key(symbol)
    }

    pub fn push_symbol(&mut self, token: Token) {
        if token.label.is_some() {
            self.usages
                .entry(token.clone().label.unwrap())
                .or_insert_with(|| Vec::new())
                .push(token);
        }
    }

    pub fn push_definition(&mut self, token: Token) {
        if token.label.is_some() {
            self.definitions.insert(token.clone().label.unwrap(), token);
        }
    }
}
