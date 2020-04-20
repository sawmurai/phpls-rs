use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct Scope {
    /// HashMap where the key is the name of the symbol and the value is a
    /// vector of all the tokens of its usage within the scope
    pub(crate) usages: HashMap<String, Vec<Token>>,

    /// HashMap where the key is the name of the symbol and the value is a
    /// definition of a class, property, constant, method or function
    pub(crate) definitions: HashMap<String, Token>,

    pub(crate) parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn new(parent: Rc<RefCell<Scope>>) -> Self {
        Self {
            parent: Some(parent),
            ..Scope::default()
        }
    }

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

    /// Determine if the current scope has acces to a symbol, either by defining it or by a parent defining it
    pub fn has_symbol(&mut self, symbol: &str) -> bool {
        return self.usages.contains_key(symbol)
            || if let Some(ref mut parent) = self.parent.as_ref() {
                parent.borrow_mut().has_symbol(symbol)
            } else {
                false
            };
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
