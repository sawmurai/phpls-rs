use crate::expression::*;
use crate::statement::*;
use crate::token::{Token, TokenType};

use std::iter::Iterator;
use std::iter::Peekable;
use std::slice::Iter;

type StatementResult = Result<Box<dyn Stmt>, String>;
type StatementListResult = Result<Vec<Box<dyn Stmt>>, String>;
type ArgumentListResult = Result<Option<Vec<Node>>, String>;
type ExpressionResult = Result<Box<Node>, String>;

/// Inspired by https://craftinginterpreters.com/statements-and-state.html
///
/// Parses a token stream of a `Scanner` and generates an Abstract Syntax Tree
#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    literals: Vec<TokenType>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
            errors: Vec::new(),
            literals: vec![
                TokenType::False,
                TokenType::True,
                TokenType::Null,
                TokenType::LongNumber,
                TokenType::DecimalNumber,
                TokenType::HexNumber,
                TokenType::ConstantEncapsedString,
                TokenType::EncapsedAndWhitespaceString,
                TokenType::Variable,
            ],
        }
    }

    pub fn errors(&self) -> &Vec<String> {
        self.errors.as_ref()
    }

    /// Fast forwards to the end of the current statement or block
    fn error_fast_forward(&mut self) {
        while self.tokens.peek().is_some() {
            self.tokens.next();

            if self.next_token_one_of(&vec![TokenType::Semicolon]) {
                self.tokens.next();

                break;
            }

            if self.next_token_one_of(&vec![TokenType::CloseCurly]) {
                self.tokens.next();
                break;
            }
        }
    }

    /// Parses the entire token stream and returns an abstract syntax tree representation
    ///
    /// # Example
    /// ```
    /// // Content contains the source code
    /// let mut scanner = Scanner::new(&content);
    /// scanner.scan()?;
    /// let mut parser = Parser::new(&scanner.tokens);
    /// parser.ast();
    /// ```
    pub fn ast(&mut self) -> StatementListResult {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        self.consume_or_err(TokenType::ScriptStart)?;

        while self.tokens.peek().is_some() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    self.errors.push(error);
                    self.error_fast_forward();
                }
            }
        }
        Ok(statements)
    }

    /// Parses a code block, which basically is a vector of `dyn Stmt` / statements.
    /// It expects to already be past the `{` and it will read until it encounters a `}`
    ///
    /// # Details
    /// ```php
    /// while (true) {
    /// // Parse here
    /// }
    /// ```
    pub fn block(&mut self) -> Result<Box<dyn Stmt>, String> {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        // TODO: Make sure namespace etc can not pop up here
        while !self.next_token_one_of(&vec![TokenType::CloseCurly]) && self.tokens.peek().is_some()
        {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    self.errors.push(error);
                    self.error_fast_forward();
                }
            }
        }

        Ok(Box::new(Block::new(statements)))
    }

    /// Parses a class block, so basically the body that contains all the method definitions etc.
    /// It expects to be past the `{` and will read until it encounters a `}`
    ///  
    /// # Details
    /// ```php
    /// abstract class Whatever {
    /// // Parse here
    /// }
    /// ```
    fn class_block(&mut self) -> StatementListResult {
        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        while !self.next_token_one_of(&vec![TokenType::CloseCurly]) {
            if self.next_token_one_of(&vec![TokenType::Use]) {
                statements.push(self.use_trait_statement()?);

                continue;
            }

            let mut is_abstract = self.consume_or_ignore(TokenType::Abstract).is_some();
            let is_final = self.consume_or_ignore(TokenType::Final);

            let visibility = self.consume_one_of_cloned_or_ignore(&vec![
                TokenType::Public,
                TokenType::Var,
                TokenType::Private,
                TokenType::Protected,
            ]);

            if !is_abstract {
                is_abstract = self.consume_or_ignore(TokenType::Abstract).is_some();
            }

            if self.next_token_one_of(&vec![TokenType::Const]) {
                self.tokens.next();
                let name = self.consume_identifier_cloned()?;

                self.consume_or_err(TokenType::Assignment)?;
                statements.push(Box::new(ClassConstantDefinitionStatement::new(
                    name,
                    visibility,
                    self.expression()?,
                )));

                self.consume_end_of_statement()?;

                continue;
            };

            let is_static = self.consume_or_ignore(TokenType::Static).is_some();

            if let Some(next) = self.tokens.peek() {
                match next.t {
                    TokenType::Function => {
                        self.tokens.next();

                        let by_ref = self.consume_or_ignore(TokenType::BinaryAnd);
                        let name = self.consume_identifier_cloned()?;

                        statements.push(Box::new(MethodDefinitionStatement::new(
                            is_final,
                            by_ref,
                            name,
                            visibility,
                            is_abstract,
                            self.anonymous_function_statement()?,
                            is_static,
                        )));
                    }
                    // One or more of those ...
                    TokenType::Variable => {
                        loop {
                            // The next variable
                            let name = self.consume_cloned(TokenType::Variable)?;
                            let assignment =
                                match self.next_token_one_of(&vec![TokenType::Assignment]) {
                                    true => {
                                        self.tokens.next();
                                        Some(self.expression()?)
                                    }
                                    false => None,
                                };

                            statements.push(Box::new(PropertyDefinitionStatement::new(
                                name,
                                visibility.clone(),
                                is_abstract,
                                assignment,
                                is_static,
                            )));

                            if !self.next_token_one_of(&vec![TokenType::Comma]) {
                                break;
                            }

                            // The comma
                            self.tokens.next();
                        }

                        self.consume_end_of_statement()?;
                    }
                    _ => {
                        return Err(format!(
                            "Unexpected {:?} on line {}, col {}",
                            next.t, next.line, next.col
                        ));
                    }
                }
            } else {
                return Err(String::from("End of file"));
            }

            if let Some(Token {
                t: TokenType::CloseCurly,
                ..
            }) = self.tokens.peek()
            {
                break;
            }
        }

        Ok(statements)
    }

    /// Parses a function definition by calling methods to parse the argument list, return type and body.
    /// It only handles anonymous functions, since the name of a named function was parses previously ... and
    /// a named function stripped off of the name is ... anonymous :)
    ///
    /// # Details
    /// ```php
    /// function my_funy /** from here **/ (string $a, int $b): void {
    ///     echo "Hello!";
    /// }
    /// /** to here **/
    /// ```
    fn anonymous_function_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let arguments = self.argument_list()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;

        let return_type = self.return_type()?;

        if self.consume_or_ignore(TokenType::Semicolon).is_some() {
            return Ok(Box::new(FunctionDefinitionStatement::new(
                arguments,
                return_type,
                None,
            )));
        }

        self.consume_or_err(TokenType::OpenCurly)?;
        let body = self.block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        return Ok(Box::new(FunctionDefinitionStatement::new(
            arguments,
            return_type,
            Some(body),
        )));
    }

    /// Parses a function definition by calling methods to parse the argument list, return type and body.
    /// Handles named function
    ///
    /// # Details
    /// ```php
    /// function /** from here **/my_funy (string $a, int $b): void {
    ///     echo "Hello!";
    /// }
    /// /** to here **/
    /// ```
    fn named_function(&mut self) -> StatementResult {
        Ok(Box::new(NamedFunctionDefinitionStatement::new(
            self.consume_or_ignore(TokenType::BinaryAnd),
            self.consume_identifier_cloned()?,
            self.anonymous_function_statement()?,
        )))
    }

    fn inline_html(&mut self) -> StatementResult {
        let start = self.consume_cloned(TokenType::ScriptEnd)?;

        Ok(Box::new(InlineHtml::new(
            start,
            self.consume_or_ignore(TokenType::ScriptStart),
        )))
    }

    /// Parses the argument list of a function, excluding the parenthesis
    ///
    /// # Details
    /// ```php
    /// function my_funy (/** from here **/string $a, int $b/** to here **/): void {
    ///     echo "Hello!";
    /// }
    /// ```
    fn argument_list(&mut self) -> ArgumentListResult {
        let mut arguments = Vec::new();

        if self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
            return Ok(None);
        }

        loop {
            let t = self.argument_type()?;
            let spread = self.consume_or_ignore(TokenType::Elipsis);
            let reference = self.consume_or_ignore(TokenType::BinaryAnd);
            let name = self.consume_cloned(TokenType::Variable)?;
            let has_default = self.consume_or_ignore(TokenType::Assignment);

            let default_value = match has_default.is_some() {
                true => Some(self.expression()?),
                false => None,
            };

            arguments.push(Node::FunctionArgument {
                argument_type: t,
                name,
                spread,
                reference,
                has_default,
                default_value,
            });

            if self.next_token_one_of(&vec![TokenType::Comma]) {
                self.tokens.next();
            } else {
                break;
            }
        }

        Ok(Some(arguments))
    }

    /// Parses the argument type of a function argument
    ///
    /// # Details
    /// ```php
    /// function my_funy (/** from here **/string/** to here **/ $a, int $b): ?int {
    ///     echo "Hello!";
    /// }
    /// ```
    fn argument_type(&mut self) -> Result<Option<Box<Node>>, String> {
        if let Some(qm) = self.consume_or_ignore(TokenType::QuestionMark) {
            Ok(Some(Box::new(Node::ArgumentType {
                nullable: Some(qm),
                type_ref: self.non_empty_type_ref()?,
            })))
        } else if let Some(type_ref) = self.type_ref()? {
            Ok(Some(Box::new(Node::ArgumentType {
                nullable: None,
                type_ref,
            })))
        } else {
            Ok(None)
        }
    }

    /// Parses the return type of a function, excluding the colon
    ///
    /// # Details
    /// ```php
    /// function my_funy (string $a, int $b): /** from here **/?int/** to here **/ {
    ///     echo "Hello!";
    /// }
    /// ```
    fn return_type(&mut self) -> Result<Option<Box<Node>>, String> {
        if let Some(colon) = self.consume_or_ignore(TokenType::Colon) {
            Ok(Some(Box::new(Node::ReturnType {
                token: colon,
                nullable: self.consume_or_ignore(TokenType::QuestionMark),
                type_ref: self.non_empty_type_ref()?,
            })))
        } else {
            Ok(None)
        }
    }

    /// Parses a single statement (offloaded depending on which statement was encountered)
    ///
    /// # Details
    /// ```php
    /// /** from here **/
    /// function my_funy (string $a, int $b): ?int {
    ///     echo "Hello!";
    /// }
    /// /** to here **/
    /// ```
    fn statement(&mut self) -> StatementResult {
        if let Some(&token) = self.tokens.peek() {
            match token.t {
                TokenType::ScriptEnd => {
                    return self.inline_html();
                }
                TokenType::Function => {
                    self.tokens.next();

                    return self.named_function();
                }
                TokenType::Namespace => {
                    self.tokens.next();

                    return self.namespace_statement();
                }
                TokenType::Use => {
                    self.tokens.next();

                    return self.use_statement();
                }
                TokenType::Static => {
                    let expr = self.expression_statement()?;

                    // Find a way to associate the sometimes following var list to the static keyword
                    return Ok(expr);
                    // TODO: Issue a warning if the assignment uses values that are not defined at compile-time (variables etc.)
                    //return self.static_variables();
                }
                TokenType::Global => {
                    self.tokens.next();

                    return self.global_variables();
                }
                TokenType::Echo => {
                    self.tokens.next();

                    return self.echo_statement();
                }
                TokenType::Goto => {
                    self.tokens.next();

                    return self.goto_statement();
                }
                TokenType::Return => {
                    self.tokens.next();

                    return self.return_statement();
                }
                TokenType::Throw => {
                    self.tokens.next();

                    return self.throw_statement();
                }
                TokenType::Class => {
                    self.tokens.next();
                    return self.class_statement(false, false);
                }
                TokenType::Trait => {
                    self.tokens.next();
                    return self.trait_statement();
                }
                TokenType::Abstract => {
                    self.tokens.next();
                    return self.abstract_class_statement();
                }
                TokenType::Final => {
                    self.tokens.next();
                    return self.final_class_statement();
                }
                TokenType::Interface => {
                    self.tokens.next();
                    return self.interface();
                }
                TokenType::While => {
                    self.tokens.next();
                    return self.while_statement();
                }
                TokenType::Do => {
                    self.tokens.next();
                    return self.do_while_statement();
                }
                TokenType::For => {
                    self.tokens.next();
                    return self.for_statement();
                }
                TokenType::Foreach => {
                    self.tokens.next();
                    return self.foreach_statement();
                }
                TokenType::If => {
                    self.tokens.next();
                    return self.if_statement();
                }
                TokenType::OpenCurly => {
                    self.tokens.next();
                    let block = self.block();

                    self.consume_or_err(TokenType::CloseCurly)?;

                    return block;
                }
                TokenType::Switch => {
                    self.tokens.next();
                    return self.switch_statement();
                }
                TokenType::Semicolon => {
                    return Ok(Box::new(TokenStatement::new(
                        self.consume_cloned(TokenType::Semicolon)?,
                        None,
                    )));
                }
                TokenType::Exit => {
                    return Ok(Box::new(TokenStatement::new(
                        self.consume_cloned(TokenType::Exit)?,
                        None,
                    )));
                }
                TokenType::Break | TokenType::Continue => {
                    let token = self.consume_cloned(token.t.clone())?;

                    let expr = match self.next_token_one_of(&vec![TokenType::Semicolon]) {
                        true => None,
                        false => Some(self.expression()?),
                    };

                    let statement = Box::new(TokenStatement::new(token, expr));

                    self.consume_end_of_statement()?;

                    return Ok(statement);
                }
                TokenType::Try => {
                    self.tokens.next();
                    return self.try_catch_statement();
                }
                TokenType::Declare => {
                    return self.declare_statement();
                }
                TokenType::Unset => {
                    return self.unset_statement();
                }
                // Support labels here
                // TokenType::Identifier => {}
                _ => {
                    let expr = self.expression_statement()?;

                    self.consume_end_of_statement()?;

                    return Ok(expr);
                }
            }
        }

        return Err(String::from("Unexpected EOF!"));
    }

    /// Parses a try catch statement
    ///
    /// # Details
    /// ```php
    /// try /** from here **/(true) {
    /// } catch (Exception $e) {}
    ///     echo "stuff";
    /// }
    /// /** to here **/
    /// ```
    fn try_catch_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::OpenCurly)?;
        let try_block = self.block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        // Needs to have at least one, so we unroll one iteration
        let mut catch_blocks = Vec::new();
        catch_blocks.push(self.catch_block()?);

        while self.next_token_one_of(&vec![TokenType::Catch]) {
            catch_blocks.push(self.catch_block()?);
        }

        let finally_block = match self.tokens.peek() {
            Some(Token {
                t: TokenType::Finally,
                ..
            }) => {
                self.tokens.next();
                self.consume_or_err(TokenType::OpenCurly)?;
                let b = Some(self.block()?);
                self.consume_or_err(TokenType::CloseCurly)?;
                b
            }
            _ => None,
        };

        Ok(Box::new(TryCatch::new(
            try_block,
            catch_blocks,
            finally_block,
        )))
    }

    /// Parses a catch block (including the catch-keyword, yes, I need to make my mind up about including / excluding the keyword)
    ///
    /// # Details
    /// ```php
    /// /** from here **/catch (Exception $e) {}
    ///     echo "stuff";
    /// }
    /// /** to here **/
    /// ```
    fn catch_block(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::Catch)?;
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let types = self.type_ref_union()?;
        let var = self.consume_cloned(TokenType::Variable)?;
        self.consume_or_err(TokenType::CloseParenthesis)?;

        self.consume_or_err(TokenType::OpenCurly)?;
        let catch_block = self.block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(CatchBlock::new(types, var, catch_block)))
    }

    /// Parses a path list, pipe separated. This needs to become a type-list!
    ///
    /// # Details
    /// ```php
    /// catch (/** from here **/Exception1 | Exception2/** to here **/ $e) {}
    ///     echo "stuff";
    /// }
    ///
    /// ```
    fn type_ref_union(&mut self) -> Result<Vec<Box<Node>>, String> {
        let mut paths = Vec::new();

        paths.push(self.non_empty_type_ref()?);

        while self.consume_or_ignore(TokenType::BinaryOr).is_some() {
            if let Some(type_ref) = self.type_ref()? {
                paths.push(type_ref);
            } else {
                return Err(String::from("Expected type after | operator"));
            }
        }

        Ok(paths)
    }

    /// Parses a switch case
    ///
    /// # Details
    /// ```php
    /// switch /** from here **/(true) {
    ///     case "bla":
    ///         echo "stuff";
    /// }
    /// /** to here **/
    /// ```
    fn switch_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let expr = self.expression()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;
        self.consume_or_err(TokenType::OpenCurly)?;

        let mut branches = Vec::new();
        while !self.next_token_one_of(&vec![TokenType::CloseCurly]) {
            let cases_current_branch = self.case_list()?;

            let mut statements = Vec::new();
            while !self.next_token_one_of(&vec![
                TokenType::CloseCurly,
                TokenType::Case,
                TokenType::Default,
            ]) {
                statements.push(self.statement()?);
            }

            branches.push(Box::new(SwitchBranch::new(
                cases_current_branch,
                statements,
            )));
        }

        self.tokens.next();

        Ok(Box::new(SwitchCase::new(expr, branches)))
    }

    fn case_list(&mut self) -> Result<Vec<Option<Box<Node>>>, String> {
        let mut cases_current_branch = Vec::new();

        loop {
            match self.tokens.peek() {
                Some(Token {
                    t: TokenType::Default,
                    ..
                }) => {
                    cases_current_branch.push(None);
                    self.tokens.next();
                    self.consume_or_err(TokenType::Colon)
                        .or_else(|_| self.consume_end_of_statement())?;
                }
                Some(Token {
                    t: TokenType::Case, ..
                }) => {
                    self.tokens.next();
                    cases_current_branch.push(Some(self.expression()?));
                    self.consume_or_err(TokenType::Colon)
                        .or_else(|_| self.consume_end_of_statement())?;
                }
                _ => {
                    break;
                }
            }
        }

        Ok(cases_current_branch)
    }

    /// Parses a while loop
    ///
    /// # Details
    /// ```php
    /// while /** from here **/(true) {
    ///     do_stuff();
    /// }
    /// /** to here **/
    /// ```
    fn while_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let condition = self.expression()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;
        let body = match self.tokens.peek() {
            Some(Token {
                t: TokenType::OpenCurly,
                ..
            }) => {
                self.tokens.next();
                let block = self.block()?;
                self.tokens.next();

                block
            }
            Some(_) => Box::new(Block::new(vec![self.statement()?])),
            None => return Err(String::from("Unexpected EOF!")),
        };

        Ok(Box::new(WhileStatement::new(condition, body)))
    }

    /// Parses a foreach loop
    ///
    /// # Details
    /// ```php
    /// foreach /** from here **/($array as $k => $v) {
    ///     do_stuff();
    /// }
    /// /** to here **/
    /// ```
    fn foreach_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let collection = self.expression()?;

        self.consume_or_err(TokenType::As)?;
        let key_value = self.array_pair()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;

        let body = match self.tokens.peek() {
            Some(Token {
                t: TokenType::OpenCurly,
                ..
            }) => {
                self.tokens.next();
                let block = self.block()?;
                self.tokens.next();

                block
            }
            Some(_) => Box::new(Block::new(vec![self.statement()?])),
            None => return Err(String::from("Unexpected EOF!")),
        };

        Ok(Box::new(ForEachStatement::new(collection, key_value, body)))
    }

    /// Parses a do-while loop
    ///
    /// # Details
    /// ```php
    /// do
    /// /** from here **/ {
    ///     do_stuff();
    /// } while (true);
    /// /** to here **/
    /// ```
    fn do_while_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::OpenCurly)?;
        let body = self.block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        self.consume_or_err(TokenType::While)?;
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let condition = self.expression()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;
        self.consume_end_of_statement()?;

        Ok(Box::new(DoWhileStatement::new(condition, body)))
    }

    /// Parses a for loop
    ///
    /// # Details
    /// ```php
    /// for /** from here **/ ($i = 0; $i < 100; $i++) {
    ///     do_stuff();
    /// }
    /// /** to here **/
    /// ```
    fn for_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::OpenParenthesis)?;

        let mut init = Vec::new();
        while !self.next_token_one_of(&vec![TokenType::Semicolon]) {
            init.push(self.expression_statement()?);

            if self.next_token_one_of(&vec![TokenType::Comma]) {
                self.tokens.next();
            } else {
                break;
            }
        }

        self.consume_or_err(TokenType::Semicolon)?;

        let mut condition = Vec::new();
        while !self.next_token_one_of(&vec![TokenType::Semicolon]) {
            condition.push(self.expression_statement()?);

            if self.next_token_one_of(&vec![TokenType::Comma]) {
                self.tokens.next();
            } else {
                break;
            }
        }

        self.consume_or_err(TokenType::Semicolon)?;

        let mut step = Vec::new();
        while !self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
            step.push(self.expression_statement()?);

            if self.next_token_one_of(&vec![TokenType::Comma]) {
                self.tokens.next();
            } else {
                break;
            }
        }
        self.consume_or_err(TokenType::CloseParenthesis)?;

        self.consume_or_err(TokenType::OpenCurly)?;
        let body = self.block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(ForStatement::new(init, condition, step, body)))
    }

    /// Parses an if statement
    ///
    /// # Details
    /// ```php
    /// if /** from here **/ (true) {
    ///     do_stuff();
    /// } elseif (false) {
    ///     other_stuff();
    /// } else {
    ///     rest_stuff();
    /// }
    /// /** to here **/
    /// ```
    fn if_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let condition = self.expression()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;
        let if_branch = IfBranch::new(condition, self.statement()?);

        let mut elseif_branches = Vec::new();
        while self.next_token_one_of(&vec![TokenType::ElseIf]) {
            self.tokens.next();
            self.consume_or_err(TokenType::OpenParenthesis)?;
            let condition = self.expression()?;
            self.consume_or_err(TokenType::CloseParenthesis)?;

            elseif_branches.push(Box::new(IfBranch::new(condition, self.statement()?)));
        }

        let else_branch = match self.tokens.peek() {
            Some(Token {
                t: TokenType::Else, ..
            }) => {
                self.tokens.next();
                Some(self.statement()?)
            }
            _ => None,
        };

        Ok(Box::new(IfStatement::new(
            Box::new(if_branch),
            elseif_branches,
            else_branch,
        )))
    }

    /// Parses a single namespace statement
    ///
    /// # Details
    /// ```php
    /// namespace /** from here **/My\Super\Duper\Namespace;/** to here **/
    /// ```
    fn namespace_statement(&mut self) -> StatementResult {
        if let Some(type_ref) = self.type_ref()? {
            // TODO: Implement block
            self.consume_end_of_statement()?;

            Ok(Box::new(NamespaceStatement::new(type_ref)))
        } else {
            Err("Empty path after namespace".to_owned())
        }
    }

    // use -> "use" ("function" | "const")? path (("{" use_group "}") | ("as" identifier))?
    fn use_statement(&mut self) -> StatementResult {
        let function_const = self
            .consume_or_ignore(TokenType::Function)
            .or_else(|| self.consume_or_ignore(TokenType::Const));

        let path = self.non_empty_type_ref()?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(format!("Unexpected EOF"));
        };

        let stmt = match next.t {
            TokenType::As => {
                self.tokens.next();
                UseStatement::aliased(
                    function_const,
                    path,
                    self.consume_cloned(TokenType::Identifier)?,
                )
            }
            TokenType::OpenCurly => {
                self.tokens.next();
                UseStatement::grouped(function_const, path, self.use_group()?)
            }
            TokenType::Semicolon => UseStatement::new(path),
            _ => {
                return Err(format!(
                    "Unexpected {:?} on line {}, col {}",
                    next.t, next.line, next.col
                ));
            }
        };
        self.consume_end_of_statement()?;

        Ok(Box::new(stmt))
    }

    // use -> "use" identifier (, identifier)*
    fn use_trait_statement(&mut self) -> StatementResult {
        let statement = Box::new(UseTraitStatement::new(
            self.consume_cloned(TokenType::Use)?,
            self.type_ref_list()?,
        ));

        self.consume_end_of_statement()?;

        Ok(statement)
    }

    fn type_ref_list(&mut self) -> Result<Vec<Box<Node>>, String> {
        let mut type_refs = Vec::new();

        type_refs.push(self.non_empty_type_ref()?);

        while self.consume_or_ignore(TokenType::Comma).is_some() {
            type_refs.push(self.non_empty_type_ref()?);
        }

        Ok(type_refs)
    }

    // use_group -> (grouped_use ",")* grouped_use
    fn use_group(&mut self) -> StatementListResult {
        let mut group = Vec::new();

        loop {
            group.push(self.grouped_use_statement()?);

            match self.tokens.next() {
                Some(Token {
                    t: TokenType::Comma,
                    ..
                }) => {
                    continue;
                }
                Some(Token {
                    t: TokenType::CloseCurly,
                    ..
                }) => {
                    break;
                }
                Some(Token { t, line, col, .. }) => {
                    return Err(format!("Unexpected {:?} on line {}, col {}", t, line, col));
                }
                _ => {
                    return Err(format!("Unexpected EOF"));
                }
            }
        }

        Ok(group)
    }

    // grouped_use -> path ("as" identifier)?"
    fn grouped_use_statement(&mut self) -> StatementResult {
        let path = self.non_empty_type_ref()?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(String::from("Unexpected EOF"));
        };

        let stmt = match next.t {
            TokenType::As => {
                self.tokens.next();
                UseStatement::aliased(None, path, self.consume_cloned(TokenType::Identifier)?)
            }
            _ => UseStatement::new(path),
        };

        Ok(Box::new(stmt))
    }

    fn global_variables(&mut self) -> StatementResult {
        let mut vars = Vec::new();
        vars.push(self.consume_cloned(TokenType::Variable)?);

        self.consume_or_ignore(TokenType::Comma);

        while !self.next_token_one_of(&vec![TokenType::Semicolon]) {
            vars.push(self.consume_cloned(TokenType::Variable)?);

            if self.next_token_one_of(&vec![TokenType::Semicolon]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        self.consume_end_of_statement()?;

        Ok(Box::new(GlobalVariablesStatement::new(vars)))
    }

    fn echo_statement(&mut self) -> StatementResult {
        let mut values = Vec::new();

        values.push(self.expression()?);

        while self.consume_or_ignore(TokenType::Comma).is_some() {
            values.push(self.expression()?);
        }

        self.consume_end_of_statement()?;

        Ok(Box::new(EchoStatement::new(values)))
    }
    fn goto_statement(&mut self) -> StatementResult {
        let label = self.consume_identifier_cloned()?;

        self.consume_end_of_statement()?;

        Ok(Box::new(GotoStatement::new(label)))
    }

    fn throw_statement(&mut self) -> StatementResult {
        let value = self.expression()?;

        self.consume_end_of_statement()?;

        Ok(Box::new(ThrowStatement::new(value)))
    }

    fn return_statement(&mut self) -> StatementResult {
        if self.next_token_one_of(&vec![TokenType::Semicolon]) {
            self.consume_end_of_statement()?;
            Ok(Box::new(ReturnStatement::new(None)))
        } else {
            let value = self.expression()?;

            self.consume_end_of_statement()?;

            Ok(Box::new(ReturnStatement::new(Some(value))))
        }
    }

    // abstract_class -> "abstract" class
    fn abstract_class_statement(&mut self) -> StatementResult {
        self.tokens.next();

        self.class_statement(true, false)
    }

    // final_class -> "final" class
    fn final_class_statement(&mut self) -> StatementResult {
        self.tokens.next();

        self.class_statement(false, true)
    }

    // class -> "class" identifier (extends identifier_list)? (implements identifier_list)?
    fn class_statement(&mut self, is_abstract: bool, is_final: bool) -> StatementResult {
        let name = self.consume_cloned(TokenType::Identifier)?;

        let extends = match self.consume_or_ignore(TokenType::Extends) {
            Some(_) => Some(self.identifier_list()?),
            None => None,
        };

        let implements = match self.consume_or_ignore(TokenType::Implements) {
            Some(_) => Some(self.identifier_list()?),
            None => None,
        };

        self.consume_or_err(TokenType::OpenCurly)?;
        let block = self.class_block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(ClassStatement::new(
            name,
            is_abstract,
            is_final,
            extends,
            implements,
            block,
        )))
    }

    fn anonymous_class(&mut self) -> ExpressionResult {
        let token = self.consume_cloned(TokenType::Class)?;

        let arguments = match self.next_token_one_of(&vec![TokenType::OpenParenthesis]) {
            true => Some(self.parameter_list()?),
            false => None,
        };

        let extends = match self.consume_or_ignore(TokenType::Extends) {
            Some(_) => Some(self.identifier_list()?),
            None => None,
        };

        let implements = match self.consume_or_ignore(TokenType::Implements) {
            Some(_) => Some(self.identifier_list()?),
            None => None,
        };

        self.consume_or_err(TokenType::OpenCurly)?;
        let body = self.class_block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(Node::Class {
            token,
            arguments,
            extends,
            implements,
            body,
        }))
    }

    /// Parses a trait
    fn trait_statement(&mut self) -> StatementResult {
        let name = self.consume_cloned(TokenType::Identifier)?;

        self.consume_or_err(TokenType::OpenCurly)?;
        let block = self.class_block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(TraitStatement::new(name, block)))
    }

    /// Parses an interface definition
    fn interface(&mut self) -> StatementResult {
        let name = self.consume_cloned(TokenType::Identifier)?;

        let extends = match self.consume_or_ignore(TokenType::Extends) {
            Some(_) => Some(self.identifier_list()?),
            None => None,
        };

        self.consume_or_err(TokenType::OpenCurly)?;
        let block = self.class_block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(Interface::new(name, extends, block)))
    }

    // (("extends" identifier) (, "extends" identifier)*)?
    // (("implements" identifier) (, "implements" identifier)*)?
    fn identifier_list(&mut self) -> Result<Vec<Box<Node>>, String> {
        let mut extends = Vec::new();

        loop {
            extends.push(self.non_empty_type_ref()?);

            if !self.next_token_one_of(&vec![TokenType::Comma]) {
                break;
            }

            self.tokens.next();
        }

        Ok(extends)
    }

    /// Parses declare statements
    fn declare_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::Declare)?;
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let directive = self.consume_cloned(TokenType::Identifier)?;
        self.consume_or_err(TokenType::Assignment)?;
        let val = self.consume_one_of_cloned(&vec![
            TokenType::False,
            TokenType::True,
            TokenType::Null,
            TokenType::LongNumber,
            TokenType::DecimalNumber,
            TokenType::HexNumber,
            TokenType::ConstantEncapsedString,
            TokenType::EncapsedAndWhitespaceString,
        ])?;
        self.consume_or_err(TokenType::CloseParenthesis)?;

        Ok(Box::new(DeclareStatement::new(directive, val)))
    }

    /// Parses unset statements
    fn unset_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::Unset)?;
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let vars = self.non_empty_parameter_list()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;

        Ok(Box::new(UnsetStatement::new(vars)))
    }

    fn expression_statement(&mut self) -> StatementResult {
        let value = self.expression()?;

        Ok(Box::new(ExpressionStatement::new(value)))
    }

    // path -> identifier ("\" identifier)*
    fn type_ref(&mut self) -> Result<Option<Box<Node>>, String> {
        let mut path = Vec::new();

        if let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
            path.push(ns);
        }

        while let Some(identifier) = self.tokens.peek() {
            if identifier.is_identifier() {
                path.push(self.tokens.next().unwrap().clone());

                if let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
                    path.push(ns);
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if path.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Box::new(Node::TypeRef(path))))
        }
    }

    // non_empty_type_ref -> identifier ("\" identifier)*
    fn non_empty_type_ref(&mut self) -> Result<Box<Node>, String> {
        let mut path = Vec::new();

        if let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
            path.push(ns);
        }

        path.push(self.consume_identifier_cloned()?);

        loop {
            if let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
                path.push(ns);
            } else {
                break;
            }

            path.push(self.consume_identifier_cloned()?);
        }

        Ok(Box::new(Node::TypeRef(path)))
    }

    /// Parses an expression. This can be anything that evaluates to a value. A function call, a comparison or even an assignment
    fn expression(&mut self) -> ExpressionResult {
        let expr = self.logic();

        if let Some(qm) = self.consume_or_ignore(TokenType::QuestionMark) {
            if let Some(colon) = self.consume_or_ignore(TokenType::Colon) {
                return Ok(Box::new(Node::Ternary {
                    check: expr?,
                    qm,
                    true_arm: None,
                    colon,
                    false_arm: self.expression()?,
                }));
            } else {
                return Ok(Box::new(Node::Ternary {
                    check: expr?,
                    qm,
                    true_arm: Some(self.expression()?),
                    colon: self.consume_cloned(TokenType::Colon)?,
                    false_arm: self.expression()?,
                }));
            }
        }

        expr
    }

    fn logic(&mut self) -> ExpressionResult {
        let mut expr = self.equality()?;

        let potential_matches = vec![TokenType::LogicOr, TokenType::LogicAnd];
        // Todo: Make this an if and force && or || in between. Basically add a new non-terminal
        while self.next_token_one_of(&potential_matches) {
            // Unwrap should be fine since the condition already checks that there is a next element
            let next = self.tokens.next().unwrap();
            let right = self.equality()?;

            expr = Box::new(Node::Binary {
                left: expr,
                token: next.clone(),
                right,
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ExpressionResult {
        let mut expr = self.comparison()?;

        let potential_matches = vec![
            TokenType::IsNotEqual,
            TokenType::IsEqual,
            TokenType::IsNotIdentical,
            TokenType::IsIdentical,
            TokenType::Coalesce,
            TokenType::InstanceOf,
        ];
        // Todo: Make this an if and force && or || in between. Basically add a new non-terminal
        while self.next_token_one_of(&potential_matches) {
            // Unwrap should be fine since the condition already checks that there is a next element
            let next = self.tokens.next().unwrap();
            let right = self.comparison()?;

            expr = Box::new(Node::Binary {
                left: expr,
                token: next.clone(),
                right,
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ExpressionResult {
        let mut expr = self.assignment()?;

        let potential_matches = vec![
            TokenType::Greater,
            TokenType::GreaterOrEqual,
            TokenType::Smaller,
            TokenType::SmallerOrEqual,
            TokenType::SpaceShip,
        ];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.assignment()?;

            expr = Box::new(Node::Binary {
                left: expr,
                token: next.clone(),
                right,
            });
        }

        Ok(expr)
    }

    /// Parses an assignment. First parses the l-value as a normal expression. Then determines if it is followed by an assignment
    /// operator which says we are looking at an assignment. Then checks, if the already parsed l-value is something that values
    /// can be assigned to. If it is, parse the r-value as another expression and wrap all up in an `Assignment`-expression. If not,
    /// return an error.
    fn assignment(&mut self) -> ExpressionResult {
        let expr = self.addition()?;

        if self.next_token_one_of(&vec![
            TokenType::Assignment,
            TokenType::BinaryAndAssignment,
            TokenType::BinaryOrAssignment,
            TokenType::ModuloAssignment,
            TokenType::ConcatAssignment,
            TokenType::XorAssignment,
            TokenType::RightShiftAssignment,
            TokenType::LeftShiftAssignment,
            TokenType::ModuloAssignment,
            TokenType::ConcatAssignment,
            TokenType::XorAssignment,
            TokenType::RightShiftAssignment,
            TokenType::LeftShiftAssignment,
            TokenType::PowerAssignment,
            TokenType::CoalesceAssignment,
            TokenType::PlusAssign,
            TokenType::MinusAssign,
            TokenType::MulAssign,
            TokenType::DivAssign,
        ]) {
            // Past the assignment token
            let operator = self.tokens.next().unwrap();
            let value = self.assignment()?;

            if expr.is_lvalue() {
                return Ok(Box::new(Node::Binary {
                    left: expr,
                    token: operator.clone(),
                    right: value,
                }));
            } else {
                return Err(format!(
                    "Unable to assign value to expression on line {}, col {}",
                    operator.line, operator.col
                ));
            }
        }

        Ok(expr)
    }
    fn addition(&mut self) -> ExpressionResult {
        let mut expr = self.multiplication()?;

        let potential_matches = vec![
            TokenType::Minus,
            TokenType::Plus,
            TokenType::LeftShift,
            TokenType::RightShift,
            TokenType::BinaryAnd,
            TokenType::BinaryOr,
            TokenType::Modulo,
        ];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.multiplication()?;

            expr = Box::new(Node::Binary {
                left: expr,
                token: next.clone(),
                right,
            });
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ExpressionResult {
        let mut expr = self.unary()?;

        let potential_matches = vec![
            TokenType::Multiplication,
            TokenType::Power,
            TokenType::Division,
            // TODO: Make sure precendence is the same, otherwise split
            TokenType::Concat,
            TokenType::BinaryOr,
            TokenType::BinaryAnd,
        ];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.unary()?;

            expr = Box::new(Node::Binary {
                left: expr,
                token: next.clone(),
                right,
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ExpressionResult {
        if self.next_token_one_of(&vec![
            TokenType::Negation,
            TokenType::Minus,
            TokenType::Increment,
            TokenType::Decrement,
            TokenType::BoolCast,
            TokenType::IntCast,
            TokenType::StringCast,
            TokenType::ArrayCast,
            TokenType::ObjectCast,
            TokenType::DoubleCast,
            TokenType::UnsetCast,
            TokenType::Elipsis,
            TokenType::Clone,
            TokenType::Silencer,
            TokenType::BinaryAnd,
            TokenType::BitwiseNegation,
        ]) {
            let next = self.tokens.next().unwrap();
            let right = self.unary()?;

            return Ok(Box::new(Node::Unary {
                token: next.clone(),
                expr: right,
            }));
        }

        let primary = self.call()?;

        if self.next_token_one_of(&vec![TokenType::Increment, TokenType::Decrement]) {
            let next = self.tokens.next().unwrap();

            return Ok(Box::new(Node::PostUnary {
                token: next.clone(),
                expr: primary,
            }));
        }
        Ok(primary)
    }

    /// Parses class-member access and array access. This also includes non-method call member access!
    fn call(&mut self) -> ExpressionResult {
        let expr = self.primary()?;

        self.init_call(expr)
    }

    fn init_call(&mut self, mut expr: Box<Node>) -> ExpressionResult {
        loop {
            if self.next_token_one_of(&vec![TokenType::OpenParenthesis]) {
                expr = self.finish_call(expr)?;

            // Accessing an object member
            } else if let Some(os) = self.consume_or_ignore(TokenType::ObjectOperator) {
                // Using the ->{} syntax, so the member is the result of an expression
                if self.consume_or_ignore(TokenType::OpenCurly).is_some() {
                    expr = Box::new(Node::Member {
                        object: expr,
                        arrow: os,
                        member: self.expression()?,
                    });
                    self.consume_or_err(TokenType::CloseCurly)?;

                // Using the ->member syntax, so the member is either an identifier or a variable
                } else if let Some(variable) = self.consume_or_ignore(TokenType::Variable) {
                    expr = Box::new(Node::Member {
                        object: expr,
                        arrow: os,
                        member: Box::new(Node::Literal(variable)),
                    });
                } else {
                    expr = Box::new(Node::Member {
                        object: expr,
                        arrow: os,
                        member: Box::new(Node::Literal(self.consume_identifier_cloned()?)),
                    });
                }
            } else if let Some(pn) = self.consume_or_ignore(TokenType::PaamayimNekudayim) {
                expr = Box::new(Node::StaticMember {
                    class: expr,
                    pn,
                    member: Box::new(Node::Literal(self.consume_member_cloned()?)),
                });
            } else if let Some(ob) = self.consume_or_ignore(TokenType::OpenBrackets) {
                // TODO: Think about a nicer solution for $a[] = ...
                expr = match self.consume_or_ignore(TokenType::CloseBrackets) {
                    Some(cb) => Box::new(Node::Field {
                        array: expr,
                        ob,
                        index: None,
                        cb,
                    }),
                    None => Box::new(Node::Field {
                        array: expr,
                        ob,
                        index: Some(self.expression()?),
                        cb: self.consume_cloned(TokenType::CloseBrackets)?,
                    }),
                };
            } else if let Some(oc) = self.consume_or_ignore(TokenType::OpenCurly) {
                expr = Box::new(Node::Field {
                    array: expr,
                    ob: oc,
                    index: Some(self.expression()?),
                    cb: self.consume_cloned(TokenType::CloseCurly)?,
                });
            } else if let Some(assignment) = self.consume_or_ignore(TokenType::Assignment) {
                // Something like static::$member = 1;
                expr = Box::new(Node::Binary {
                    left: expr,
                    token: assignment,
                    right: self.expression()?,
                });
            } else {
                break;
            }
        }

        return Ok(expr);
    }

    /// Parses all the parameters of a call
    fn finish_call(&mut self, expr: Box<Node>) -> ExpressionResult {
        let op = self.consume_cloned(TokenType::OpenParenthesis)?;

        let mut parameters = Vec::new();
        while !self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
            parameters.push(self.expression()?);

            if self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        Ok(Box::new(Node::Call {
            callee: expr,
            op,
            parameters,
            cp: self.consume_cloned(TokenType::CloseParenthesis)?,
        }))
    }

    /// Parses all the parameters of a call
    fn non_empty_parameter_list(&mut self) -> Result<Vec<Box<Node>>, String> {
        let mut arguments = Vec::new();
        arguments.push(self.expression()?);

        self.consume_or_ignore(TokenType::Comma);

        while !self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
            arguments.push(self.expression()?);

            if self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        Ok(arguments)
    }

    /// Parses all the arguments of a call
    fn parameter_list(&mut self) -> Result<Vec<Box<Node>>, String> {
        self.consume_or_err(TokenType::OpenParenthesis)?;

        let mut arguments = Vec::new();
        while !self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
            arguments.push(self.expression()?);

            if self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        self.consume_or_err(TokenType::CloseParenthesis)?;

        Ok(arguments)
    }

    /// Parses all the arguments of a call
    fn non_empty_lexical_variables_list(&mut self) -> Result<Vec<Box<Node>>, String> {
        self.consume_or_err(TokenType::OpenParenthesis)?;

        let mut arguments = Vec::new();
        arguments.push(self.lexical_variable()?);

        self.consume_or_ignore(TokenType::Comma);

        while !self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
            arguments.push(self.lexical_variable()?);

            if self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        self.consume_or_err(TokenType::CloseParenthesis)?;

        Ok(arguments)
    }

    /// Parses a lexical variable, used in a "use ()" list for example
    fn lexical_variable(&mut self) -> Result<Box<Node>, String> {
        Ok(Box::new(Node::LexicalVariable {
            reference: self.consume_or_ignore(TokenType::BinaryAnd),
            variable: self.consume_cloned(TokenType::Variable)?,
        }))
    }

    fn primary(&mut self) -> ExpressionResult {
        if self.next_token_one_of(&vec![
            TokenType::False,
            TokenType::True,
            TokenType::Null,
            TokenType::LongNumber,
            TokenType::DecimalNumber,
            TokenType::HexNumber,
            TokenType::ConstantEncapsedString,
            TokenType::EncapsedAndWhitespaceString,
            TokenType::Variable,
            TokenType::ConstDir,
            TokenType::ConstFile,
            TokenType::ConstFunction,
            TokenType::ConstLine,
            TokenType::ConstMethod,
            TokenType::ConstTrait,
            TokenType::ConstClass,
        ]) {
            return Ok(Box::new(Node::Literal(self.tokens.next().unwrap().clone())));
        }

        if self.next_token_one_of(&vec![TokenType::NamespaceSeparator, TokenType::Identifier]) {
            // Load path as identifier
            return self.non_empty_type_ref();
        }

        if self.next_token_one_of(&vec![TokenType::HereDocStart]) {
            self.consume_or_err(TokenType::HereDocStart)?;
            let string = self.tokens.next().unwrap().clone();
            self.consume_or_err(TokenType::HereDocEnd)?;

            return Ok(Box::new(Node::Literal(string)));
        }

        if let Some(isset) = self.consume_or_ignore(TokenType::Isset) {
            return Ok(Box::new(Node::Isset {
                isset,
                op: self.consume_cloned(TokenType::OpenParenthesis)?,
                parameters: self.non_empty_parameter_list()?,
                cp: self.consume_cloned(TokenType::CloseParenthesis)?,
            }));
        }

        if let Some(empty) = self.consume_or_ignore(TokenType::Empty) {
            return Ok(Box::new(Node::Empty {
                empty,
                op: self.consume_cloned(TokenType::OpenParenthesis)?,
                parameters: self.non_empty_parameter_list()?,
                cp: self.consume_cloned(TokenType::CloseParenthesis)?,
            }));
        }

        if self.next_token_one_of(&vec![TokenType::TypeArray]) {
            return self.old_array();
        }

        if self.next_token_one_of(&vec![TokenType::List]) {
            return self.list();
        }

        if let Some(include) = self.consume_one_of_cloned_or_ignore(&vec![
            TokenType::Require,
            TokenType::RequireOnce,
            TokenType::Include,
            TokenType::IncludeOnce,
        ]) {
            return Ok(Box::new(Node::FileInclude {
                token: include,
                resource: self.expression()?,
            }));
        }

        if self.next_token_one_of(&vec![TokenType::OpenBrackets]) {
            let expr = self.array()?;

            return Ok(expr);
        }

        if self.next_token_one_of(&vec![TokenType::OpenParenthesis]) {
            self.consume_or_err(TokenType::OpenParenthesis)?;
            let expr = self.expression()?;
            self.consume_or_err(TokenType::CloseParenthesis)?;

            return Ok(Box::new(Node::Grouping(expr)));
        }

        if self.next_token_one_of(&vec![TokenType::Function]) {
            return self.anonymous_function(None);

            // Can be either "static function ..." or "static::$lol"
        }

        // Static is fun ... watch this ...
        if let Some(static_token) = self.consume_or_ignore(TokenType::Static) {
            // Followed by ::? Probably a member access
            if self.next_token_one_of(&vec![TokenType::PaamayimNekudayim]) {
                return Ok(Box::new(Node::Literal(static_token.clone())));
            }

            // Followed by "function"? Static function expression
            if self.next_token_one_of(&vec![TokenType::Function]) {
                return self.anonymous_function(Some(static_token));
            }

            // Otherwise probably used in a instantiation context
            return Ok(Box::new(Node::Literal(static_token.clone())));
        }

        // Self is like static but less mighty
        if let Some(self_token) = self.consume_or_ignore(TokenType::TypeSelf) {
            // Followed by ::? Probably a member access
            if self.next_token_one_of(&vec![TokenType::PaamayimNekudayim]) {
                return Ok(Box::new(Node::Literal(self_token.clone())));
            }

            // Otherwise ... no clue if an error after all. Need to check official grammar
            return Ok(Box::new(Node::Literal(self_token.clone())));
        }

        if self.next_token_one_of(&vec![TokenType::Class]) {
            return self.anonymous_class();
        }

        if let Some(new) = self.consume_or_ignore(TokenType::New) {
            return Ok(Box::new(Node::New {
                token: new,
                class: self.call()?,
            }));
        }

        if let Some(token) = self.consume_or_ignore(TokenType::Yield) {
            if let Some(from) = self.consume_or_ignore(TokenType::From) {
                return Ok(Box::new(Node::YieldFrom {
                    token: from,
                    expr: self.expression()?,
                }));
            }

            return Ok(Box::new(Node::Yield {
                token,
                expr: self.array_pair()?,
            }));
        }

        let next = self.tokens.peek().unwrap();
        Err(format!("Unsupported primary {:?}", next))
    }

    fn anonymous_function(&mut self, is_static: Option<Token>) -> ExpressionResult {
        let token = self.consume_cloned(TokenType::Function)?;

        self.consume_or_err(TokenType::OpenParenthesis)?;
        let arguments = self.argument_list()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;

        let uses = match self.next_token_one_of(&vec![TokenType::Use]) {
            true => {
                self.tokens.next();
                Some(self.non_empty_lexical_variables_list()?)
            }
            false => None,
        };
        let return_type = self.return_type()?;

        self.consume_or_err(TokenType::OpenCurly)?;
        let body = self.block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(Node::Function {
            is_static,
            token,
            arguments,
            return_type,
            uses,
            body,
        }))
    }

    fn array(&mut self) -> ExpressionResult {
        let start = self.consume_cloned(TokenType::OpenBrackets)?;
        let mut elements = Vec::new();

        while !self.next_token_one_of(&vec![TokenType::CloseBrackets]) {
            elements.push(self.array_pair()?);

            self.consume_or_ignore(TokenType::Comma);
        }

        Ok(Box::new(Node::Array {
            ob: start,
            elements,
            cb: self.consume_cloned(TokenType::CloseBrackets)?,
        }))
    }

    fn old_array(&mut self) -> ExpressionResult {
        let start = self.consume_cloned(TokenType::TypeArray)?;
        let op = self.consume_cloned(TokenType::OpenParenthesis)?;
        let mut elements = Vec::new();

        while !self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
            elements.push(self.array_pair()?);

            self.consume_or_ignore(TokenType::Comma);
        }

        Ok(Box::new(Node::OldArray {
            token: start,
            op,
            elements,
            cp: self.consume_cloned(TokenType::CloseParenthesis)?,
        }))
    }

    /// Parses the list destructuring operation
    fn list(&mut self) -> ExpressionResult {
        let start = self.consume_cloned(TokenType::List)?;
        let op = self.consume_cloned(TokenType::OpenParenthesis)?;
        let mut elements = Vec::new();

        while !self.next_token_one_of(&vec![TokenType::CloseParenthesis]) {
            // Empty element ... list(,,,$a)
            if let Some(_) = self.consume_or_ignore(TokenType::Comma) {
                continue;
            }

            elements.push(self.array_pair()?);

            self.consume_or_ignore(TokenType::Comma);
        }

        Ok(Box::new(Node::List {
            token: start,
            op,
            elements,
            cp: self.consume_cloned(TokenType::CloseParenthesis)?,
        }))
    }

    fn array_pair(&mut self) -> ExpressionResult {
        // At this point key might as well be the value
        let key = match self.next_token_one_of(&vec![TokenType::BinaryAnd]) {
            true => self.lexical_variable()?,
            false => self.expression()?,
        };

        if let Some(arrow) = self.consume_or_ignore(TokenType::DoubleArrow) {
            // TODO: Raise warning if key is access by reference ... this no works

            // Todo: Rather check for scalarity
            if !key.is_offset() {
                return Err(format!(
                    "Illegal offset type at line {} col {}",
                    arrow.line, arrow.col,
                ));
            }

            if self.next_token_one_of(&vec![TokenType::BinaryAnd]) {
                Ok(Box::new(Node::ArrayElement {
                    key: Some(key),
                    arrow: Some(arrow),
                    value: self.lexical_variable()?,
                }))
            } else {
                Ok(Box::new(Node::ArrayElement {
                    key: Some(key),
                    arrow: Some(arrow),
                    value: self.expression()?,
                }))
            }
        } else {
            Ok(Box::new(Node::ArrayElement {
                key: None,
                arrow: None,
                value: key,
            }))
        }
    }

    fn consume_end_of_statement(&mut self) -> Result<(), String> {
        if let Some(token) = self.tokens.peek() {
            if token.t == TokenType::Semicolon || token.t == TokenType::ScriptEnd {
                self.tokens.next();
                return Ok(());
            }

            return Err(format!(
                "Expected end of statement, found {:?} on line {}, col {}",
                token.t, token.line, token.col
            ));
        }

        Ok(())
    }

    fn consume_or_err(&mut self, t: TokenType) -> Result<(), String> {
        if let Some(token) = self.tokens.peek() {
            if token.t == t {
                self.tokens.next();
                return Ok(());
            }

            return Err(format!(
                "Expected {:?}, found {:?} on line {}, col {}",
                t, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected {:?}, found end of file.", t))
    }
    fn consume_or_ignore(&mut self, t: TokenType) -> Option<Token> {
        if let Some(&token) = self.tokens.peek() {
            if token.t != t {
                return None;
            }
        } else {
            return None;
        }

        Some(self.tokens.next().unwrap().clone())
    }

    fn consume_cloned(&mut self, t: TokenType) -> Result<Token, String> {
        if let Some(token) = self.tokens.next() {
            if token.t == t {
                return Ok(token.clone());
            }

            return Err(format!(
                "Expected {:?}, found {:?} on line {}, col {}",
                t, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected {:?}, found end of file.", t))
    }

    fn consume_identifier_cloned(&mut self) -> Result<Token, String> {
        if let Some(token) = self.tokens.next() {
            if token.is_identifier() {
                return Ok(token.clone());
            }

            return Err(format!(
                "Expected Identifier, found {:?} on line {}, col {}",
                token.t, token.line, token.col
            ));
        }

        Err(format!("Expected Identifier, found end of file."))
    }

    fn consume_member_cloned(&mut self) -> Result<Token, String> {
        if let Some(token) = self.tokens.next() {
            if token.is_identifier() || token.t == TokenType::Variable {
                return Ok(token.clone());
            }

            return Err(format!(
                "Expected Identifier or Variable, found {:?} on line {}, col {}",
                token.t, token.line, token.col
            ));
        }

        Err(format!("Expected Identifier, found end of file."))
    }

    fn consume_one_of_cloned(&mut self, types: &Vec<TokenType>) -> Result<Token, String> {
        if let Some(token) = self.tokens.next() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Ok(token.clone());
                }
            }

            return Err(format!(
                "Expected one of {:?}, found {:?} on line {}, col {}",
                types, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected one of {:?}, found end of file.", types))
    }

    fn consume_one_of_cloned_or_ignore(&mut self, types: &Vec<TokenType>) -> Option<Token> {
        if let Some(token) = self.tokens.peek() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Some(self.tokens.next().unwrap().clone());
                }
            }
        }

        None
    }

    fn next_token_one_of(&mut self, types: &Vec<TokenType>) -> bool {
        if let Some(&token) = self.tokens.peek() {
            for tt in types {
                if *tt == token.t {
                    return true;
                }
            }
        }

        return false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::Scanner;

    #[test]
    fn test_creates_ast_for_addition() {
        let mut scanner = Scanner::new("<?php\n1 + 2 == 3;");
        scanner.scan().unwrap();

        let mut parser = Parser::new(&scanner.tokens);

        println!("{:?}", parser.ast());
    }
}
