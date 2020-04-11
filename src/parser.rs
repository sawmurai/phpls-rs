use crate::expression::*;
use crate::statement::*;
use crate::token::{Token, TokenType};

use std::iter::Iterator;
use std::iter::Peekable;
use std::slice::Iter;

type FuncDefStatementResult = Result<Box<FunctionDefinitionStatement>, String>;
type StatementResult = Result<Box<dyn Stmt>, String>;
type ClassStatementResult = Result<Box<dyn Stmt>, String>;
type StatementListResult = Result<Vec<Box<dyn Stmt>>, String>;
type ArgumentListResult = Result<Option<Vec<FunctionArgument>>, String>;
type ReturnTypeResult = Result<ReturnType, String>;
type PathExpressionResult = Result<Box<PathExpression>, String>;
type ExpressionResult = Result<Box<dyn Expr>, String>;

/// Inspired by https://craftinginterpreters.com/statements-and-state.html
///
/// Parses a token stream of a `Scanner` and generates an Abstract Syntax Tree
#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
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
            statements.push(self.statement()?);
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
        while !self.next_token_one_of(&vec![TokenType::CloseCurly]) {
            statements.push(self.statement()?);
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

        while let Some(next) = self.tokens.next() {
            let visibility = match self.next_token_one_of(&vec![
                TokenType::Public,
                TokenType::Private,
                TokenType::Protected,
            ]) {
                true => {
                    self.tokens.next();

                    Some(next.t.clone())
                }
                false => None,
            };

            if self.next_token_one_of(&vec![TokenType::Const]) {
                self.tokens.next();
                let name = self.consume_cloned(TokenType::Identifier)?;

                self.consume_or_err(TokenType::Assignment)?;
                statements.push(Box::new(ClassConstantDefinitionStatement::new(
                    name,
                    visibility,
                    self.expression()?,
                )));

                continue;
            };

            let is_static = match self.next_token_one_of(&vec![TokenType::Static]) {
                true => {
                    self.tokens.next();

                    true
                }
                false => false,
            };

            if let Some(next) = self.tokens.peek() {
                match next.t {
                    TokenType::Function => {
                        self.tokens.next();
                        let name = self.consume_cloned(TokenType::Identifier)?;

                        statements.push(Box::new(MethodDefinitionStatement::new(
                            name,
                            visibility,
                            self.function()?,
                            is_static,
                        )));
                    }
                    // One or more of those ...
                    TokenType::Variable => loop {
                        // The next variable
                        let name = self.consume_cloned(TokenType::Variable)?;
                        let assignment = match self.next_token_one_of(&vec![TokenType::Assignment])
                        {
                            true => {
                                self.tokens.next();
                                Some(self.expression()?)
                            }
                            false => None,
                        };

                        statements.push(Box::new(PropertyDefinitionStatement::new(
                            name,
                            visibility.clone(),
                            assignment,
                            is_static,
                        )));

                        if !self.next_token_one_of(&vec![TokenType::Comma]) {
                            break;
                        }

                        // The comma
                        self.tokens.next();
                    },
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
    fn function(&mut self) -> FuncDefStatementResult {
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let arguments = self.argument_list()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;

        let return_type = match self.next_token_one_of(&vec![TokenType::Colon]) {
            true => {
                self.tokens.next();
                Some(self.return_type()?)
            }
            false => None,
        };

        self.consume_or_err(TokenType::OpenCurly)?;
        let body = self.block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(FunctionDefinitionStatement::new(
            arguments,
            return_type,
            body,
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
            let nullable = match self.next_token_one_of(&vec![TokenType::QuestionMark]) {
                true => {
                    self.tokens.next();
                    true
                }
                false => false,
            };

            let t = match self.next_token_one_of(&vec![
                TokenType::TypeString,
                TokenType::TypeObject,
                TokenType::TypeInt,
                TokenType::TypeFloat,
                TokenType::TypeBool,
                TokenType::TypeArray,
                TokenType::Identifier,
            ]) {
                true => match self.tokens.next() {
                    Some(token) => Some(token.clone()),
                    None => unreachable!("Should not happen!"),
                },
                false => None,
            };

            let name = self.consume_cloned(TokenType::Variable)?;

            let default = match self.next_token_one_of(&vec![TokenType::Assignment]) {
                true => {
                    self.tokens.next();

                    if let Some(default) = self.tokens.next() {
                        match default.t {
                            TokenType::LongNumber
                            | TokenType::DecimalNumber
                            | TokenType::ConstantEncapsedString
                            | TokenType::Null
                            | TokenType::EncapsedAndWhitespaceString => Some(default.clone()),
                            _ => {
                                return Err(format!(
                                    "Unexpected {:?} on line {}, col {}",
                                    default.t, default.line, default.col
                                ));
                            }
                        }
                    } else {
                        return Err(String::from("Missing default value for argument!"));
                    }
                }
                false => None,
            };

            arguments.push(FunctionArgument::new(t, name, nullable, default));

            if self.next_token_one_of(&vec![TokenType::Comma]) {
                self.tokens.next();
            } else {
                break;
            }
        }

        Ok(Some(arguments))
    }

    /// Parses the return type of a function, excluding the colon
    ///
    /// # Details
    /// ```php
    /// function my_funy (string $a, int $b): /** from here **/?int/** to here **/ {
    ///     echo "Hello!";
    /// }
    /// ```
    fn return_type(&mut self) -> ReturnTypeResult {
        let nullable = match self.next_token_one_of(&vec![TokenType::QuestionMark]) {
            true => {
                self.tokens.next();
                true
            }
            false => false,
        };

        if let Some(token) = self.tokens.next() {
            match token.t {
                TokenType::TypeString
                | TokenType::TypeObject
                | TokenType::TypeInt
                | TokenType::TypeFloat
                | TokenType::TypeBool
                | TokenType::TypeArray
                | TokenType::Void
                | TokenType::Null
                | TokenType::Identifier => Ok(ReturnType::new(token.clone(), nullable)),
                _ => {
                    return Err(format!(
                        "Unexpected {:?} on line {}, col {}",
                        token.t, token.line, token.col
                    ));
                }
            }
        } else {
            return Err(String::from("EOF when searching for return type"));
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
                TokenType::Namespace => {
                    self.tokens.next();

                    return self.namespace_statement();
                }
                TokenType::Use => {
                    self.tokens.next();

                    return self.use_statement();
                }
                TokenType::Echo => {
                    self.tokens.next();

                    return self.echo_statement();
                }
                TokenType::Class => {
                    self.tokens.next();
                    return self.class_statement(false);
                }
                TokenType::Abstract => {
                    self.tokens.next();
                    return self.abstract_class_statement();
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
                    )));
                }
                // TODO: Add break/continue level, probably have separate statements. Same game with "return"
                TokenType::Break | TokenType::Continue => {
                    let statement =
                        Box::new(TokenStatement::new(self.consume_cloned(token.t.clone())?));

                    self.consume_or_err(TokenType::Semicolon)?;

                    return Ok(statement);
                }
                TokenType::Try => {
                    self.tokens.next();
                    return self.try_catch_statement();
                }
                TokenType::Declare => {
                    return self.expression_statement();
                }
                _ => {
                    let expr = self.expression_statement()?;

                    self.consume_or_err(TokenType::Semicolon)?;

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
                Some(self.block()?)
            }
            _ => None,
        };

        self.consume_or_err(TokenType::CloseCurly)?;

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
        let types = self.path_list()?;
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
    fn path_list(&mut self) -> Result<Vec<Box<PathExpression>>, String> {
        let mut paths = vec![self.path()?];

        while self.next_token_one_of(&vec![TokenType::BinaryOr]) {
            self.tokens.next();
            paths.push(self.path()?);
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

    fn case_list(&mut self) -> Result<Vec<Option<Box<dyn Expr>>>, String> {
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
                        .or_else(|_| self.consume_or_err(TokenType::Semicolon))?;
                }
                Some(Token {
                    t: TokenType::Case, ..
                }) => {
                    self.tokens.next();
                    cases_current_branch.push(Some(self.expression()?));
                    self.consume_or_err(TokenType::Colon)
                        .or_else(|_| self.consume_or_err(TokenType::Semicolon))?;
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
        self.consume_or_err(TokenType::Semicolon)?;

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
        loop {
            init.push(self.expression_statement()?);

            if self.next_token_one_of(&vec![TokenType::Comma]) {
                self.tokens.next();
            } else {
                break;
            }
        }

        self.consume_or_err(TokenType::Semicolon)?;

        let mut condition = Vec::new();
        loop {
            condition.push(self.expression_statement()?);

            if self.next_token_one_of(&vec![TokenType::Comma]) {
                self.tokens.next();
            } else {
                break;
            }
        }

        self.consume_or_err(TokenType::Semicolon)?;

        let mut step = Vec::new();
        loop {
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
        // Make sure this does not start with a \ or something
        self.peek_or_err(TokenType::Identifier)?;
        let path = self.path()?;

        // TODO: Implement block
        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(NamespaceStatement::new(path)))
    }

    // use -> "use" ("function" | "const")? path (("{" use_group "}") | ("as" identifier))?
    fn use_statement(&mut self) -> StatementResult {
        let path = self.path()?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(format!("Unexpected EOF"));
        };

        let stmt = match next.t {
            TokenType::As => {
                self.tokens.next();
                UseStatement::aliased(path, self.consume_cloned(TokenType::Identifier)?)
            }
            TokenType::OpenCurly => {
                self.tokens.next();
                UseStatement::grouped(path, self.use_group()?)
            }
            TokenType::Semicolon => UseStatement::new(path),
            _ => {
                return Err(format!(
                    "Unexpected {:?} on line {}, col {}",
                    next.t, next.line, next.col
                ));
            }
        };

        // TODO: Implement block
        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(stmt))
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
        let path = self.path()?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(String::from("Unexpected EOF"));
        };

        let stmt = match next.t {
            TokenType::As => {
                self.tokens.next();
                UseStatement::aliased(path, self.consume_cloned(TokenType::Identifier)?)
            }
            _ => UseStatement::new(path),
        };

        Ok(Box::new(stmt))
    }

    fn echo_statement(&mut self) -> StatementResult {
        let value = self.expression()?;

        self.consume_or_err(TokenType::Semicolon)?;

        Ok(Box::new(EchoStatement::new(value)))
    }

    // abstract_class -> "abstract" class
    fn abstract_class_statement(&mut self) -> ClassStatementResult {
        self.tokens.next();

        self.class_statement(true)
    }

    // class -> "class" identifier (extends identifier_list)? (implements identifier_list)?
    fn class_statement(&mut self, is_abstract: bool) -> ClassStatementResult {
        let name = self.consume_cloned(TokenType::Identifier)?;

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(format!("Unexpected EOF"));
        };

        let mut extends = None;
        if next.t == TokenType::Extends {
            self.tokens.next();
            extends = Some(self.identifier_list()?);
        }

        let next = if let Some(next) = self.tokens.peek() {
            next
        } else {
            return Err(format!("Unexpected EOF"));
        };

        let mut implements = None;
        if next.t == TokenType::Implements {
            self.tokens.next();
            implements = Some(self.identifier_list()?);
        }

        self.consume_or_err(TokenType::OpenCurly)?;
        let block = self.class_block()?;
        self.consume_or_err(TokenType::CloseCurly)?;

        Ok(Box::new(ClassStatement::new(
            name,
            is_abstract,
            extends,
            implements,
            block,
        )))
    }

    // (("extends" identifier) (, "extends" identifier)*)?
    // (("implements" identifier) (, "implements" identifier)*)?
    fn identifier_list(&mut self) -> Result<Vec<Token>, String> {
        let mut extends = Vec::new();

        loop {
            extends.push(self.consume_cloned(TokenType::Identifier)?);

            if !self.next_token_one_of(&vec![TokenType::Comma]) {
                break;
            }

            self.tokens.next();
        }

        Ok(extends)
    }

    fn expression_statement(&mut self) -> StatementResult {
        let value = self.expression()?;

        Ok(Box::new(ExpressionStatement::new(value)))
    }

    /// Parses an expression. This can be anything that evaluates to a value. A function call, a comparison or even an assignment
    fn expression(&mut self) -> ExpressionResult {
        self.assignment()
    }

    /// Parses an assignment. First parses the l-value as a normal expression. Then determines if it is followed by an assignment
    /// operator which says we are looking at an assignment. Then checks, if the already parsed l-value is something that values
    /// can be assigned to. If it is, parse the r-value as another expression and wrap all up in an `Assignment`-expression. If not,
    /// return an error.
    fn assignment(&mut self) -> ExpressionResult {
        let expr = self.equality()?;

        if let Some(Token {
            t: TokenType::Assignment,
            line,
            col,
            ..
        }) = self.tokens.peek()
        {
            // Past the assignment token
            self.tokens.next();
            let value = self.assignment()?;

            if expr.is_offset() {
                return Ok(Box::new(Assignment::new(expr, value)));
            } else {
                return Err(format!(
                    "Unable to assign value to expression on line {}, col {}",
                    line, col
                ));
            }
        }

        Ok(expr)
    }

    // path -> identifier ("\" identifier)*
    fn path(&mut self) -> PathExpressionResult {
        let absolute = self.consume_or_ignore(TokenType::NamespaceSeparator);

        let mut path = vec![self.consume_cloned(TokenType::Identifier)?];
        let matches_ns = vec![TokenType::NamespaceSeparator];
        let matches_ident = vec![TokenType::Identifier];

        loop {
            // Next is another namespace separator
            if !self.next_token_one_of(&matches_ns) {
                break;
            }

            self.consume_or_err(TokenType::NamespaceSeparator)?;

            if !self.next_token_one_of(&matches_ident) {
                break;
            }

            path.push(self.consume_cloned(TokenType::Identifier)?);
        }

        Ok(Box::new(PathExpression::new(absolute.is_some(), path)))
    }

    fn equality(&mut self) -> ExpressionResult {
        let mut expr = self.comparison()?;

        let potential_matches = vec![TokenType::IsNotEqual, TokenType::IsEqual];
        // Todo: Make this an if and force && or || in between. Basically add a new non-terminal
        while self.next_token_one_of(&potential_matches) {
            // Unwrap should be fine since the condition already checks that there is a next element
            let next = self.tokens.next().unwrap();
            let right = self.comparison()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ExpressionResult {
        let mut expr = self.addition()?;

        let potential_matches = vec![
            TokenType::Greater,
            TokenType::GreaterOrEqual,
            TokenType::Smaller,
            TokenType::SmallerOrEqual,
        ];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.addition()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> ExpressionResult {
        let mut expr = self.multiplication()?;

        let potential_matches = vec![TokenType::Minus, TokenType::Plus];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.multiplication()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> ExpressionResult {
        let mut expr = self.unary()?;

        let potential_matches = vec![TokenType::Multiplication, TokenType::Division];

        while self.next_token_one_of(&potential_matches) {
            let next = self.tokens.next().unwrap();
            let right = self.unary()?;

            expr = Box::new(Binary::new(expr, next.clone(), right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ExpressionResult {
        if self.next_token_one_of(&vec![
            TokenType::Negation,
            TokenType::Minus,
            TokenType::Increment,
            TokenType::Decrement,
        ]) {
            let next = self.tokens.next().unwrap();
            let right = self.unary()?;

            return Ok(Box::new(Unary::new(next.clone(), right)));
        }

        let primary = self.call()?;

        if self.next_token_one_of(&vec![TokenType::Increment, TokenType::Decrement]) {
            let next = self.tokens.next().unwrap();

            return Ok(Box::new(PostUnary::new(next.clone(), primary)));
        }
        Ok(primary)
    }

    /// Parses class-member access and array access.
    fn call(&mut self) -> ExpressionResult {
        let mut expr = self.primary()?;

        loop {
            if self.next_token_one_of(&vec![TokenType::OpenParenthesis]) {
                expr = self.finish_call(expr)?;
            } else if self.next_token_one_of(&vec![TokenType::ObjectOperator]) {
                self.tokens.next();
                expr = Box::new(Member::new(
                    expr,
                    self.consume_one_of_cloned(&vec![TokenType::Identifier, TokenType::Variable])?,
                    false,
                ));
            } else if self.next_token_one_of(&vec![TokenType::PaamayimNekudayim]) {
                self.tokens.next();
                expr = Box::new(Member::new(
                    expr,
                    self.consume_one_of_cloned(&vec![TokenType::Identifier, TokenType::Variable])?,
                    true,
                ));
            } else if self.next_token_one_of(&vec![TokenType::OpenBrackets]) {
                self.tokens.next();
                expr = Box::new(Field::new(expr, self.expression()?));
                self.consume_cloned(TokenType::CloseBrackets)?;
            } else {
                break;
            }
        }

        return Ok(expr);
    }

    /// Parses all the arguments of a call
    fn finish_call(&mut self, expr: Box<dyn Expr>) -> ExpressionResult {
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

        Ok(Box::new(Call::new(expr, arguments)))
    }

    fn primary(&mut self) -> ExpressionResult {
        if self.next_token_one_of(&vec![
            TokenType::False,
            TokenType::True,
            TokenType::Null,
            TokenType::LongNumber,
            TokenType::DecimalNumber,
            TokenType::ConstantEncapsedString,
            TokenType::EncapsedAndWhitespaceString,
            TokenType::Variable,
            TokenType::Identifier,
            TokenType::Declare,
        ]) {
            return Ok(Box::new(Literal::new(self.tokens.next().unwrap().clone())));
        }

        if self.next_token_one_of(&vec![TokenType::OpenBrackets]) {
            let expr = self.array()?;

            return Ok(expr);
        } else if self.next_token_one_of(&vec![TokenType::OpenParenthesis]) {
            self.consume_or_err(TokenType::OpenParenthesis)?;
            let expr = self.expression()?;
            self.consume_or_err(TokenType::CloseParenthesis)?;

            return Ok(Box::new(Grouping::new(expr)));
        }

        let next = self.tokens.peek().unwrap();
        Err(format!("Unsupported primary {:?}", next))
    }

    fn array(&mut self) -> ExpressionResult {
        let start = self.consume_cloned(TokenType::OpenBrackets)?;
        let mut elements = Vec::new();

        while !self.next_token_one_of(&vec![TokenType::CloseBrackets]) {
            elements.push(self.array_pair()?);

            self.consume_or_ignore(TokenType::Comma);
        }

        Ok(Box::new(Array::new(
            start,
            elements,
            self.consume_cloned(TokenType::CloseBrackets)?,
        )))
    }

    fn array_pair(&mut self) -> ExpressionResult {
        let key = self.expression()?;

        if self.consume_or_ignore(TokenType::DoubleArrow).is_some() {
            // Todo: Rather check for scalarity
            if !key.is_offset() {
                return Err(format!(
                    "Illegal offset type at line {} col {}",
                    key.line(),
                    key.col(),
                ));
            }

            Ok(Box::new(ArrayPair::new(Some(key), self.expression()?)))
        } else {
            Ok(Box::new(ArrayPair::new(None, key)))
        }
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
        }

        Some(self.tokens.next().unwrap().clone())
    }

    fn peek_or_err(&mut self, t: TokenType) -> Result<(), String> {
        if let Some(token) = self.tokens.peek() {
            if token.t == t {
                return Ok(());
            }

            return Err(format!(
                "Expected {:?}, found {:?} on line {}, col {}",
                t, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected {:?}, found end of file.", t))
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
