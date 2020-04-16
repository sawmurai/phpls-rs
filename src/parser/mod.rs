use crate::expression::*;
use crate::statement::*;
use crate::token::{Token, TokenType};

type StatementResult = Result<Box<dyn Stmt>, String>;
type StatementListResult = Result<Vec<Box<dyn Stmt>>, String>;
type ArgumentListResult = Result<Option<Vec<Node>>, String>;
type ExpressionResult = Result<Node, String>;
type AstResult = Result<(Vec<Box<dyn Stmt>>, Vec<String>), String>;

pub mod conditionals;
pub mod loops;
pub mod try_catch;

/// Inspired by https://craftinginterpreters.com/statements-and-state.html
///
/// Parses a token stream of a `Scanner` and generates an Abstract Syntax Tree
#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    errors: Vec<String>,
}

impl Parser {
    pub fn errors(&self) -> &Vec<String> {
        self.errors.as_ref()
    }

    /// Fast forwards to the end of the current statement or block
    fn error_fast_forward(&mut self) {
        while self.peek().is_some() {
            self.next();

            if self.next_token_one_of(&[TokenType::Semicolon]) {
                self.next();

                break;
            }

            if self.next_token_one_of(&[TokenType::CloseCurly]) {
                self.next();
                break;
            }
        }
    }

    /// Parses the entire token stream and returns an abstract syntax tree representation and a vector of
    /// accumulated parse errors.
    ///
    /// # Example
    /// ```
    /// // Content contains the source code
    /// let mut scanner = Scanner::new(&content);
    /// scanner.scan()?;
    /// let Ok(ast, errors) = Parser::ast(scanner.tokens);
    /// ```
    pub fn ast(mut tokens: Vec<Token>) -> AstResult {
        tokens.reverse();

        let mut parser = Parser {
            tokens,
            errors: Vec::new(),
        };

        let mut statements: Vec<Box<dyn Stmt>> = Vec::new();

        parser.consume_or_err(TokenType::ScriptStart)?;

        while parser.peek().is_some() {
            match parser.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    parser.errors.push(error);
                    parser.error_fast_forward();
                }
            }
        }

        Ok((statements, parser.errors))
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

        self.consume_or_err(TokenType::OpenCurly)?;

        // TODO: Make sure namespace etc can not pop up here
        while !self.next_token_one_of(&[TokenType::CloseCurly]) && self.peek().is_some() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => {
                    self.errors.push(error);
                    self.error_fast_forward();
                }
            }
        }

        self.consume_or_err(TokenType::CloseCurly)?;

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

        while !self.next_token_one_of(&[TokenType::CloseCurly]) {
            if self.next_token_one_of(&[TokenType::Use]) {
                statements.push(self.use_trait_statement()?);

                continue;
            }

            let mut is_abstract = None;
            let mut is_final = None;
            let mut visibility = None;
            let mut is_static = None;

            // Collect all modifiers
            while self.next_token_one_of(&[
                TokenType::Abstract,
                TokenType::Final,
                TokenType::Public,
                TokenType::Var,
                TokenType::Private,
                TokenType::Protected,
                TokenType::Static,
            ]) {
                is_abstract = self.consume_or_ignore(TokenType::Abstract);
                is_final = self.consume_or_ignore(TokenType::Final);
                visibility = self.consume_one_of_cloned_or_ignore(&[
                    TokenType::Public,
                    TokenType::Var,
                    TokenType::Private,
                    TokenType::Protected,
                ]);
                is_static = self.consume_or_ignore(TokenType::Static);
            }

            if self.next_token_one_of(&[TokenType::Const]) {
                self.next();
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

            if let Some(next) = self.peek() {
                match next.t {
                    TokenType::Function => {
                        self.next();

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
                            let assignment = if self.next_token_one_of(&[TokenType::Assignment]) {
                                self.next();
                                Some(self.expression()?)
                            } else {
                                None
                            };

                            statements.push(Box::new(PropertyDefinitionStatement::new(
                                name,
                                visibility.clone(),
                                is_abstract.clone(),
                                assignment,
                                is_static.clone(),
                            )));

                            if !self.next_token_one_of(&[TokenType::Comma]) {
                                break;
                            }

                            // The comma
                            self.next();
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
            }) = self.peek()
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

        let body = self.block()?;

        Ok(Box::new(FunctionDefinitionStatement::new(
            arguments,
            return_type,
            Some(body),
        )))
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

        if self.next_token_one_of(&[TokenType::CloseParenthesis]) {
            return Ok(None);
        }

        loop {
            let t = self.argument_type()?;
            let spread = self.consume_or_ignore(TokenType::Elipsis);
            let reference = self.consume_or_ignore(TokenType::BinaryAnd);
            let name = self.consume_cloned(TokenType::Variable)?;
            let has_default = self.consume_or_ignore(TokenType::Assignment);

            let default_value = if has_default.is_some() {
                Some(Box::new(self.expression()?))
            } else {
                None
            };

            arguments.push(Node::FunctionArgument {
                argument_type: Box::new(t),
                name,
                spread,
                reference,
                has_default,
                default_value,
            });

            if self.next_token_one_of(&[TokenType::Comma]) {
                self.next();
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
    fn argument_type(&mut self) -> Result<Option<Node>, String> {
        if let Some(qm) = self.consume_or_ignore(TokenType::QuestionMark) {
            Ok(Some(Node::ArgumentType {
                nullable: Some(qm),
                type_ref: Box::new(self.non_empty_type_ref()?),
            }))
        } else if let Some(type_ref) = self.type_ref()? {
            Ok(Some(Node::ArgumentType {
                nullable: None,
                type_ref: Box::new(type_ref),
            }))
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
    fn return_type(&mut self) -> Result<Option<Node>, String> {
        if let Some(colon) = self.consume_or_ignore(TokenType::Colon) {
            Ok(Some(Node::ReturnType {
                token: colon,
                nullable: self.consume_or_ignore(TokenType::QuestionMark),
                type_ref: Box::new(self.non_empty_type_ref()?),
            }))
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
        if let Some(token) = self.peek() {
            match token.t {
                TokenType::ScriptEnd => {
                    return self.inline_html();
                }
                TokenType::Function => {
                    self.next();

                    return self.named_function();
                }
                TokenType::Namespace => {
                    self.next();

                    return self.namespace_statement();
                }
                TokenType::Use => {
                    self.next();

                    if self.consume_or_ignore(TokenType::Function).is_some() {
                        return self.use_function_statement();
                    }

                    if self.consume_or_ignore(TokenType::Const).is_some() {
                        return self.use_const_statement();
                    }

                    return self.use_statement();
                }
                TokenType::Const => {
                    self.next();

                    return self.const_statement();
                }
                TokenType::Global => {
                    self.next();

                    return self.global_variables();
                }
                TokenType::Echo => {
                    self.next();

                    return self.echo_statement();
                }
                TokenType::Print => {
                    self.next();

                    return self.print_statement();
                }
                TokenType::Goto => {
                    self.next();

                    return self.goto_statement();
                }
                TokenType::Return => {
                    self.next();

                    return self.return_statement();
                }
                TokenType::Throw => {
                    self.next();

                    return self.throw_statement();
                }
                TokenType::Class => {
                    self.next();
                    return self.class_statement(false, false);
                }
                TokenType::Trait => {
                    self.next();
                    return self.trait_statement();
                }
                TokenType::Abstract => {
                    self.next();
                    return self.abstract_class_statement();
                }
                TokenType::Final => {
                    self.next();
                    return self.final_class_statement();
                }
                TokenType::Interface => {
                    self.next();
                    return self.interface();
                }
                TokenType::While => {
                    self.next();
                    return loops::while_statement(self);
                }
                TokenType::Do => {
                    self.next();
                    return loops::do_while_statement(self);
                }
                TokenType::For => {
                    self.next();
                    return loops::for_statement(self);
                }
                TokenType::Foreach => {
                    self.next();
                    return loops::foreach_statement(self);
                }
                TokenType::If => {
                    self.next();
                    return conditionals::if_statement(self);
                }
                TokenType::OpenCurly => {
                    let block = self.block();

                    return block;
                }
                TokenType::Switch => {
                    self.next();
                    return conditionals::switch_statement(self);
                }
                TokenType::Semicolon => {
                    return Ok(Box::new(TokenStatement::new(
                        self.consume_cloned(TokenType::Semicolon)?,
                        None,
                    )));
                }
                TokenType::Break | TokenType::Continue => {
                    let token = self.next().unwrap();

                    let expr = if self.next_token_one_of(&[TokenType::Semicolon]) {
                        None
                    } else {
                        Some(self.expression()?)
                    };

                    let statement = Box::new(TokenStatement::new(token, expr));

                    self.consume_end_of_statement()?;

                    return Ok(statement);
                }
                TokenType::Try => {
                    self.next();
                    return try_catch::try_catch_statement(self);
                }
                TokenType::Declare => {
                    return self.declare_statement();
                }
                TokenType::Unset => {
                    return self.unset_statement();
                }
                _ => {
                    if let Some(static_token) = self.consume_or_ignore(TokenType::Static) {
                        if self.next_token_one_of(&[TokenType::Variable]) {
                            return self.static_variables();
                        } else {
                            // Back on the stack
                            self.tokens.push(static_token);
                        }

                    // Labels
                    } else if let Some(identifier) = self.consume_or_ignore(TokenType::Identifier) {
                        if let Some(colon) = self.consume_or_ignore(TokenType::Colon) {
                            return Ok(Box::new(LabelStatement::new(identifier, colon)));
                        } else {
                            // Back on the stack
                            self.tokens.push(identifier);
                        }
                    }
                    let expr = self.expression_statement()?;

                    self.consume_end_of_statement()?;

                    return Ok(expr);
                }
            }
        }

        Err(String::from("Unexpected EOF!"))
    }

    fn static_variables(&mut self) -> StatementResult {
        let mut variables = Vec::new();

        loop {
            let variable = self.consume_cloned(TokenType::Variable)?;

            if let Some(assignment) = self.consume_or_ignore(TokenType::Assignment) {
                variables.push(Node::StaticVariable {
                    variable,
                    assignment: Some(assignment),
                    value: Some(Box::new(self.expression()?)),
                });
            }

            if self.consume_or_ignore(TokenType::Comma).is_none() {
                break;
            }
        }

        Ok(Box::new(StaticVariablesStatement::new(variables)))
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
    fn type_ref_union(&mut self) -> Result<Vec<Node>, String> {
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

    /// Parses a single namespace statement
    ///
    /// # Details
    /// ```php
    /// namespace /** from here **/My\Super\Duper\Namespace;/** to here **/
    /// ```
    fn namespace_statement(&mut self) -> StatementResult {
        let type_ref = self.type_ref()?;

        if self.next_token_one_of(&[TokenType::OpenCurly]) {
            Ok(Box::new(NamespaceBlock::new(type_ref, self.block()?)))
        } else if let Some(type_ref) = type_ref {
            self.consume_end_of_statement()?;
            Ok(Box::new(NamespaceStatement::new(type_ref)))
        } else {
            Err("Empty path after namespace".to_owned())
        }
    }

    fn const_statement(&mut self) -> StatementResult {
        let mut constants = Vec::new();

        constants.push(Node::Const {
            name: self.consume_identifier_cloned()?,
            token: self.consume_cloned(TokenType::Assignment)?,
            value: Box::new(self.expression()?),
        });

        while let None = self.consume_or_ignore(TokenType::Semicolon) {
            self.consume_or_err(TokenType::Comma)?;
            constants.push(Node::Const {
                name: self.consume_identifier_cloned()?,
                token: self.consume_cloned(TokenType::Assignment)?,
                value: Box::new(self.expression()?),
            });
        }

        Ok(Box::new(ConstStatement::new(constants)))
    }

    fn symbol_import(&mut self) -> Result<Node, String> {
        if self.consume_or_ignore(TokenType::Function).is_some() {
            let name = self.non_empty_type_ref()?;

            if let Some(alias) = self.consume_or_ignore(TokenType::As) {
                return Ok(Node::UseFunction {
                    function: Box::new(name),
                    aliased: Some(alias),
                    alias: Some(self.consume_cloned(TokenType::Identifier)?),
                });
            } else {
                return Ok(Node::UseFunction {
                    function: Box::new(name),
                    aliased: None,
                    alias: None,
                });
            }
        }

        if self.consume_or_ignore(TokenType::Const).is_some() {
            let name = self.non_empty_type_ref()?;

            if let Some(alias) = self.consume_or_ignore(TokenType::As) {
                return Ok(Node::UseConst {
                    constant: Box::new(name),
                    aliased: Some(alias),
                    alias: Some(self.consume_cloned(TokenType::Identifier)?),
                });
            } else {
                return Ok(Node::UseConst {
                    constant: Box::new(name),
                    aliased: None,
                    alias: None,
                });
            }
        }

        let name = self.non_empty_type_ref()?;

        if let Some(alias) = self.consume_or_ignore(TokenType::As) {
            Ok(Node::UseDeclaration {
                declaration: Box::new(name),
                aliased: Some(alias),
                alias: Some(self.consume_cloned(TokenType::Identifier)?),
            })
        } else {
            Ok(Node::UseDeclaration {
                declaration: Box::new(name),
                aliased: None,
                alias: None,
            })
        }
    }

    fn symbol_imports(&mut self) -> Result<Vec<Node>, String> {
        let mut symbols = Vec::new();

        symbols.push(self.symbol_import()?);

        while self.consume_or_ignore(TokenType::Comma).is_some() {
            if !self.next_token_one_of(&[
                TokenType::Identifier,
                TokenType::Const,
                TokenType::Function,
            ]) {
                break;
            }

            symbols.push(self.symbol_import()?);
        }

        Ok(symbols)
    }

    fn use_function_statement(&mut self) -> StatementResult {
        let mut symbols = Vec::new();

        loop {
            let name = self.non_empty_type_ref()?;

            if let Some(alias) = self.consume_or_ignore(TokenType::As) {
                symbols.push(Node::UseFunction {
                    function: Box::new(name),
                    aliased: Some(alias),
                    alias: Some(self.consume_cloned(TokenType::Identifier)?),
                });
            } else {
                symbols.push(Node::UseFunction {
                    function: Box::new(name),
                    aliased: None,
                    alias: None,
                });
            }

            if self.consume_or_ignore(TokenType::Comma).is_some() {
                continue;
            }

            break;
        }

        self.consume_end_of_statement()?;

        Ok(Box::new(UseFunctionStatement::new(symbols)))
    }

    fn use_const_statement(&mut self) -> StatementResult {
        let mut symbols = Vec::new();

        loop {
            let name = self.non_empty_type_ref()?;

            if let Some(alias) = self.consume_or_ignore(TokenType::As) {
                symbols.push(Node::UseConst {
                    constant: Box::new(name),
                    aliased: Some(alias),
                    alias: Some(self.consume_cloned(TokenType::Identifier)?),
                });
            } else {
                symbols.push(Node::UseConst {
                    constant: Box::new(name),
                    aliased: None,
                    alias: None,
                });
            }

            if self.consume_or_ignore(TokenType::Comma).is_some() {
                continue;
            }

            break;
        }

        self.consume_end_of_statement()?;

        Ok(Box::new(UseConstStatement::new(symbols)))
    }

    // use -> "use" ("function" | "const")? path (("{" use_group "}") | ("as" identifier))?
    fn use_statement(&mut self) -> StatementResult {
        let mut imports = Vec::new();

        loop {
            let declaration = self.non_empty_namespace_ref()?;

            // Ends with \, so it should be followed by a group wrapped in curly braces
            if declaration.last().unwrap().t == TokenType::NamespaceSeparator {
                imports.push(Node::GroupedUse {
                    parent: Box::new(Node::TypeRef(declaration)),
                    oc: self.consume_cloned(TokenType::OpenCurly)?,
                    uses: self.symbol_imports()?,
                    cc: self.consume_cloned(TokenType::CloseCurly)?,
                });
            // Is aliased
            } else if let Some(aliased) = self.consume_or_ignore(TokenType::As) {
                imports.push(Node::UseDeclaration {
                    declaration: Box::new(Node::TypeRef(declaration)),
                    aliased: Some(aliased),
                    alias: Some(self.consume_cloned(TokenType::Identifier)?),
                });
            // Is a regular use
            } else {
                imports.push(Node::UseDeclaration {
                    declaration: Box::new(Node::TypeRef(declaration)),
                    aliased: None,
                    alias: None,
                });
            }

            if self.consume_or_ignore(TokenType::Comma).is_some() {
                continue;
            }

            self.consume_end_of_statement()?;

            break;
        }

        Ok(Box::new(UseStatement::new(imports)))
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

    fn type_ref_list(&mut self) -> Result<Vec<Node>, String> {
        let mut type_refs = Vec::new();

        type_refs.push(self.non_empty_type_ref()?);

        while self.consume_or_ignore(TokenType::Comma).is_some() {
            type_refs.push(self.non_empty_type_ref()?);
        }

        Ok(type_refs)
    }

    fn global_variables(&mut self) -> StatementResult {
        let mut vars = Vec::new();
        vars.push(self.variable()?);

        self.consume_or_ignore(TokenType::Comma);

        while !self.next_token_one_of(&[TokenType::Semicolon]) {
            vars.push(self.variable()?);

            if self.next_token_one_of(&[TokenType::Semicolon]) {
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

    fn print_statement(&mut self) -> StatementResult {
        let mut values = Vec::new();

        values.push(self.expression()?);

        self.consume_end_of_statement()?;

        Ok(Box::new(PrintStatement::new(values)))
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
        if self.next_token_one_of(&[TokenType::Semicolon]) {
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
        self.next();

        self.class_statement(true, false)
    }

    // final_class -> "final" class
    fn final_class_statement(&mut self) -> StatementResult {
        self.next();

        self.class_statement(false, true)
    }

    // class -> "class" identifier (extends identifier_list)? (implements identifier_list)?
    fn class_statement(&mut self, is_abstract: bool, is_final: bool) -> StatementResult {
        let name = self.consume_identifier_cloned()?;

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

        let arguments = if self.next_token_one_of(&[TokenType::OpenParenthesis]) {
            self.consume_or_err(TokenType::OpenParenthesis)?;
            let result = Some(self.parameter_list()?);
            self.consume_or_err(TokenType::CloseParenthesis)?;

            result
        } else {
            None
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

        Ok(Node::Class {
            token,
            arguments,
            extends,
            implements,
            body,
        })
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
    fn identifier_list(&mut self) -> Result<Vec<Node>, String> {
        let mut extends = Vec::new();

        loop {
            extends.push(self.non_empty_type_ref()?);

            if !self.next_token_one_of(&[TokenType::Comma]) {
                break;
            }

            self.next();
        }

        Ok(extends)
    }

    /// Parses declare statements
    fn declare_statement(&mut self) -> StatementResult {
        self.consume_or_err(TokenType::Declare)?;
        self.consume_or_err(TokenType::OpenParenthesis)?;
        let directive = self.consume_cloned(TokenType::Identifier)?;
        self.consume_or_err(TokenType::Assignment)?;
        let val = self.consume_one_of_cloned(&[
            TokenType::False,
            TokenType::True,
            TokenType::Null,
            TokenType::LongNumber,
            TokenType::DecimalNumber,
            TokenType::ExponentialNumber,
            TokenType::HexNumber,
            TokenType::BinaryNumber,
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
    fn type_ref(&mut self) -> Result<Option<Node>, String> {
        let mut path = Vec::new();

        if let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
            path.push(ns);
        }

        while let Some(identifier) = self.peek() {
            if identifier.is_identifier() {
                path.push(self.next().unwrap());

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
            Ok(Some(Node::TypeRef(path)))
        }
    }

    // non_empty_type_ref -> identifier ("\" identifier)*
    fn non_empty_type_ref(&mut self) -> Result<Node, String> {
        let mut path = Vec::new();

        if let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
            path.push(ns);
        }

        path.push(self.consume_identifier_cloned()?);

        while let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
            path.push(ns);
            path.push(self.consume_identifier_cloned()?);
        }

        Ok(Node::TypeRef(path))
    }

    // non_empty_namespace_ref -> "\"? identifier ("\" identifier)* "\"?
    fn non_empty_namespace_ref(&mut self) -> Result<Vec<Token>, String> {
        let mut path = Vec::new();

        if let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
            path.push(ns);
        }

        path.push(self.consume_identifier_cloned()?);

        while let Some(ns) = self.consume_or_ignore(TokenType::NamespaceSeparator) {
            path.push(ns);

            if let Some(ident) = self.peek() {
                if ident.is_identifier() {
                    path.push(self.next().unwrap());

                    continue;
                }
            }

            break;
        }

        Ok(path)
    }

    /// Parses an expression. This can be anything that evaluates to a value. A function call, a comparison or even an assignment
    fn expression(&mut self) -> ExpressionResult {
        let expr = self.logic();

        if let Some(qm) = self.consume_or_ignore(TokenType::QuestionMark) {
            if let Some(colon) = self.consume_or_ignore(TokenType::Colon) {
                return Ok(Node::Ternary {
                    check: Box::new(expr?),
                    qm,
                    true_arm: None,
                    colon,
                    false_arm: Box::new(self.expression()?),
                });
            } else {
                return Ok(Node::Ternary {
                    check: Box::new(expr?),
                    qm,
                    true_arm: Some(Box::new(self.expression()?)),
                    colon: self.consume_cloned(TokenType::Colon)?,
                    false_arm: Box::new(self.expression()?),
                });
            }
        }

        expr
    }

    fn logic(&mut self) -> ExpressionResult {
        let mut expr = self.equality()?;

        let potential_matches = vec![TokenType::LogicOr, TokenType::LogicAnd, TokenType::LogicXor];
        // Todo: Make this an if and force && or || in between. Basically add a new non-terminal
        while self.next_token_one_of(&potential_matches) {
            // Unwrap should be fine since the condition already checks that there is a next element
            let next = self.next().unwrap();
            let right = self.equality()?;

            expr = Node::Binary {
                left: Box::new(expr),
                token: next,
                right: Box::new(right),
            };
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
            let next = self.next().unwrap();
            let right = self.comparison()?;

            expr = Node::Binary {
                left: Box::new(expr),
                token: next,
                right: Box::new(right),
            };
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
            let next = self.next().unwrap();
            let right = self.assignment()?;

            expr = Node::Binary {
                left: Box::new(expr),
                token: next,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    /// Parses an assignment. First parses the l-value as a normal expression. Then determines if it is followed by an assignment
    /// operator which says we are looking at an assignment. Then checks, if the already parsed l-value is something that values
    /// can be assigned to. If it is, parse the r-value as another expression and wrap all up in an `Assignment`-expression. If not,
    /// return an error.
    fn assignment(&mut self) -> ExpressionResult {
        let expr = self.addition()?;

        if self.next_token_one_of(&[
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
            let operator = self.next().unwrap();
            let value = self.assignment()?;

            if expr.is_lvalue() {
                return Ok(Node::Binary {
                    left: Box::new(expr),
                    token: operator,
                    right: Box::new(value),
                });
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
            TokenType::BinaryXor,
            TokenType::Modulo,
        ];

        while self.next_token_one_of(&potential_matches) {
            let next = self.next().unwrap();
            let right = self.multiplication()?;

            expr = Node::Binary {
                left: Box::new(expr),
                token: next,
                right: Box::new(right),
            };
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
            let next = self.next().unwrap();
            let right = self.unary()?;

            expr = Node::Binary {
                left: Box::new(expr),
                token: next.clone(),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ExpressionResult {
        if self.next_token_one_of(&[
            TokenType::Negation,
            TokenType::Minus,
            TokenType::Plus,
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
            let next = self.next().unwrap();
            let right = self.unary()?;

            return Ok(Node::Unary {
                token: next,
                expr: Box::new(right),
            });
        }

        let primary = self.call()?;

        if self.next_token_one_of(&[TokenType::Increment, TokenType::Decrement]) {
            let next = self.next().unwrap();

            return Ok(Node::PostUnary {
                token: next,
                expr: Box::new(primary),
            });
        }
        Ok(primary)
    }

    /// Parses class-member access and array access. This also includes non-method call member access!
    fn call(&mut self) -> ExpressionResult {
        let expr = self.primary()?;

        self.init_call(expr)
    }

    fn init_call(&mut self, mut expr: Node) -> ExpressionResult {
        loop {
            if self.next_token_one_of(&[TokenType::OpenParenthesis]) {
                expr = self.finish_call(expr)?;

            // Accessing an object member
            } else if let Some(os) = self.consume_or_ignore(TokenType::ObjectOperator) {
                // Using the ->{} syntax, so the member is the result of an expression
                if self.consume_or_ignore(TokenType::OpenCurly).is_some() {
                    expr = Node::Member {
                        object: Box::new(expr),
                        arrow: os,
                        member: Box::new(self.expression()?),
                    };
                    self.consume_or_err(TokenType::CloseCurly)?;

                // Using the ->member syntax, so the member is either an identifier or a variable
                } else if self.next_token_one_of(&[TokenType::Variable]) {
                    expr = Node::Member {
                        object: Box::new(expr),
                        arrow: os,
                        member: Box::new(self.variable()?),
                    };
                } else {
                    expr = Node::Member {
                        object: Box::new(expr),
                        arrow: os,
                        member: Box::new(Node::Literal(self.consume_identifier_cloned()?)),
                    };
                }
            // Accessing a static class member
            } else if let Some(pn) = self.consume_or_ignore(TokenType::PaamayimNekudayim) {
                // Class property
                if self.next_token_one_of(&[TokenType::Variable]) {
                    expr = Node::StaticMember {
                        class: Box::new(expr),
                        pn,
                        member: Box::new(self.variable()?),
                    };

                // Class method
                } else if self.next_token_one_of(&[TokenType::OpenCurly]) {
                    expr = Node::StaticMethod {
                        class: Box::new(expr),
                        pn,
                        oc: self.consume_cloned(TokenType::OpenCurly)?,
                        method: Box::new(self.variable()?),
                        cc: self.consume_cloned(TokenType::CloseCurly)?,
                    };
                // Class constant
                } else {
                    expr = Node::StaticMember {
                        class: Box::new(expr),
                        pn,
                        member: Box::new(Node::Literal(self.consume_member_cloned()?)),
                    };
                }
            } else if let Some(ob) = self.consume_or_ignore(TokenType::OpenBrackets) {
                // TODO: Think about a nicer solution for $a[] = ...
                expr = match self.consume_or_ignore(TokenType::CloseBrackets) {
                    Some(cb) => Node::Field {
                        array: Box::new(expr),
                        ob,
                        index: None,
                        cb,
                    },
                    None => Node::Field {
                        array: Box::new(expr),
                        ob,
                        index: Some(Box::new(self.expression()?)),
                        cb: self.consume_cloned(TokenType::CloseBrackets)?,
                    },
                };
            } else if let Some(oc) = self.consume_or_ignore(TokenType::OpenCurly) {
                expr = Node::Field {
                    array: Box::new(expr),
                    ob: oc,
                    index: Some(Box::new(self.expression()?)),
                    cb: self.consume_cloned(TokenType::CloseCurly)?,
                };
            } else if let Some(assignment) = self.consume_or_ignore(TokenType::Assignment) {
                // Something like static::$member = 1;
                expr = Node::Binary {
                    left: Box::new(expr),
                    token: assignment,
                    right: Box::new(self.expression()?),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parses all the parameters of a call
    fn finish_call(&mut self, expr: Node) -> ExpressionResult {
        let op = self.consume_cloned(TokenType::OpenParenthesis)?;

        let mut parameters = Vec::new();
        while !self.next_token_one_of(&[TokenType::CloseParenthesis]) {
            parameters.push(self.expression()?);

            if self.next_token_one_of(&[TokenType::CloseParenthesis]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        Ok(Node::Call {
            callee: Box::new(expr),
            op,
            parameters,
            cp: self.consume_cloned(TokenType::CloseParenthesis)?,
        })
    }

    /// Parses all the parameters of a call
    fn non_empty_parameter_list(&mut self) -> Result<Vec<Node>, String> {
        let mut arguments = Vec::new();
        arguments.push(self.expression()?);

        self.consume_or_ignore(TokenType::Comma);

        while !self.next_token_one_of(&[TokenType::CloseParenthesis]) {
            arguments.push(self.expression()?);

            if self.next_token_one_of(&[TokenType::CloseParenthesis]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        Ok(arguments)
    }

    /// Parses all the arguments of a call
    /// Does not parse the surounding parenthesis so the caller can fetch and store them
    fn parameter_list(&mut self) -> Result<Vec<Node>, String> {
        let mut arguments = Vec::new();
        while !self.next_token_one_of(&[TokenType::CloseParenthesis]) {
            arguments.push(self.expression()?);

            if self.next_token_one_of(&[TokenType::CloseParenthesis]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        Ok(arguments)
    }

    /// Parses all the arguments of a call
    fn non_empty_lexical_variables_list(&mut self) -> Result<Vec<Node>, String> {
        self.consume_or_err(TokenType::OpenParenthesis)?;

        let mut arguments = Vec::new();
        arguments.push(self.lexical_variable()?);

        self.consume_or_ignore(TokenType::Comma);

        while !self.next_token_one_of(&[TokenType::CloseParenthesis]) {
            arguments.push(self.lexical_variable()?);

            if self.next_token_one_of(&[TokenType::CloseParenthesis]) {
                break;
            } else {
                self.consume_or_err(TokenType::Comma)?;
            }
        }

        self.consume_or_err(TokenType::CloseParenthesis)?;

        Ok(arguments)
    }

    /// Parses a lexical variable, used in a "use ()" list for example
    fn lexical_variable(&mut self) -> Result<Node, String> {
        Ok(Node::LexicalVariable {
            reference: self.consume_or_ignore(TokenType::BinaryAnd),
            variable: self.consume_cloned(TokenType::Variable)?,
        })
    }

    fn primary(&mut self) -> ExpressionResult {
        if self.next_token_one_of(&[
            TokenType::False,
            TokenType::True,
            TokenType::Null,
            TokenType::LongNumber,
            TokenType::DecimalNumber,
            TokenType::ExponentialNumber,
            TokenType::HexNumber,
            TokenType::BinaryNumber,
            TokenType::ConstantEncapsedString,
            TokenType::EncapsedAndWhitespaceString,
            TokenType::ShellEscape,
            TokenType::ConstDir,
            TokenType::ConstFile,
            TokenType::ConstFunction,
            TokenType::ConstLine,
            TokenType::ConstMethod,
            TokenType::ConstTrait,
            TokenType::ConstClass,
            TokenType::ConstNan,
            TokenType::ConstInf,
        ]) {
            return Ok(Node::Literal(self.next().unwrap()));
        }

        if self.next_token_one_of(&[TokenType::Variable]) {
            return self.variable();
        }

        if self.next_token_one_of(&[TokenType::NamespaceSeparator, TokenType::Identifier]) {
            // Load path as identifier
            return self.non_empty_type_ref();
        }

        if self.next_token_one_of(&[TokenType::HereDocStart]) {
            self.consume_or_err(TokenType::HereDocStart)?;
            let string = self.next().unwrap();
            self.consume_or_err(TokenType::HereDocEnd)?;

            return Ok(Node::Literal(string));
        }

        if let Some(isset) = self.consume_or_ignore(TokenType::Isset) {
            return Ok(Node::Isset {
                isset,
                op: self.consume_cloned(TokenType::OpenParenthesis)?,
                parameters: self.non_empty_parameter_list()?,
                cp: self.consume_cloned(TokenType::CloseParenthesis)?,
            });
        }

        if let Some(exit) = self.consume_or_ignore(TokenType::Exit) {
            if let Some(op) = self.consume_or_ignore(TokenType::OpenParenthesis) {
                return Ok(Node::Exit {
                    exit,
                    op: Some(op),
                    parameters: Some(self.parameter_list()?),
                    cp: Some(self.consume_cloned(TokenType::CloseParenthesis)?),
                });
            }

            return Ok(Node::Exit {
                exit,
                op: None,
                parameters: None,
                cp: None,
            });
        }

        if let Some(empty) = self.consume_or_ignore(TokenType::Empty) {
            return Ok(Node::Empty {
                empty,
                op: self.consume_cloned(TokenType::OpenParenthesis)?,
                parameters: self.non_empty_parameter_list()?,
                cp: self.consume_cloned(TokenType::CloseParenthesis)?,
            });
        }

        if self.next_token_one_of(&[TokenType::TypeArray]) {
            return self.old_array();
        }

        if self.next_token_one_of(&[TokenType::List]) {
            return self.list();
        }

        if let Some(include) = self.consume_one_of_cloned_or_ignore(&[
            TokenType::Require,
            TokenType::RequireOnce,
            TokenType::Include,
            TokenType::IncludeOnce,
        ]) {
            return Ok(Node::FileInclude {
                token: include,
                resource: Box::new(self.expression()?),
            });
        }

        if self.next_token_one_of(&[TokenType::OpenBrackets]) {
            let expr = self.array()?;

            return Ok(expr);
        }

        if self.next_token_one_of(&[TokenType::OpenParenthesis]) {
            self.consume_or_err(TokenType::OpenParenthesis)?;
            let expr = self.expression()?;
            self.consume_or_err(TokenType::CloseParenthesis)?;

            return Ok(Node::Grouping(Box::new(expr)));
        }

        if self.next_token_one_of(&[TokenType::Function]) {
            return self.anonymous_function(None);

            // Can be either "static function ..." or "static::$lol"
        }

        // Static is fun ... watch this ...
        if let Some(static_token) = self.consume_or_ignore(TokenType::Static) {
            // Followed by ::? Probably a member access
            if self.next_token_one_of(&[TokenType::PaamayimNekudayim]) {
                return Ok(Node::Literal(static_token));
            }

            // Followed by "function"? Static function expression
            if self.next_token_one_of(&[TokenType::Function]) {
                return self.anonymous_function(Some(static_token));
            }

            // Otherwise probably used in a instantiation context
            return Ok(Node::Literal(static_token));
        }

        // Self is like static but less mighty
        if let Some(self_token) = self.consume_or_ignore(TokenType::TypeSelf) {
            // Followed by ::? Probably a member access
            if self.next_token_one_of(&[TokenType::PaamayimNekudayim]) {
                return Ok(Node::Literal(self_token));
            }

            // Otherwise ... no clue if an error after all. Need to check official grammar
            return Ok(Node::Literal(self_token));
        }

        if self.next_token_one_of(&[TokenType::Class]) {
            return self.anonymous_class();
        }

        if let Some(new) = self.consume_or_ignore(TokenType::New) {
            return Ok(Node::New {
                token: new,
                class: Box::new(self.call()?),
            });
        }

        if let Some(token) = self.consume_or_ignore(TokenType::Yield) {
            if self.next_token_one_of(&[TokenType::Semicolon]) {
                return Ok(Node::Yield { token, expr: None });
            }

            return Ok(Node::Yield {
                token,
                expr: Some(Box::new(self.array_pair()?)),
            });
        }

        if let Some(from) = self.consume_or_ignore(TokenType::YieldFrom) {
            return Ok(Node::YieldFrom {
                token: from,
                expr: Box::new(self.expression()?),
            });
        }

        let next = self.next().unwrap();

        // Maybe some sort of other identifier?
        if next.is_identifier() {
            return Ok(Node::Literal(next));
        }

        Err(format!("Unsupported primary {:?}", next))
    }

    fn variable(&mut self) -> Result<Node, String> {
        let variable = self.consume_cloned(TokenType::Variable)?;

        // Named, regular variable. No problem here.
        if variable.label.is_some() {
            return Ok(Node::Literal(variable));
        }

        // Dynamic member ${expr}
        if let Some(oc) = self.consume_or_ignore(TokenType::OpenCurly) {
            return Ok(Node::DynamicVariable {
                variable,
                oc,
                expr: Box::new(self.expression()?),
                cc: self.consume_cloned(TokenType::CloseCurly)?,
            });
        }

        // Aliased variable $$$$a
        Ok(Node::AliasedVariable {
            variable,
            expr: Box::new(self.primary()?),
        })
    }

    fn anonymous_function(&mut self, is_static: Option<Token>) -> ExpressionResult {
        let token = self.consume_cloned(TokenType::Function)?;

        self.consume_or_err(TokenType::OpenParenthesis)?;
        let arguments = self.argument_list()?;
        self.consume_or_err(TokenType::CloseParenthesis)?;

        let uses = if self.next_token_one_of(&[TokenType::Use]) {
            self.next();
            Some(self.non_empty_lexical_variables_list()?)
        } else {
            None
        };

        let return_type = self.return_type()?;

        let body = self.block()?;

        Ok(Node::Function {
            is_static,
            token,
            arguments,
            return_type: Box::new(return_type),
            uses,
            body,
        })
    }

    fn array(&mut self) -> ExpressionResult {
        let start = self.consume_cloned(TokenType::OpenBrackets)?;
        let mut elements = Vec::new();

        while !self.next_token_one_of(&[TokenType::CloseBrackets]) {
            // TODO: This is only allowed in a destructuring context. Probably need to split
            // this
            if self.consume_or_ignore(TokenType::Comma).is_some() {
                continue;
            }

            elements.push(self.array_pair()?);

            self.consume_or_ignore(TokenType::Comma);
        }

        Ok(Node::Array {
            ob: start,
            elements,
            cb: self.consume_cloned(TokenType::CloseBrackets)?,
        })
    }

    fn old_array(&mut self) -> ExpressionResult {
        let start = self.consume_cloned(TokenType::TypeArray)?;
        let op = self.consume_cloned(TokenType::OpenParenthesis)?;
        let mut elements = Vec::new();

        while !self.next_token_one_of(&[TokenType::CloseParenthesis]) {
            elements.push(self.array_pair()?);

            self.consume_or_ignore(TokenType::Comma);
        }

        Ok(Node::OldArray {
            token: start,
            op,
            elements,
            cp: self.consume_cloned(TokenType::CloseParenthesis)?,
        })
    }

    /// Parses the list destructuring operation
    fn list(&mut self) -> ExpressionResult {
        let start = self.consume_cloned(TokenType::List)?;
        let op = self.consume_cloned(TokenType::OpenParenthesis)?;
        let mut elements = Vec::new();

        while !self.next_token_one_of(&[TokenType::CloseParenthesis]) {
            // Empty element ... list(,,,$a)
            if self.consume_or_ignore(TokenType::Comma).is_some() {
                continue;
            }

            elements.push(self.array_pair()?);

            self.consume_or_ignore(TokenType::Comma);
        }

        Ok(Node::List {
            token: start,
            op,
            elements,
            cp: self.consume_cloned(TokenType::CloseParenthesis)?,
        })
    }

    fn array_pair(&mut self) -> ExpressionResult {
        // At this point key might as well be the value
        let key = Box::new(self.expression()?);

        if let Some(arrow) = self.consume_or_ignore(TokenType::DoubleArrow) {
            // TODO: Raise warning if key is access by reference ... this no works

            // Todo: Rather check for scalarity
            if !key.is_offset() {
                return Err(format!(
                    "Illegal offset type at line {} col {}",
                    arrow.line, arrow.col,
                ));
            }

            if self.next_token_one_of(&[TokenType::BinaryAnd]) {
                Ok(Node::ArrayElement {
                    key: Some(key),
                    arrow: Some(arrow),
                    value: Box::new(self.lexical_variable()?),
                })
            } else {
                Ok(Node::ArrayElement {
                    key: Some(key),
                    arrow: Some(arrow),
                    value: Box::new(self.expression()?),
                })
            }
        } else {
            Ok(Node::ArrayElement {
                key: None,
                arrow: None,
                value: key,
            })
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.last()
    }

    fn consume_end_of_statement(&mut self) -> Result<(), String> {
        if let Some(token) = self.peek() {
            if token.t == TokenType::Semicolon || token.t == TokenType::ScriptEnd {
                self.next();
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
        if let Some(token) = self.peek() {
            if token.t == t {
                self.next();
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
        if let Some(token) = self.peek() {
            if token.t != t {
                return None;
            }
        } else {
            return None;
        }

        Some(self.next().unwrap())
    }

    fn consume_cloned(&mut self, t: TokenType) -> Result<Token, String> {
        if let Some(token) = self.next() {
            if token.t == t {
                return Ok(token);
            }

            return Err(format!(
                "Expected {:?}, found {:?} on line {}, col {}",
                t, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected {:?}, found end of file.", t))
    }

    fn consume_identifier_cloned(&mut self) -> Result<Token, String> {
        if let Some(token) = self.next() {
            if token.is_identifier() {
                return Ok(token);
            }

            return Err(format!(
                "Expected Identifier, found {:?} on line {}, col {}",
                token.t, token.line, token.col
            ));
        }

        Err("Expected Identifier, found end of file.".to_string())
    }

    fn consume_member_cloned(&mut self) -> Result<Token, String> {
        if let Some(token) = self.next() {
            if token.is_identifier() || token.t == TokenType::Variable {
                return Ok(token);
            }

            return Err(format!(
                "Expected Identifier or Variable, found {:?} on line {}, col {}",
                token.t, token.line, token.col
            ));
        }

        Err("Expected Identifier, found end of file.".to_string())
    }

    fn consume_one_of_cloned(&mut self, types: &[TokenType]) -> Result<Token, String> {
        if let Some(token) = self.next() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Ok(token);
                }
            }

            return Err(format!(
                "Expected one of {:?}, found {:?} on line {}, col {}",
                types, token.t, token.line, token.col
            ));
        }

        Err(format!("Expected one of {:?}, found end of file.", types))
    }

    fn consume_one_of_cloned_or_ignore(&mut self, types: &[TokenType]) -> Option<Token> {
        if let Some(token) = self.peek() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Some(self.next().unwrap());
                }
            }
        }

        None
    }

    fn next_token_one_of(&mut self, types: &[TokenType]) -> bool {
        if let Some(token) = self.peek() {
            for tt in types {
                if *tt == token.t {
                    return true;
                }
            }
        }

        false
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

        println!("{:?}", Parser::ast(scanner.tokens));
    }
}
