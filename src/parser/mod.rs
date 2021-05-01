use self::token::ScriptStartType;
use crate::parser::ast::*;
use node::Node;
use snafu::Snafu;
use token::{Token, TokenType};

#[derive(Debug, Snafu, PartialEq)]
pub enum Error {
    #[snafu(display("Unexpected token {:?}, expected on of [{:?}] on line {}, col {}", token.t, expected, token.line, token.col))]
    WrongTokenError {
        expected: Vec<TokenType>,
        token: Token,
    },

    #[snafu(display("Missing identifier on line {}, col {}", token.line, token.col))]
    MissingIdentifier { token: Token },

    #[snafu(display("Unexpected token {:?} on line {}, col {}", token.t, token.line, token.col))]
    UnexpectedTokenError { token: Token },

    #[snafu(display("Illegal offset type on line {}, col {}", (expr.range().0).0, (expr.range().0).1))]
    IllegalOffsetType { expr: Node },

    #[snafu(display("Can not use expression in write context on line {}, col {}", token.line, token.col))]
    RValueInWriteContext { token: Token },

    #[snafu(display("Unexpected end of file"))]
    Eof,
}

// Overwrite result
type Result<T, E = Error> = std::result::Result<T, E>;
type ArgumentListResult = Result<Option<Vec<Node>>>;
type ExpressionResult = Result<Node>;
type ExpressionListResult = Result<Vec<Node>>;
pub type AstResult = Result<(Vec<Node>, Vec<Error>)>;

/// Representation of the Abstract Syntax Tree used by the parser
pub mod ast;

/// The node type
pub mod node;

/// The scanner used to generate tokens from source files
pub mod scanner;

/// The token type
pub mod token;
#[derive(Debug)]
pub enum Context {
    Out,
    Script,
    Echo,
}

/// Inspired by <https://craftinginterpreters.com/statements-and-state.html>
///
/// Parses a token stream of a `Scanner` and generates an Abstract Syntax Tree
#[derive(Debug)]
pub struct Parser {
    doc_comments: Vec<Token>,
    tokens: Vec<Token>,
    errors: Vec<Error>,
    context: Context,
    // Line and col of end of file
    eof: (u32, u32),

    // End of the previously read token
    end_of_prev_token: (u32, u32),
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        tokens.reverse();

        let eof = if let Some(eof) = tokens.first() {
            (eof.line, eof.col)
        } else {
            (0, 0)
        };

        Parser {
            doc_comments: Vec::new(),
            tokens,
            errors: Vec::new(),
            context: Context::Out,
            eof,
            end_of_prev_token: (0, 0),
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
    pub fn ast(tokens: Vec<Token>) -> AstResult {
        if tokens.is_empty() {
            return Ok((Vec::new(), Vec::new()));
        }
        let mut parser = Parser::new(tokens);

        let mut statements: Vec<Node> = Vec::new();

        while let Some(next) = parser.peek() {
            if next.t == TokenType::Eof {
                break;
            }

            let new_statement = match parser.statement() {
                Ok(statement) => statement,
                Err(error) => {
                    parser.errors.push(error);
                    parser.error_fast_forward();

                    continue;
                }
            };

            while !parser.doc_comments.is_empty() {
                if let Some(doc_comment) = comments::consume_optional_doc_comment(&mut parser) {
                    if let Node::DocComment { var_docs, .. } = doc_comment.as_ref() {
                        for var_doc in var_docs {
                            statements.push(var_doc.clone());
                        }
                    }
                }
            }

            statements.push(new_statement);
        }

        Ok((statements, parser.errors))
    }

    /// Fast forwards to the end of the current statement or block instead of simply aborting parsing.
    /// This way at least a partial ast can be returned in the end
    fn error_fast_forward(&mut self) {
        while self.peek().is_some() {
            if self.next_token_one_of(&[
                TokenType::Semicolon,
                TokenType::ScriptStart(ScriptStartType::Regular),
                TokenType::ScriptStart(ScriptStartType::Echo),
                TokenType::ScriptStart(ScriptStartType::Short),
                TokenType::CloseCurly,
            ]) {
                break;
            }

            self.next();
        }
    }

    /// Parses a code block, which basically is a vector of `Node` / statements.
    ///
    /// # Details
    /// ```php
    /// while (true) /* from here */{
    ///    echo "lol";
    /// } /* to here */
    /// ```
    pub fn block(&mut self) -> ExpressionResult {
        let mut statements: Vec<Node> = Vec::new();

        let oc = self.consume(TokenType::OpenCurly)?;

        while !self.next_token_one_of(&[TokenType::CloseCurly]) && self.peek().is_some() {
            let new_statement = match self.statement() {
                Ok(statement) => statement,
                Err(error) => {
                    self.errors.push(error);
                    self.error_fast_forward();

                    continue;
                }
            };

            while !self.doc_comments.is_empty() {
                if let Some(doc_comment) = comments::consume_optional_doc_comment(self) {
                    if let Node::DocComment { var_docs, .. } = doc_comment.as_ref() {
                        for var_doc in var_docs {
                            statements.push(var_doc.clone());
                        }
                    }
                }
            }

            statements.push(new_statement);
        }

        let cc = self.consume(TokenType::CloseCurly)?;

        Ok(Node::Block { oc, statements, cc })
    }

    /// Parses an alternative code block, which basically is a vector of `Node` / statements.
    ///
    /// # Details
    /// ```php
    /// while (true) /* from here */:
    ///    echo "lol";
    /// endwhile /* to here */
    /// ```
    pub fn alternative_block(&mut self, expected_terminator: TokenType) -> ExpressionResult {
        let mut statements: Vec<Node> = Vec::new();

        let colon = self.consume(TokenType::Colon)?;

        // TODO: Make sure namespace etc can not pop up here
        while !self.next_token_one_of(&[
            TokenType::EndFor,
            TokenType::EndForeach,
            TokenType::EndWhile,
            TokenType::EndIf,
        ]) && self.peek().is_some()
        {
            let new_statement = match self.statement() {
                Ok(statement) => statement,
                Err(error) => {
                    self.errors.push(error);
                    self.error_fast_forward();

                    continue;
                }
            };

            while !self.doc_comments.is_empty() {
                if let Some(doc_comment) = comments::consume_optional_doc_comment(self) {
                    if let Node::DocComment { var_docs, .. } = doc_comment.as_ref() {
                        for var_doc in var_docs {
                            statements.push(var_doc.clone());
                        }
                    }
                }
            }

            statements.push(new_statement);
        }

        let terminator = self.consume(expected_terminator)?;

        Ok(Node::AlternativeBlock {
            colon,
            statements,
            terminator,
        })
    }

    /// Parses sudden appearances of inline HTML.
    ///
    /// # Example
    /// ```php
    /// echo "This is PHP";
    /// ?><h1>HTML</h1><?php
    /// echo "This is PHP ... again!";
    /// ```
    fn inline_html(&mut self) -> ExpressionResult {
        let start = self.consume(TokenType::ScriptEnd)?;
        // If the next token is a <?= we consider this a short-tag-echo-statement (nice name, huh?)
        if self.next_token_one_of(&[TokenType::ScriptStart(ScriptStartType::Echo)]) {
            let expr = keywords::short_tag_echo_statement(self)?;

            // The previous call extracts everything including the closing ?> token. Now,
            // if the next token is a regular opening tag (or a short opening tag) we already
            // consume it to not be in a limbo state where the parser would be out of script-mode
            // but, semantically, could still be withing a block for example. This is to handle
            // cases like this:
            // foreach ($elements as $e) { ?>
            //    <?= echo $e; ?>
            // <?php }   // <-- this
            if self.next_token_one_of(&[
                TokenType::ScriptStart(ScriptStartType::Regular),
                TokenType::ScriptStart(ScriptStartType::Short),
            ]) {
                self.context = Context::Script;
                self.next();
            // Otherwise we remain in Out mode. The next token might be another <?=
            } else {
                self.context = Context::Out;
            }
            return Ok(expr);
        }

        // If this is just a regular inline html block we fast-forward to the end of it.
        // TODO: Collect its content as well, so we can restore it on format
        let end = self.consume_one_of_or_ignore(&[
            TokenType::ScriptStart(ScriptStartType::Regular),
            TokenType::ScriptStart(ScriptStartType::Short),
        ]);

        self.context = Context::Script;
        Ok(Node::InlineHtml { start, end })
    }

    /// A helper for wrapping the switch between regular statement or alternative block
    fn alternative_block_or_statement(&mut self, terminator: TokenType) -> ExpressionResult {
        if self.next_token_one_of(&[TokenType::Colon]) {
            return self.alternative_block(terminator);
        }

        self.statement()
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
    fn statement(&mut self) -> ExpressionResult {
        // If we are currently not in PHP Land check if the next token starts the show
        // otherwise error
        if let Context::Out = self.context {
            if let Some(token) = self.peek() {
                match token.t {
                    TokenType::ScriptStart(ScriptStartType::Regular)
                    | TokenType::ScriptStart(ScriptStartType::Short) => {
                        self.context = Context::Script;
                        self.next();
                    }
                    TokenType::ScriptStart(ScriptStartType::Echo) => {
                        // TODO: Integrate this part into inline_html pretty pls
                        let expr = keywords::short_tag_echo_statement(self);

                        if self.next_token_one_of(&[
                            TokenType::ScriptStart(ScriptStartType::Regular),
                            TokenType::ScriptStart(ScriptStartType::Short),
                        ]) {
                            self.context = Context::Script;
                            self.next();
                        }

                        return expr;
                    }
                    _ => {
                        return Err(Error::UnexpectedTokenError {
                            token: token.clone(),
                        });
                    }
                }
            }
        }

        let attributes = attributes::attributes_block(self)?;

        if let Some(token) = self.peek() {
            match token.t {
                // If we are currently in PHP land, check if the next token ends the show
                TokenType::ScriptEnd => {
                    return self.inline_html();
                }
                TokenType::Function => {
                    let comment = comments::consume_optional_doc_comment(self);
                    /*let comment = match comments::consume_optional_doc_comment(self) {
                        Ok(comment) => comment,
                        Err(e) => panic!("{:?}", e),
                    };*/
                    return functions::named_function(self, &comment, attributes);
                }
                TokenType::Namespace => return namespaces::namespace_statement(self),
                TokenType::Use => {
                    let token = self.consume(TokenType::Use)?;

                    if self.consume_or_ignore(TokenType::Function).is_some() {
                        return namespaces::use_function_statement(self, token);
                    }

                    if self.consume_or_ignore(TokenType::Const).is_some() {
                        return namespaces::use_const_statement(self, token);
                    }

                    return namespaces::use_statement(self, token);
                }
                TokenType::Const => return variables::const_statement(self),
                TokenType::Global => return variables::global_variables(self),
                TokenType::Echo => return keywords::echo_statement(self),
                TokenType::Print => return keywords::print_statement(self),
                TokenType::Goto => return keywords::goto_statement(self),
                TokenType::Return => return functions::return_statement(self),
                TokenType::Throw => return exception_handling::throw_statement(self),
                TokenType::Class => return classes::class_statement(self, None, None, attributes),
                TokenType::Trait => return classes::trait_statement(self),
                TokenType::Abstract => return classes::abstract_class_statement(self, attributes),
                TokenType::Final => return classes::final_class_statement(self, attributes),
                TokenType::Interface => return classes::interface(self),
                TokenType::While => return loops::while_statement(self),
                TokenType::Do => return loops::do_while_statement(self),
                TokenType::For => return loops::for_statement(self),
                TokenType::Foreach => return loops::foreach_statement(self),
                TokenType::If => return conditionals::if_statement(self),
                TokenType::OpenCurly => return self.block(),
                TokenType::Switch => return conditionals::switch_statement(self),
                //TokenType::HaltCompiler | TokenType::Die => {
                // Exit
                //}
                TokenType::Semicolon => {
                    return Ok(Node::TokenStatement {
                        token: self.consume(TokenType::Semicolon)?,
                        expr: None,
                    });
                }
                TokenType::Break | TokenType::Continue => {
                    let token = self.next().unwrap();

                    let expr = if self.next_token_one_of(&[TokenType::Semicolon]) {
                        None
                    } else {
                        Some(Box::new(expressions::expression(self, 0)?))
                    };

                    let statement = Node::TokenStatement { token, expr };

                    self.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;

                    return Ok(statement);
                }
                TokenType::Try => return exception_handling::try_catch_statement(self),
                TokenType::Declare => return keywords::declare_statement(self),
                TokenType::Unset => return keywords::unset_statement(self),
                TokenType::Die => return keywords::die_statement(self),
                TokenType::Define => return keywords::define_statement(self),
                _ => {
                    if let Some(static_token) = self.consume_or_ignore(TokenType::Static) {
                        if self.next_token_one_of(&[TokenType::Variable]) {
                            return variables::static_variables(self, static_token);
                        } else {
                            // Back on the stack
                            self.tokens.push(static_token);
                        }

                    // Labels
                    } else if let Some(label) = self.consume_or_ignore(TokenType::Identifier) {
                        if let Some(colon) = self.consume_or_ignore(TokenType::Colon) {
                            return Ok(Node::LabelStatement { label, colon });
                        } else {
                            // Back on the stack
                            self.tokens.push(label);
                        }
                    }
                    let expr = expressions::expression_statement(self)?;

                    if self.next_token_one_of(&[TokenType::ScriptEnd]) {
                        self.consume(TokenType::ScriptEnd)?;
                        self.context = Context::Out;
                    } else {
                        self.consume_or_ff_after(TokenType::Semicolon, &[TokenType::Semicolon])?;
                    }

                    return Ok(expr);
                }
            }
        }

        Err(Error::Eof)
    }

    /// Pop and return the next token, pushing doc comments on the comment stack
    fn next(&mut self) -> Option<Token> {
        while let Some(next) = self.tokens.pop() {
            self.end_of_prev_token = next.range().1;

            match next.t {
                TokenType::MultilineComment => {
                    self.doc_comments.push(next);
                }
                _ => return Some(next),
            }
        }

        None
    }

    /// Return the next token without popping it off the stream, pushing doc comments on the comment stack
    fn peek(&mut self) -> Option<&Token> {
        while let Some(last) = self.tokens.pop() {
            if last.t == TokenType::Eof {
                return None;
            } else if last.t == TokenType::MultilineComment {
                self.doc_comments.push(last);
            } else {
                self.tokens.push(last);

                return self.tokens.last();
            }
        }

        None
    }

    /// Consumes the end of a statement. This can either be a semicolon or a script end.
    fn consume_end_of_statement(&mut self) -> Result<()> {
        let wrong_token = if let Some(token) = self.peek() {
            if token.t == TokenType::Semicolon {
                self.next();
                return Ok(());
            }

            // Implicit end of statement. Return Ok but do not consume as we need to consume it later to record
            // a proper inline html statement
            if token.t == TokenType::ScriptEnd {
                return Ok(());
            }

            token.clone()
        } else {
            self.errors.push(Error::Eof);
            return Ok(());
        };

        self.error_fast_forward();

        self.errors.push(Error::WrongTokenError {
            expected: vec![TokenType::Semicolon, TokenType::ScriptEnd],
            token: wrong_token,
        });
        Ok(())
    }

    /// Consume a token of type `t` or return an Err
    fn consume_or_ff_after(&mut self, t: TokenType, ff_to: &[TokenType]) -> Result<()> {
        let bad_token = if let Some(token) = self.peek() {
            if token.t == t {
                self.next();
                return Ok(());
            }

            token.clone()
        } else {
            self.errors.push(Error::Eof);
            return Ok(());
        };

        self.errors.push(Error::WrongTokenError {
            expected: vec![t],
            token: bad_token.clone(),
        });

        'outer: while let Some(next) = self.next() {
            for tt in ff_to {
                if *tt == next.t {
                    break 'outer;
                }
            }
        }

        Ok(())
    }

    fn consume_or_ff_before(&mut self, t: TokenType, ff_to: &[TokenType]) -> Result<Token> {
        let bad_token = if let Some(token) = self.peek() {
            if token.t == t {
                return Ok(self.next().unwrap());
            }

            token.clone()
        } else {
            self.errors.push(Error::Eof);
            return Err(Error::Eof);
        };

        self.errors.push(Error::WrongTokenError {
            expected: vec![t],
            token: bad_token.clone(),
        });

        'outer: while let Some(next) = self.peek() {
            for tt in ff_to {
                if *tt == next.t {
                    break 'outer;
                }
            }
            self.next();
        }

        Ok(bad_token)
    }

    /// Consume a Some of token of type `t` or do nothing.
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

    /// Consume a token of type `t` or return an error
    fn consume(&mut self, t: TokenType) -> Result<Token> {
        let (token, is_wrong_token) = if let Some(token) = self.peek() {
            if token.t == t {
                (self.next().unwrap(), false)
            } else {
                (Token::missing(self.end_of_prev_token), true)
            }
        } else {
            (Token::missing(self.eof), true)
        };

        if is_wrong_token {
            self.error_fast_forward();
            self.errors.push(Error::WrongTokenError {
                expected: vec![t],
                token: token.clone(),
            });
        }

        Ok(token)
    }

    /// Consume an identifier or return an error
    fn consume_identifier(&mut self) -> Result<Token> {
        let (token, is_wrong_token) = if let Some(token) = self.peek() {
            if token.is_identifier() {
                (self.next().unwrap(), false)
            } else {
                (Token::missing(self.end_of_prev_token), true)
            }
        } else {
            (Token::missing(self.eof), true)
        };
        if is_wrong_token {
            self.errors.push(Error::MissingIdentifier {
                token: token.clone(),
            });
        }

        Ok(token)
    }

    /// Consume a potential member of a class / an object or return an error
    fn consume_member(&mut self) -> Result<Token> {
        if let Some(token) = self.next() {
            if token.is_identifier() || token.t == TokenType::Variable {
                return Ok(token);
            }

            return Err(Error::WrongTokenError {
                expected: vec![TokenType::Identifier, TokenType::Variable],
                token,
            });
        }

        Err(Error::Eof)
    }

    fn next_is_member(&mut self) -> bool {
        if let Some(token) = self.peek() {
            return token.is_identifier() || token.t == TokenType::Variable;
        }

        false
    }

    fn next_is_identifier(&mut self) -> bool {
        if let Some(token) = self.peek() {
            return token.is_identifier();
        }

        false
    }

    // Consume a token of one of the types of `types` or return an error
    fn consume_one_of(&mut self, types: &[TokenType]) -> Result<Token> {
        if let Some(token) = self.next() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Ok(token);
                }
            }

            return Err(Error::WrongTokenError {
                expected: Vec::from(types),
                token,
            });
        }

        Err(Error::Eof)
    }

    /// Consume a token of one of the types of `types` or do nothing
    fn consume_one_of_or_ignore(&mut self, types: &[TokenType]) -> Option<Token> {
        if let Some(token) = self.peek() {
            for tt in types.iter().as_ref() {
                if token.t == *tt {
                    return Some(self.next().unwrap());
                }
            }
        }

        None
    }

    /// Returns true if the following token is of one of the types of `types`
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

    fn next_is_operator(&mut self) -> bool {
        if let Some(next) = self.peek() {
            return next.t.infix_binding_power().is_some()
                || next.t.prefix_binding_power().is_some();
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::scanner::Scanner;
    use crate::parser::Error;
    use crate::parser::Parser;
    use crate::parser::Token;
    use crate::parser::TokenType;

    #[test]
    fn test_creates_ast_for_addition() {
        let mut scanner = Scanner::new("<?php\n1 + 2 == 3;");
        scanner.scan().unwrap();
    }

    #[test]
    fn test_creates_ast_with_doc_comments() {
        let code = "<?php
        $a = new class extends AbstractDatabaseStatisticProvider
        {

            private string $namespace, $rofl;

            /**
             * Hello !
             * or not?
             * @param string|null $rofl The best variable in the world
             * @return string
             * @deprecated
             */
            static public function getIdentifier(string $rofl): string
            {
                /** @var string $x */
                $x = rofl();
                return '';
            }
        };
        ";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan().unwrap();

        assert_eq!(true, Parser::ast(tokens.clone()).is_ok());
    }

    #[test]
    fn test_expects_identifier_after_object_operator() {
        let code = "<?php
        class Test {
            public function method() {
                $this->
            }
        }
        ";

        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan().unwrap();
        let ast_result = Parser::ast(tokens.clone());

        let expected = Error::MissingIdentifier {
            token: Token {
                col: 23,
                line: 3,
                t: TokenType::Missing,
                label: None,
            },
        };

        assert_eq!(&expected, ast_result.unwrap().1.first().unwrap());
    }

    #[test]
    fn test_expects_either_a_semicolon_or_a_script_end_after_an_expression_statement() {
        let code_semicolon = "<?php echo 1 + 1; ";

        let mut scanner = Scanner::new(code_semicolon);
        let tokens = scanner.scan().unwrap();
        let ast_result = Parser::ast(tokens.clone());

        assert!(ast_result.unwrap().1.is_empty());

        let code_semicolon = "<?php 
        empty(2)
        ?>";

        let mut scanner = Scanner::new(code_semicolon);
        let tokens = scanner.scan().unwrap();
        let ast_result = Parser::ast(tokens.clone());

        assert!(ast_result.unwrap().1.is_empty());
    }

    #[test]
    fn test_parses_attributes_without_errors() {
        let code_semicolon = "
        <?php
        use Attribute;
        
        #[Attribute(Attribute::TARGET_METHOD | Attribute::TARGET_FUNCTION)]
        class MyAttribute
        {
        }

        #[Attribute]
        class TheOtherAttribute {}
";

        let mut scanner = Scanner::new(code_semicolon);
        let tokens = scanner.scan().unwrap();
        let ast_result = Parser::ast(tokens.clone());

        assert!(ast_result.unwrap().1.is_empty());
    }

    #[test]
    fn test_parses_deeply_nested_assignments() {
        let code_semicolon = r#"
        <?php
 $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = 
 $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = 
 $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = 
 $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = $x->p = [];       
"#;

        let mut scanner = Scanner::new(code_semicolon);
        let tokens = scanner.scan().unwrap();
        let ast_result = Parser::ast(tokens.clone());

        assert!(ast_result.unwrap().1.is_empty());
    }
}
