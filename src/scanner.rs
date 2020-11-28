use crate::node::NodeRange;
use crate::token::{Token, TokenType};

/// Enum to represent the current scanner context. Can either be within a code block (`InScript`),
/// within a comment within a code block (`InComment`), or between code blocks (`OutScript`). An
/// example for the latter would be `...?> HERE <?php ...`
#[derive(PartialEq)]
enum Context {
    InScript,
    OutScript,
    InComment,
}

/// The `Scanner` type is used to generate a token stream from an input string. The
/// input string is the content of a PHP source file.
pub struct Scanner {
    col: u32,
    start_of_token: (u32, u32),
    line: u32,
    pos: u32,

    context: Context,
    pub tokens: Vec<Token>,
    chars: Vec<char>,
}

/// Inspired by https://craftinginterpreters.com/statements-and-state.html
impl Scanner {
    /// Constructs a new `Scanner` without actually scanning anything.
    ///
    /// # Example
    ///
    /// ```
    /// use scanner::Scanner;
    ///
    /// let scanner = Scanner::new("<?php echo 'Hello World'; ?>");
    /// ```
    pub fn new(source: &str) -> Self {
        let chars = source.chars().rev().collect::<Vec<char>>();

        Scanner {
            col: 0,
            start_of_token: (0, 0),
            line: 0,
            pos: 0,
            context: Context::OutScript,
            tokens: Vec::new(),
            chars,
        }
    }

    /// Return the range of the entire document
    pub fn document_range(&self) -> NodeRange {
        ((0, 0), (self.line as u32, self.col as u32))
    }

    /// Scans the source file into a token stream `tokens` and returns a result containing a
    /// reference to that token stream.
    ///
    /// # Example
    ///
    /// ```
    /// pub mod scanner;
    ///
    /// use crate::scanner::Scanner;
    ///
    /// let scanner = Scanner::new("<?php echo 'Hello World'; ?>");
    /// let tokens = scanner.scan();
    ///
    /// assert_eq!(true, tokens.is_ok());
    /// ```
    pub fn scan(&mut self) -> Result<&Vec<Token>, String> {
        loop {
            self.start_of_token = (self.line, self.col);

            let c = match self.advance() {
                Some(c) => c,
                _ => {
                    self.push_token(TokenType::Eof);

                    break;
                }
            };

            // Ignore everything until the multiline comment is done
            if self.context == Context::InComment {
                continue;
            }

            if self.context == Context::OutScript && c != '<' {
                continue;
            }

            match c {
                ' ' | '\t' | '\r' | '\n' => {}
                ':' => match self.peek() {
                    Some(':') => {
                        self.advance();
                        self.push_token(TokenType::PaamayimNekudayim);
                    }
                    _ => {
                        self.push_token(TokenType::Colon);
                    }
                },
                '@' => {
                    self.push_token(TokenType::Silencer);
                }
                '~' => {
                    self.push_token(TokenType::BitwiseNegation);
                }
                ',' => {
                    self.push_token(TokenType::Comma);
                }
                '\\' => {
                    self.push_token(TokenType::NamespaceSeparator);
                }
                '&' => match self.peek() {
                    Some('=') => {
                        self.advance();

                        self.push_token(TokenType::BinaryAndAssignment);
                    }
                    Some('&') => {
                        self.advance();

                        self.push_token(TokenType::LogicAnd);
                    }
                    _ => {
                        self.push_token(TokenType::BinaryAnd);
                    }
                },
                '%' => match self.peek() {
                    Some('=') => {
                        self.advance();

                        self.push_token(TokenType::ModuloAssignment);
                    }
                    _ => {
                        self.push_token(TokenType::Modulo);
                    }
                },
                '|' => match self.peek() {
                    Some('=') => {
                        self.advance();

                        self.push_token(TokenType::BinaryOrAssignment);
                    }
                    Some('|') => {
                        self.advance();

                        self.push_token(TokenType::LogicOr);
                    }
                    _ => {
                        self.push_token(TokenType::BinaryOr);
                    }
                },
                '>' => match self.peek() {
                    Some('>') => {
                        self.advance();

                        match self.peek() {
                            Some('=') => {
                                self.advance();

                                self.push_token(TokenType::RightShiftAssignment);
                            }
                            _ => {
                                self.push_token(TokenType::RightShift);
                            }
                        }
                    }
                    Some('=') => {
                        self.advance();

                        self.push_token(TokenType::GreaterOrEqual);
                    }
                    _ => {
                        self.push_token(TokenType::Greater);
                    }
                },
                '<' => match self.peek() {
                    Some('<') => {
                        self.advance();

                        match self.peek() {
                            Some('<') => {
                                self.advance();

                                self.tokenize_here_doc()?;
                            }
                            Some('=') => {
                                self.advance();

                                self.push_token(TokenType::LeftShiftAssignment);
                            }
                            _ => {
                                self.push_token(TokenType::LeftShift);
                            }
                        }
                    }
                    Some('?') => {
                        self.advance();
                        match self.peek() {
                            Some(' ') | Some('\n') | Some('\t') | Some('\r') => {
                                self.context = Context::InScript;
                                self.push_token(TokenType::ScriptStart);
                            }
                            Some('p') => {
                                self.advance();

                                if let Some('h') = self.peek() {
                                    self.advance();

                                    if let Some('p') = self.peek() {
                                        self.advance();

                                        self.context = Context::InScript;
                                        self.push_token(TokenType::ScriptStart);
                                    }
                                }
                            }
                            // Might be something like <?xml
                            _ => continue,
                        }
                    }
                    Some('=') => {
                        self.advance();

                        match self.peek() {
                            Some('>') => {
                                self.advance();

                                self.push_token(TokenType::SpaceShip);
                            }
                            _ => {
                                self.push_token(TokenType::SmallerOrEqual);
                            }
                        }
                    }
                    _ => {
                        self.push_token(TokenType::Smaller);
                    }
                },
                '?' => match self.peek() {
                    Some('>') => {
                        self.advance();
                        self.push_token(TokenType::ScriptEnd);
                        self.context = Context::OutScript;
                    }
                    Some('?') => {
                        self.advance();

                        match self.peek() {
                            Some('=') => {
                                self.advance();

                                self.push_token(TokenType::CoalesceAssignment);
                            }
                            _ => {
                                self.push_token(TokenType::Coalesce);
                            }
                        }
                    }
                    _ => {
                        self.push_token(TokenType::QuestionMark);
                    }
                },
                '!' => match self.peek() {
                    Some('=') => {
                        self.advance();

                        match self.peek() {
                            Some('=') => {
                                self.advance();
                                self.push_token(TokenType::IsNotIdentical);
                            }
                            _ => {
                                self.push_token(TokenType::IsNotEqual);
                            }
                        }
                    }
                    _ => {
                        self.push_token(TokenType::Negation);
                    }
                },
                '.' => match self.peek() {
                    Some('.') => {
                        self.advance();

                        match self.peek() {
                            Some('.') => {
                                self.advance();

                                self.push_token(TokenType::Elipsis);
                            }
                            _ => {
                                self.push_token(TokenType::Concat);
                                self.push_token(TokenType::Concat);
                            }
                        }
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::ConcatAssignment);
                    }
                    Some('0'..='9') => {
                        let decimal = self.collect_number();

                        self.push_named_token(TokenType::DecimalNumber, &format!("0.{}", decimal));
                    }
                    _ => {
                        self.push_token(TokenType::Concat);
                    }
                },
                '^' => match self.peek() {
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::XorAssignment);
                    }
                    _ => {
                        self.push_token(TokenType::BinaryXor);
                    }
                },
                '+' => match self.peek() {
                    Some('+') => {
                        self.advance();
                        self.push_token(TokenType::Increment);
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::PlusAssign);
                    }
                    _ => {
                        self.push_token(TokenType::Plus);
                    }
                },
                '-' => match self.peek() {
                    Some('-') => {
                        self.advance();

                        self.push_token(TokenType::Decrement);
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::MinusAssign);
                    }
                    Some('>') => {
                        self.advance();
                        self.push_token(TokenType::ObjectOperator);
                    }
                    _ => {
                        self.push_token(TokenType::Minus);
                    }
                },
                '*' => match self.peek() {
                    Some('*') => {
                        self.advance();

                        match self.peek() {
                            Some('=') => {
                                self.advance();

                                self.push_token(TokenType::PowerAssignment);
                            }
                            _ => {
                                self.push_token(TokenType::Power);
                            }
                        }
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::MulAssign);
                    }
                    _ => {
                        self.push_token(TokenType::Multiplication);
                    }
                },
                '#' => {
                    self.advance_until_after_line_comment();

                    //self.push_token(TokenType::LineComment);
                }
                '/' => match self.peek() {
                    Some('/') => {
                        self.advance_until_after_line_comment();

                        //self.push_token(TokenType::LineComment);
                    }
                    Some('*') => {
                        self.advance();
                        self.collect_doc_comment_information();
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::DivAssign);
                    }
                    _ => {
                        self.push_token(TokenType::Division);
                    }
                },
                '=' => match self.peek() {
                    Some('=') => {
                        self.advance();

                        match self.peek() {
                            Some('=') => {
                                self.advance();
                                self.push_token(TokenType::IsIdentical);
                            }
                            _ => {
                                self.push_token(TokenType::IsEqual);
                            }
                        }
                    }
                    Some('>') => {
                        self.advance();

                        self.push_token(TokenType::DoubleArrow);
                    }
                    _ => {
                        self.push_token(TokenType::Assignment);
                    }
                },
                '$' => {
                    let name = self.collect_identifer();

                    if !name.is_empty() {
                        self.push_named_token(TokenType::Variable, &name);
                    } else {
                        self.push_token(TokenType::Variable);
                    }
                }
                '0'..='9' => {
                    let mut number = String::new();
                    number.push(c);

                    if let Some('x') = self.peek() {
                        if c == '0' {
                            number.push('x');
                            self.advance();
                            number.push_str(&self.collect_hex_number());
                            self.push_named_token(TokenType::HexNumber, &number);

                            continue;
                        }
                    }

                    if let Some('b') = self.peek() {
                        if c == '0' {
                            number.push('b');
                            self.advance();
                            number.push_str(&self.collect_binary_number());
                            self.push_named_token(TokenType::BinaryNumber, &number);

                            continue;
                        }
                    }

                    number.push_str(&self.collect_number());
                    let mut number_type = TokenType::LongNumber;

                    if let Some(&'.') = self.peek() {
                        number.push('.');
                        self.advance();

                        let decimal = self.collect_number();

                        number.push_str(&decimal);

                        number_type = TokenType::DecimalNumber;
                    }

                    if let Some(&'e') | Some(&'E') = self.peek() {
                        self.advance();
                        number.push('e');

                        match self.peek() {
                            Some('-') => {
                                self.advance();
                                number.push('-')
                            }
                            Some('+') => {
                                self.advance();
                                number.push('+')
                            }
                            _ => {}
                        }

                        let decimal = self.collect_number();

                        self.push_named_token(
                            TokenType::ExponentialNumber,
                            &format!("{}{}", number, decimal),
                        );
                    } else {
                        self.push_named_token(number_type, &number);
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' | '\u{0080}'..='\u{00ff}' => {
                    let mut name = String::new();
                    name.push(c);
                    name.push_str(&self.collect_identifer());

                    if let Some(t) = self.map_keyword(&name) {
                        self.push_token(t);
                    } else if name == "from" {
                        // Combine yield from to one token ...

                        if let Some(Token {
                            t: TokenType::Yield,
                            ..
                        }) = self.tokens.last()
                        {
                            self.tokens.pop();
                            self.push_token(TokenType::YieldFrom);
                        } else {
                            self.push_named_token(TokenType::Identifier, &name);
                        }
                    } else {
                        self.push_named_token(TokenType::Identifier, &name);
                    }
                }
                '(' => {
                    self.push_token(TokenType::OpenParenthesis);
                }
                ')' => {
                    if let Some(previous_token) = self.tokens.pop() {
                        // Is the last recorded token a type?
                        if let Some(cast_to) = self.map_cast(&previous_token.t) {
                            // It was! Was the one recorded before an open parenthesis?
                            if let Some(Token {
                                t: TokenType::OpenParenthesis,
                                ..
                            }) = self.tokens.last()
                            {
                                // It was! Replace that one with a type cast
                                self.tokens.pop();
                                self.push_token(cast_to);

                                continue;

                            // It was not. Put it back on the stack
                            } else {
                                // No token before that one ... so it can not be a cast either and we put it back
                                // on the stack
                                self.tokens.push(previous_token);
                            }
                        } else {
                            // Is no cast, put it back on the stack
                            self.tokens.push(previous_token);
                        }
                    }

                    self.push_token(TokenType::CloseParenthesis);
                }
                '{' => {
                    self.push_token(TokenType::OpenCurly);
                }
                '}' => {
                    self.push_token(TokenType::CloseCurly);
                }
                '[' => {
                    self.push_token(TokenType::OpenBrackets);
                }
                ']' => {
                    self.push_token(TokenType::CloseBrackets);
                }
                ';' => {
                    self.push_token(TokenType::Semicolon);
                }
                '\'' => {
                    let string = self.collect_until_escaped('\'');

                    self.push_named_token(TokenType::ConstantEncapsedString, &string);
                }
                '`' => {
                    let string = self.collect_until_escaped('`');

                    self.push_named_token(TokenType::ShellEscape, &string);
                }
                '"' => {
                    let string = self.collect_encapsed_and_whitespace_string();

                    self.push_named_token(TokenType::EncapsedAndWhitespaceString, &string);
                }
                _ => {
                    return Err(format!(
                        "Unexpected char '{}' at line {} column {}",
                        c, self.line, self.col
                    ));
                }
            }
        }

        Ok(&self.tokens)
    }

    fn tokenize_here_doc(&mut self) -> Result<(), String> {
        while let Some(' ') = self.peek() {
            self.advance();
        }

        let marker = match self.peek() {
            // TODO: Do not collect escaped!
            Some('"') => {
                self.advance();
                self.collect_until_escaped('"')
            }
            Some('\'') => {
                self.advance();
                self.collect_until_escaped('\'')
            }
            _ => self.collect_identifer(),
        };

        self.push_named_token(TokenType::HereDocStart, &marker);

        let mut heredoc = String::new();
        let mut line = String::new();
        let mut potential_end = false;

        while let Some(&c) = self.peek() {
            line.push(c);

            // New line, new luck. This could be the end! But first, push the previous line onto the result
            if c == '\n' {
                heredoc.push_str(&line);
                line.clear();
                potential_end = true;

                self.advance();

                continue;

            // Either this could still be the end, so skip blanks and tabs (they are added above anyway)
            // or it can not be the end anymore.
            } else if !potential_end || c == ' ' || c == '\t' {
                self.advance();

                continue;
            }

            let mut partial_line_or_end_marker = String::new();

            for marker_char in marker.chars().by_ref() {
                if let Some(next_c) = self.advance() {
                    // Ok, could still be the end
                    if next_c == marker_char {
                        partial_line_or_end_marker.push(next_c);

                    // No match ... this can not be the end anymore, so push what was collected to the
                    // current line and pick the next char in the parent loop
                    } else {
                        line.push_str(&partial_line_or_end_marker);

                        potential_end = false;

                        break;
                    }
                }
            }

            // If this is still true than we actually are done
            if potential_end {
                line = partial_line_or_end_marker;
                break;
            }
        }

        if self.peek().is_none() {
            return Err(String::from("Unterminated heredoc!"));
        }

        self.push_named_token(TokenType::EncapsedAndWhitespaceString, &heredoc);
        self.push_named_token(TokenType::HereDocEnd, &line);

        Ok(())
    }

    /// TODO: Make it support variables inside of it
    fn collect_encapsed_and_whitespace_string(&mut self) -> String {
        let mut name = String::new();
        let mut escaped = false;

        while let Some(c) = self.advance() {
            if c == '\\' {
                escaped = !escaped;
            } else if c == '"' && !escaped {
                break;
            } else if escaped {
                escaped = false;
            }

            name.push(c);
        }

        name
    }

    fn collect_until_escaped(&mut self, until: char) -> String {
        let mut name = String::new();
        let mut escaped = false;

        while let Some(c) = self.advance() {
            if c == '\\' {
                escaped = !escaped;
            } else if c == until && !escaped {
                break;
            } else if escaped {
                escaped = false;
            }

            name.push(c);
        }

        name
    }

    fn collect_identifer(&mut self) -> String {
        let mut name = String::new();

        while let Some(&c) = self.peek() {
            if ('a'..='z').contains(&c)
                || ('A'..='Z').contains(&c)
                || ('0'..='9').contains(&c)
                || c == '_'
                || c >= 0x80 as char
            {
                name.push(c);
            } else {
                break;
            }

            self.advance();
        }

        name
    }

    fn collect_number(&mut self) -> String {
        let mut number = String::new();

        while let Some(&c) = self.peek() {
            if ('0'..='9').contains(&c) {
                number.push(c);
            } else {
                break;
            }

            self.advance();
        }

        number
    }

    fn collect_hex_number(&mut self) -> String {
        let mut number = String::new();

        while let Some(&c) = self.peek() {
            if ('0'..='9').contains(&c) || ('a'..='f').contains(&c) || ('A'..='F').contains(&c) {
                number.push(c);
            } else {
                break;
            }
            self.advance();
        }

        number
    }

    fn collect_binary_number(&mut self) -> String {
        let mut number = String::new();

        while let Some(&c) = self.peek() {
            if c == '0' || c == '1' {
                number.push(c);
            } else {
                break;
            }
            self.advance();
        }

        number
    }

    /// Pop and return the next token
    fn next(&mut self) -> Option<char> {
        self.chars.pop()
    }

    /// Return the next token without popping it off the stream
    fn peek(&self) -> Option<&char> {
        self.chars.last()
    }

    fn advance(&mut self) -> Option<char> {
        self.pos += 1;

        if let Some(c) = self.next() {
            if c == '\n' || c == '\r' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }

            return Some(c);
        }

        None
    }

    /// Collect doc comment information like @return and @param
    fn collect_doc_comment_information(&mut self) {
        let mut comment = String::new();

        while let Some(c) = self.advance() {
            // Detects the end of the doc comment
            if c == '*' && self.peek() == Some(&'/') {
                self.advance();

                break;
            }

            comment.push(c);

            // Detects the end of the current line. Add and clear current_line.
            // Then skip until the leading * of the next line passed
            if c == '\n' {
                //self.advance_until_after('*');

                // Also skip spaces
                //self.advance_until_after(' ');
            }
        }

        // TODO: Fix start of the comment
        self.push_named_token(TokenType::MultilineComment, &comment);
    }

    /// Advances until after the end of the line comment, which can either be the script end or
    /// a newline
    fn advance_until_after_line_comment(&mut self) {
        while let Some(c) = self.advance() {
            match c {
                '\n' | '\r' => break,
                '?' => {
                    if let Some(&'>') = self.peek() {
                        self.advance();
                        self.push_token(TokenType::ScriptEnd);
                        break;
                    }
                }
                _ => {}
            }
        }
    }

    fn push_token(&mut self, t: TokenType) {
        if self.context != Context::InScript {
            return;
        }
        self.tokens.push(Token::new(
            t,
            self.start_of_token.0 as u32,
            self.start_of_token.1 as u32,
        ));
    }

    fn push_named_token(&mut self, t: TokenType, name: &str) {
        self.tokens.push(Token::named(
            t,
            self.start_of_token.0 as u32,
            self.start_of_token.1 as u32,
            name,
        ));
    }

    fn map_cast(&self, ident: &TokenType) -> Option<TokenType> {
        match ident {
            TokenType::TypeBool => Some(TokenType::BoolCast),
            TokenType::TypeInt => Some(TokenType::IntCast),
            TokenType::TypeString => Some(TokenType::StringCast),
            TokenType::TypeArray => Some(TokenType::ArrayCast),
            TokenType::TypeObject => Some(TokenType::ObjectCast),
            TokenType::TypeFloat => Some(TokenType::DoubleCast),
            TokenType::Unset => Some(TokenType::UnsetCast),
            _ => None,
        }
    }

    /// Returns the correct TokenType for a registered keyword
    fn map_keyword(&self, ident: &str) -> Option<TokenType> {
        match ident {
            "INF" => return Some(TokenType::ConstInf),
            "NAN" => return Some(TokenType::ConstNan),
            _ => {}
        };

        Some(match ident.to_lowercase().as_ref() {
            "bool" | "boolean" => TokenType::TypeBool,
            "int" | "integer" => TokenType::TypeInt,
            "string" | "binary" => TokenType::TypeString,
            "array" => TokenType::TypeArray,
            "object" => TokenType::TypeObject,
            "float" | "double" => TokenType::TypeFloat,
            "void" => TokenType::Void,
            "new" => TokenType::New,
            "clone" => TokenType::Clone,
            "exit" => TokenType::Exit,
            "if" => TokenType::If,
            "elseif" => TokenType::ElseIf,
            "else" => TokenType::Else,
            "endif" => TokenType::EndIf,
            "echo" => TokenType::Echo,
            "do" => TokenType::Do,
            "while" => TokenType::While,
            "endwhile" => TokenType::EndWhile,
            "for" => TokenType::For,
            "endfor" => TokenType::EndFor,
            "foreach" => TokenType::Foreach,
            "endforeach" => TokenType::EndForeach,
            "declare" => TokenType::Declare,
            "enddeclare" => TokenType::EndDeclare,
            "as" => TokenType::As,
            "switch" => TokenType::Switch,
            "endswitch" => TokenType::EndSwitch,
            "case" => TokenType::Case,
            "default" => TokenType::Default,
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "goto" => TokenType::Goto,
            "function" => TokenType::Function,
            "fn" => TokenType::Fn,
            "const" => TokenType::Const,
            "return" => TokenType::Return,
            "try" => TokenType::Try,
            "catch" => TokenType::Catch,
            "finally" => TokenType::Finally,
            "throw" => TokenType::Throw,
            "use" => TokenType::Use,
            "insteadof" => TokenType::Insteadof,
            "global" => TokenType::Global,
            "static" => TokenType::Static,
            "abstract" => TokenType::Abstract,
            "final" => TokenType::Final,
            "private" => TokenType::Private,
            "protected" => TokenType::Protected,
            "public" => TokenType::Public,
            "var" => TokenType::Var,
            "unset" => TokenType::Unset,
            "isset" => TokenType::Isset,
            "empty" => TokenType::Empty,
            "null" => TokenType::Null,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "__halt_compiler" => TokenType::HaltCompiler,
            "class" => TokenType::Class,
            "trait" => TokenType::Trait,
            "interface" => TokenType::Interface,
            "extends" => TokenType::Extends,
            "implements" => TokenType::Implements,
            "list" => TokenType::List,
            "callable" => TokenType::Callable,
            "and" => TokenType::LogicAnd,
            "or" => TokenType::LogicOr,
            "xor" => TokenType::LogicXor,
            "namespace" => TokenType::Namespace,
            "print" => TokenType::Print,
            "__line__" => TokenType::ConstLine,
            "__file__" => TokenType::ConstFile,
            "__dir__" => TokenType::ConstDir,
            "__class__" => TokenType::ConstClass,
            "__trait__" => TokenType::ConstTrait,
            "__method__" => TokenType::ConstMethod,
            "__function__" => TokenType::ConstFunction,
            "require" => TokenType::Require,
            "require_once" => TokenType::RequireOnce,
            "include" => TokenType::Include,
            "include_once" => TokenType::IncludeOnce,
            "instanceof" => TokenType::InstanceOf,
            "yield" => TokenType::Yield,
            "self" => TokenType::TypeSelf,
            "parent" => TokenType::Parent,
            "generator" => TokenType::Generator,
            _ => {
                return None;
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! token_list {
        ($vec:expr) => {
            $vec.iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<String>>()
                .join(" ");
        };
    }

    #[test]
    fn test_scans_expressions() {
        let mut scanner = Scanner::new("<?php\n$a = 1 + 2;\n$a++;$b +\n1\n + 2\n;");
        scanner.scan().unwrap();

        assert_eq!(scanner.tokens[0], Token::new(TokenType::ScriptStart, 0, 0));
        assert_eq!(
            scanner.tokens[1],
            Token::named(TokenType::Variable, 1, 0, "a")
        );
        assert_eq!(scanner.tokens[2], Token::new(TokenType::Assignment, 1, 3));
        assert_eq!(
            scanner.tokens[3],
            Token::named(TokenType::LongNumber, 1, 5, "1")
        );
        assert_eq!(scanner.tokens[4], Token::new(TokenType::Plus, 1, 7));
        assert_eq!(
            scanner.tokens[5],
            Token::named(TokenType::LongNumber, 1, 9, "2")
        );
        assert_eq!(scanner.tokens[6], Token::new(TokenType::Semicolon, 1, 10));
        assert_eq!(
            scanner.tokens[7],
            Token::named(TokenType::Variable, 2, 0, "a")
        );
        assert_eq!(scanner.tokens[8], Token::new(TokenType::Increment, 2, 2));
        assert_eq!(scanner.tokens[9], Token::new(TokenType::Semicolon, 2, 4));
        assert_eq!(
            scanner.tokens[10],
            Token::named(TokenType::Variable, 2, 5, "b")
        );
        assert_eq!(scanner.tokens[11], Token::new(TokenType::Plus, 2, 8));
        assert_eq!(
            scanner.tokens[12],
            Token::named(TokenType::LongNumber, 3, 0, "1")
        );
        assert_eq!(scanner.tokens[13], Token::new(TokenType::Plus, 4, 1));
        assert_eq!(
            scanner.tokens[14],
            Token::named(TokenType::LongNumber, 4, 3, "2")
        );
        assert_eq!(scanner.tokens[15], Token::new(TokenType::Semicolon, 5, 0));
        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a = 1 + 2 ; $a ++ ; $b + 1 + 2 ; "
        );
    }

    #[test]
    fn test_scans_multibyte_string() {
        let mut scanner = Scanner::new(
            "<?php
$object->{'\u{6771}\u{4eac}'} = 2020;
",
        );

        scanner.scan().unwrap();

        assert_eq!(scanner.tokens[0], Token::new(TokenType::ScriptStart, 0, 0));
        assert_eq!(
            scanner.tokens[1],
            Token::named(TokenType::Variable, 1, 0, "object")
        );
        assert_eq!(
            scanner.tokens[2],
            Token::new(TokenType::ObjectOperator, 1, 7)
        );
        assert_eq!(scanner.tokens[3], Token::new(TokenType::OpenCurly, 1, 9));
        assert_eq!(
            scanner.tokens[4],
            Token::named(TokenType::ConstantEncapsedString, 1, 10, "\u{6771}\u{4eac}")
        );
    }

    #[test]
    fn test_scans_empty_string_and_not_empty_string() {
        let mut scanner = Scanner::new(
            "<?php
'abc' . '' . 'def';
",
        );

        scanner.scan().unwrap();
        assert_eq!(
            scanner.tokens[1],
            Token::named(TokenType::ConstantEncapsedString, 1, 0, "abc")
        );
        assert_eq!(scanner.tokens[2], Token::new(TokenType::Concat, 1, 6));
        assert_eq!(
            scanner.tokens[3],
            Token::named(TokenType::ConstantEncapsedString, 1, 8, "")
        );
        assert_eq!(scanner.tokens[4], Token::new(TokenType::Concat, 1, 11));

        assert_eq!(
            scanner.tokens[5],
            Token::named(TokenType::ConstantEncapsedString, 1, 13, "def")
        );
    }

    #[test]
    fn test_scans_array() {
        let mut scanner = Scanner::new(
            "<?php
['rofl' => 'copter'];",
        );

        scanner.scan().unwrap();
        assert_eq!(scanner.tokens[1], Token::new(TokenType::OpenBrackets, 1, 0));
        assert_eq!(
            scanner.tokens[2],
            Token::named(TokenType::ConstantEncapsedString, 1, 1, "rofl")
        );
        assert_eq!(scanner.tokens[3], Token::new(TokenType::DoubleArrow, 1, 8));
        assert_eq!(
            scanner.tokens[4],
            Token::named(TokenType::ConstantEncapsedString, 1, 11, "copter")
        );
        assert_eq!(
            scanner.tokens[5],
            Token::new(TokenType::CloseBrackets, 1, 19)
        );
    }

    #[test]
    fn test_scans_string_argument() {
        let mut scanner = Scanner::new(
            "<?php
func('rofl');",
        );

        scanner.scan().unwrap();
        assert_eq!(
            scanner.tokens[1],
            Token::named(TokenType::Identifier, 1, 0, "func")
        );
        assert_eq!(
            scanner.tokens[2],
            Token::new(TokenType::OpenParenthesis, 1, 4)
        );
        assert_eq!(
            scanner.tokens[3],
            Token::named(TokenType::ConstantEncapsedString, 1, 5, "rofl")
        );
        assert_eq!(
            scanner.tokens[4],
            Token::new(TokenType::CloseParenthesis, 1, 11)
        );
        assert_eq!(scanner.tokens[5], Token::new(TokenType::Semicolon, 1, 12));
    }

    #[test]
    fn test_scans_keywords() {
        let mut scanner = Scanner::new(
            "<?php
while (true) {}",
        );

        scanner.scan().unwrap();

        assert_eq!(scanner.tokens[1], Token::new(TokenType::While, 1, 0));
        assert_eq!(
            scanner.tokens[2],
            Token::new(TokenType::OpenParenthesis, 1, 6)
        );
        assert_eq!(scanner.tokens[3], Token::new(TokenType::True, 1, 7));
        assert_eq!(
            scanner.tokens[4],
            Token::new(TokenType::CloseParenthesis, 1, 11)
        );
        assert_eq!(scanner.tokens[5], Token::new(TokenType::OpenCurly, 1, 13));
        assert_eq!(scanner.tokens[6], Token::new(TokenType::CloseCurly, 1, 14));
    }

    #[test]
    fn test_scans_for_loop() {
        let mut scanner = Scanner::new(
            "<?php
for ($i = 0; $i < 100; $i++) {}",
        );

        scanner.scan().unwrap();

        assert_eq!(scanner.tokens[1], Token::new(TokenType::For, 1, 0));
        assert_eq!(
            scanner.tokens[2],
            Token::new(TokenType::OpenParenthesis, 1, 4)
        );
        assert_eq!(
            scanner.tokens[3],
            Token::named(TokenType::Variable, 1, 5, "i")
        );
        assert_eq!(scanner.tokens[4], Token::new(TokenType::Assignment, 1, 8));
        assert_eq!(
            scanner.tokens[5],
            Token::named(TokenType::LongNumber, 1, 10, "0")
        );
        assert_eq!(scanner.tokens[6], Token::new(TokenType::Semicolon, 1, 11));

        assert_eq!(
            scanner.tokens[7],
            Token::named(TokenType::Variable, 1, 13, "i")
        );
        assert_eq!(scanner.tokens[8], Token::new(TokenType::Smaller, 1, 16));
        assert_eq!(
            scanner.tokens[9],
            Token::named(TokenType::LongNumber, 1, 18, "100")
        );
        assert_eq!(scanner.tokens[10], Token::new(TokenType::Semicolon, 1, 21));

        assert_eq!(
            scanner.tokens[11],
            Token::named(TokenType::Variable, 1, 23, "i")
        );
        assert_eq!(scanner.tokens[12], Token::new(TokenType::Increment, 1, 25));
        assert_eq!(
            scanner.tokens[13],
            Token::new(TokenType::CloseParenthesis, 1, 27)
        );

        assert_eq!(scanner.tokens[14], Token::new(TokenType::OpenCurly, 1, 29));
        assert_eq!(scanner.tokens[15], Token::new(TokenType::CloseCurly, 1, 30));
    }

    #[test]
    fn test_scans_boolean_operators() {
        let mut scanner = Scanner::new(
            "<?php
            true ||
            false &&
            1 === 1 &&
            $a <=> $b &&
            2 !== 1;
            2 <= 1;
            2 >= 1;
            2 ?? 1;
            2 == 1;
            2 != 1;
            2 > 1;
            2 < 1;
            2 % 1 == 1;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php true || false && 1 === 1 && $a <=> $b && 2 !== 1 ; 2 <= 1 ; 2 >= 1 ; 2 ?? 1 ; 2 == 1 ; 2 != 1 ; \
            2 > 1 ; 2 < 1 ; 2 % 1 == 1 ; "
        );
    }

    #[test]
    fn test_scans_assignments() {
        let mut scanner = Scanner::new(
            "<?php
            $a <<= 2;
            $b >>= 2;
            $c *= 2;
            $d += 2;
            $e -= 2;
            $f /= 2;
            $g %= 2;
            $h **= 2;
            $i ??= 2;
            $j &= 2;
            $k |= 2;
            $l .= 2;
            $m ^= 2;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a <<= 2 ; $b >>= 2 ; $c *= 2 ; $d += 2 ; $e -= 2 ; $f /= 2 ; $g %= 2 ; $h **= 2 ; $i ??= 2 ; $j &= 2 ; $k |= 2 ; $l .= 2 ; $m ^= 2 ; "
        );
    }

    #[test]
    fn test_scans_inline_html() {
        let mut scanner = Scanner::new(
            "<?php
            $a = 1; ?>
            Some html here
            <?php $b = 2;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a = 1 ; ?> <?php $b = 2 ; "
        );
    }

    #[test]
    fn test_scans_unary_operators() {
        let mut scanner = Scanner::new("<?php $i++; --$i; $a = !$a; @call();");

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $i ++ ; -- $i ; $a = ! $a ; @ call ( ) ; "
        );
    }

    #[test]
    fn test_parses_doc_comments_and_ignores_line_comments() {
        let mut scanner = Scanner::new(
            "<?php
            // Line comment
            echo 'blubb'; # alternative line comment
            /** some multiline comment */
            echo 'blabb';
            // ?>
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php echo 'blubb' ; /** some multiline comment */ echo 'blabb' ; ?> "
        );
    }

    #[test]
    fn test_scans_arithmetic_expression() {
        let mut scanner = Scanner::new("<?php 1 + 2 / 3 * (2 + 2 - 1 ** 2);");

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php 1 + 2 / 3 * ( 2 + 2 - 1 ** 2 ) ; "
        );
    }

    #[test]
    fn test_scans_dynamic_variable() {
        let mut scanner = Scanner::new("<?php $$$$$$a = 1;");

        scanner.scan().unwrap();

        assert_eq!(token_list!(scanner.tokens), "<?php $ $ $ $ $ $a = 1 ; ");
    }

    #[test]
    fn test_scans_binary_operators() {
        let mut scanner = Scanner::new(
            "<?php
            2 ^ 1;
            2 ~ 1;
            2 | 1;
            2 & 1;
            2 >> 1;
            2 << 1;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php 2 ^ 1 ; 2 ~ 1 ; 2 | 1 ; 2 & 1 ; 2 >> 1 ; 2 << 1 ; "
        );
    }

    #[test]
    fn test_scans_nested_array() {
        let mut scanner = Scanner::new("<?php $a = [1, 2, 3, [1, 2,]];");

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a = [ 1 , 2 , 3 , [ 1 , 2 , ] ] ; "
        );
    }

    #[test]
    fn test_scans_namespace_paths() {
        let mut scanner = Scanner::new(
            "<?php
            namespace Rofl\\Copter;

            use function \\Rofl;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php namespace Rofl \\ Copter ; use function \\ Rofl ; "
        );
    }

    #[test]
    fn test_scans_functions() {
        let mut scanner = Scanner::new(
            "<?php
            function some(?string $a, ...$rest): void {
                echo 1;
            }
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php function some ( ? string $a , ... $rest ) : void { echo 1 ; } "
        );
    }

    #[test]
    fn test_scans_yield_and_yield_from() {
        let mut scanner = Scanner::new(
            "<?php
            yield 200;
            yield from $rofl;
            from;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php yield 200 ; yield from $rofl ; from ; "
        );
    }

    #[test]
    fn test_scans_type_casts() {
        let mut scanner =
            Scanner::new("<?php $a = (string) (int) (bool) (array) (object) (float) (unset) $a;");

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a = (string) (int) (bool) (array) (object) (double) (unset) $a ; "
        );
    }

    #[test]
    fn test_scans_numbers() {
        let mut scanner = Scanner::new(
            "<?php
            $a = 1;
            $b = 2.0;
            $c = 1e10;
            $d = -100;
            $e = 0x1A;
            $f = 0b101010;
            $g = .2;
            $h = +1E5;
            $i = -2E2;
            $j = 1e-1;
            $k = 1e+1;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a = 1 ; $b = 2.0 ; $c = 1e10 ; $d = - 100 ; $e = 0x1A ; $f = 0b101010 ; $g = 0.2 ; \
            $h = + 1e5 ; $i = - 2e2 ; $j = 1e-1 ; $k = 1e+1 ; "
        );
    }

    #[test]
    fn test_scans_strings() {
        let mut scanner = Scanner::new(
            "<?php
            $a = 'aaa';
            $b = \"ddd\";
            $c = <<<  EOF
                Some fancy value
            EOF;
            $d = `ls -al`;
            $e = <<<\"EOF\"
                LOL
            EOF;
            $e = <<<'EOF'
                LOL
            EOF;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a = 'aaa' ; $b = \"ddd\" ; $c = <<<EOF \"\n                Some fancy value\n\" EOF ; $d = `ls -al` ; $e = <<<EOF \"\n                LOL\n\" EOF ; $e = <<<EOF \"\n                LOL\n\" EOF ; "
        );
    }

    #[test]
    fn test_scans_escaped_strings() {
        let mut scanner = Scanner::new(
            "<?php
            $a = 'aa\\'a';
            $b = \"d\\\"dd\";
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a = 'aa\\'a' ; $b = \"d\\\"dd\" ; "
        );
    }

    #[test]
    fn test_scans_even_invalid_sequences() {
        let mut scanner = Scanner::new(
            "<?php
            $a = $x . $y .. ;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(token_list!(scanner.tokens), "<?php $a = $x . $y . . ; ");
    }

    #[test]
    fn test_scans_short_script_start_tag() {
        let mut scanner = Scanner::new(
            "<?xml>I shall be ignored</xml><?
            $a = 1;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(token_list!(scanner.tokens), "<?php $a = 1 ; ");
    }

    #[test]
    fn test_scans_label_with_separate_colon() {
        let mut scanner = Scanner::new(
            "<?php
            goto_this_one:
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(token_list!(scanner.tokens), "<?php goto_this_one : ");
    }

    #[test]
    fn test_scans_static_access() {
        let mut scanner = Scanner::new(
            "<?php
            $object::property;
        ",
        );

        scanner.scan().unwrap();

        assert_eq!(token_list!(scanner.tokens), "<?php $object :: property ; ");
    }

    #[test]
    fn test_scans_sematically_invalid_sequences() {
        let mut scanner = Scanner::new("<?php $a = (string int) $2; ");

        scanner.scan().unwrap();

        assert_eq!(
            token_list!(scanner.tokens),
            "<?php $a = ( string int ) $2 ; "
        );

        let mut scanner = Scanner::new("<?php string) ");

        scanner.scan().unwrap();

        assert_eq!(token_list!(scanner.tokens), "<?php string ) ");
    }

    #[test]
    fn test_handles_unterminated_here_doc() {
        let mut scanner = Scanner::new("<?php $a = <<<EOF ");

        assert_eq!(true, scanner.scan().is_err())
    }

    #[test]
    fn test_handles_unexpected_char() {
        let mut scanner = Scanner::new("<?php \u{107}");

        assert_eq!(true, scanner.scan().is_err())
    }

    #[test]
    fn test_calculates_document_range() {
        let mut scanner = Scanner::new("<?php ; \n; \n;");

        scanner.scan().unwrap();

        assert_eq!(((0, 0), (2, 1)), scanner.document_range());
    }
}
