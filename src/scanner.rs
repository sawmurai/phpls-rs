use crate::token::{Token, TokenType};

use std::iter::Peekable;
use std::str::Chars;

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
pub struct Scanner<'a> {
    col: u32,
    start_of_token: u32,
    line: u32,
    pos: u32,

    context: Context,
    pub tokens: Vec<Token>,
    chars: Peekable<Chars<'a>>,
}

/// Inspired by https://craftinginterpreters.com/statements-and-state.html
impl<'a> Scanner<'a> {
    /// Constructs a new `Scanner` without actually scanning anything.
    ///
    /// # Example
    ///
    /// ```
    /// use scanner::Scanner;
    ///
    /// let scanner = Scanner::new("<?php echo 'Hello World'; ?>");
    /// ```
    pub fn new(source: &'a str) -> Self {
        Scanner {
            col: 1,
            start_of_token: 1,
            line: 1,
            pos: 0,
            context: Context::OutScript,

            tokens: Vec::new(),
            chars: source.chars().peekable(),
        }
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
            if self.chars.peek().is_none() {
                break;
            }

            self.start_of_token = self.col;

            let c = match self.advance() {
                Some(c) => c,
                _ => {
                    break;
                }
            };

            // Ignore everything until the multiline comment is done
            if self.context == Context::InComment {
                // TODO: Make the unwrap_or less hacky
                if c == '*' && self.chars.peek().unwrap_or(&' ') == &'/' {
                    self.context = Context::InScript;
                }

                continue;
            }

            if self.context == Context::OutScript && c != '<' {
                continue;
            }

            match c {
                ' ' | '\t' | '\r' | '\n' => {}
                ':' => match self.chars.peek() {
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
                '&' => match self.chars.peek() {
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
                '%' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();

                        self.push_token(TokenType::ModuloAssignment);
                    }
                    _ => {
                        self.push_token(TokenType::Modulo);
                    }
                },
                '|' => match self.chars.peek() {
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
                '>' => match self.chars.peek() {
                    Some('>') => {
                        self.advance();

                        match self.chars.peek() {
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

                        self.push_token(TokenType::SmallerOrEqual);
                    }
                    _ => {
                        self.push_token(TokenType::Greater);
                    }
                },
                '<' => match self.chars.peek() {
                    Some('<') => {
                        self.advance();

                        match self.chars.peek() {
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
                        self.context = Context::InScript;
                        self.push_token(TokenType::ScriptStart);

                        if let Some('p') = self.chars.peek() {
                            self.advance();

                            if let Some('h') = self.chars.peek() {
                                self.advance();

                                if let Some('p') = self.chars.peek() {
                                    self.advance();
                                }
                            }
                        }
                    }
                    Some('=') => {
                        self.advance();

                        match self.chars.peek() {
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
                '?' => match self.chars.peek() {
                    Some('>') => {
                        self.advance();
                        self.push_token(TokenType::ScriptEnd);
                        self.context = Context::OutScript;
                    }
                    Some('?') => {
                        self.advance();

                        match self.chars.peek() {
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
                '!' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();

                        match self.chars.peek() {
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
                '.' => match self.chars.peek() {
                    Some('.') => {
                        self.advance();

                        match self.chars.peek() {
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
                        self.advance();

                        let decimal = self.collect_number();

                        self.push_named_token(TokenType::DecimalNumber, &format!("0.{}", decimal));
                    }
                    _ => {
                        self.push_token(TokenType::Concat);
                    }
                },
                '^' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::XorAssignment);
                    }
                    _ => {
                        self.push_token(TokenType::BinaryXor);
                    }
                },
                '+' => match self.chars.peek() {
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
                '-' => match self.chars.peek() {
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
                '*' => match self.chars.peek() {
                    Some('*') => {
                        self.advance();

                        match self.chars.peek() {
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
                    self.advance_until_after('\n');

                    //self.push_token(TokenType::LineComment);
                }
                '/' => match self.chars.peek() {
                    Some('/') => {
                        self.advance_until_after('\n');

                        //self.push_token(TokenType::LineComment);
                    }
                    Some('*') => {
                        self.advance_until_after_multiline_comment();
                        //self.push_token(TokenType::MultilineComment);
                    }
                    Some('=') => {
                        self.advance();
                        self.push_token(TokenType::DivAssign);
                    }
                    _ => {
                        self.push_token(TokenType::Division);
                    }
                },
                '=' => match self.chars.peek() {
                    Some('=') => {
                        self.advance();

                        match self.chars.peek() {
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

                    if name != "" {
                        self.push_named_token(TokenType::Variable, &name);
                    } else {
                        self.push_token(TokenType::Variable);
                    }
                }
                '0'..='9' => {
                    let mut number = String::new();
                    number.push(c);

                    if let Some('x') = self.chars.peek() {
                        if c == '0' {
                            self.advance();
                            number.push_str(&self.collect_hex_number());
                            self.push_named_token(TokenType::HexNumber, &number);

                            continue;
                        }
                    }

                    if let Some('b') = self.chars.peek() {
                        if c == '0' {
                            self.advance();
                            number.push_str(&self.collect_binary_number());
                            self.push_named_token(TokenType::BinaryNumber, &number);

                            continue;
                        }
                    }

                    number.push_str(&self.collect_number());
                    let mut number_type = TokenType::LongNumber;

                    if let Some(&'.') = self.chars.peek() {
                        self.advance();

                        let decimal = self.collect_number();

                        number.push(c);
                        number.push_str(&decimal);

                        number_type = TokenType::DecimalNumber;
                    }

                    if let Some(&'e') | Some(&'E') = self.chars.peek() {
                        self.advance();
                        number.push('e');

                        match self.chars.peek() {
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
                        match self.chars.peek() {
                            // TODO: Refactor this detection as it has problems with spaces
                            // in the type cast ... like (  string  )
                            Some(')') => {
                                // Potential type cast, check if identifier could match
                                if let Some(cast_to) = self.map_cast(&t) {
                                    // Looks like a match ... was the previously recorded token an open parenthesis?
                                    if let Some(last_token) = self.tokens.pop() {
                                        // It was! Replace that one with a type cast
                                        if last_token.t == TokenType::OpenParenthesis {
                                            self.chars.next();
                                            self.push_token(cast_to);

                                        // It was not. Put it back on the stack
                                        } else {
                                            self.tokens.push(last_token);
                                            self.push_token(t);
                                        }
                                    }
                                } else {
                                    self.push_token(t);
                                }
                            }
                            _ => {
                                self.push_token(t);
                            }
                        };
                    } else if name == "from" {
                        // Combine yield from to one token ...

                        if let Some(previous_token) = self.tokens.last() {
                            if previous_token.t == TokenType::Yield {
                                self.tokens.pop();
                                self.push_token(TokenType::YieldFrom);
                            } else {
                                self.push_named_token(TokenType::Identifier, &name);
                            }
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
        while let Some(' ') = self.chars.peek() {
            self.advance();
        }

        let marker = match self.chars.peek() {
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

        while let Some(&c) = self.chars.peek() {
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
                break;
            }
        }

        if self.chars.peek().is_none() {
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

        while let Some(&c) = self.chars.peek() {
            if c >= 'a' && c <= 'z'
                || c >= 'A' && c <= 'Z'
                || c >= '0' && c <= '9'
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

        while let Some(&c) = self.chars.peek() {
            if c >= '0' && c <= '9' {
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

        while let Some(&c) = self.chars.peek() {
            if c >= '0' && c <= '9' || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F' {
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

        while let Some(&c) = self.chars.peek() {
            if c == '0' || c == '1' {
                number.push(c);
            } else {
                break;
            }
            self.advance();
        }

        number
    }

    fn advance(&mut self) -> Option<char> {
        self.pos += 1;

        if let Some(c) = self.chars.next() {
            if c == '\n' || c == '\r' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }

            return Some(c);
        }

        None
    }

    /// Advance until end of the file or until a specific character is seen
    fn advance_until_after(&mut self, until: char) {
        while let Some(c) = self.advance() {
            if c == until {
                break;
            }
        }
    }

    fn advance_until_after_multiline_comment(&mut self) {
        loop {
            self.advance_until_after('*');

            if let Some('/') = self.chars.peek() {
                break;
            }
        }
        self.advance();
    }

    fn push_token(&mut self, t: TokenType) {
        if self.context != Context::InScript {
            return;
        }
        self.tokens
            .push(Token::new(t, self.line as u16, self.start_of_token as u16));
    }

    fn push_named_token(&mut self, t: TokenType, name: &str) {
        self.tokens.push(Token::named(
            t,
            self.line as u16,
            self.start_of_token as u16,
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
            "__LINE__" => TokenType::ConstLine,
            "__FILE__" => TokenType::ConstFile,
            "__DIR__" => TokenType::ConstDir,
            "__CLASS__" => TokenType::ConstClass,
            "__TRAIT__" => TokenType::ConstTrait,
            "__METHOD__" => TokenType::ConstMethod,
            "__FUNCTION__" => TokenType::ConstFunction,
            "require" => TokenType::Require,
            "require_once" => TokenType::RequireOnce,
            "include" => TokenType::Include,
            "include_once" => TokenType::IncludeOnce,
            "instanceof" => TokenType::InstanceOf,
            "yield" => TokenType::Yield,
            "self" => TokenType::TypeSelf,
            _ => {
                return None;
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parses_expressions() {
        let mut scanner = Scanner::new("<?php\n$a = 1 + 2;\n$a++;$b +\n1\n + 2\n;");
        scanner.scan().unwrap();

        assert_eq!(scanner.tokens[0], Token::new(TokenType::ScriptStart, 1, 1));
        assert_eq!(
            scanner.tokens[1],
            Token::named(TokenType::Variable, 2, 1, "a")
        );
        assert_eq!(scanner.tokens[2], Token::new(TokenType::Assignment, 2, 4));
        assert_eq!(
            scanner.tokens[3],
            Token::named(TokenType::LongNumber, 2, 6, "1")
        );
        assert_eq!(scanner.tokens[4], Token::new(TokenType::Plus, 2, 8));
        assert_eq!(
            scanner.tokens[5],
            Token::named(TokenType::LongNumber, 2, 10, "2")
        );
        assert_eq!(scanner.tokens[6], Token::new(TokenType::Semicolon, 2, 11));
        assert_eq!(
            scanner.tokens[7],
            Token::named(TokenType::Variable, 3, 1, "a")
        );
        assert_eq!(scanner.tokens[8], Token::new(TokenType::Increment, 3, 3));
        assert_eq!(scanner.tokens[9], Token::new(TokenType::Semicolon, 3, 5));
        assert_eq!(
            scanner.tokens[10],
            Token::named(TokenType::Variable, 3, 6, "b")
        );
        assert_eq!(scanner.tokens[11], Token::new(TokenType::Plus, 3, 9));
        assert_eq!(
            scanner.tokens[12],
            Token::named(TokenType::LongNumber, 4, 1, "1")
        );
        assert_eq!(scanner.tokens[13], Token::new(TokenType::Plus, 5, 2));
        assert_eq!(
            scanner.tokens[14],
            Token::named(TokenType::LongNumber, 5, 4, "2")
        );
        assert_eq!(scanner.tokens[15], Token::new(TokenType::Semicolon, 6, 1));
    }

    #[test]
    fn test_parses_multibyte_string() {
        let mut scanner = Scanner::new(
            "<?php
$object->{'東京'} = 2020;
",
        );

        scanner.scan().unwrap();

        assert_eq!(scanner.tokens[0], Token::new(TokenType::ScriptStart, 1, 1));
        assert_eq!(
            scanner.tokens[1],
            Token::named(TokenType::Variable, 2, 1, "object")
        );
        assert_eq!(
            scanner.tokens[2],
            Token::new(TokenType::ObjectOperator, 2, 8)
        );
        assert_eq!(scanner.tokens[3], Token::new(TokenType::OpenCurly, 2, 10));
        assert_eq!(
            scanner.tokens[4],
            Token::named(TokenType::ConstantEncapsedString, 2, 11, "東京")
        );
    }

    #[test]
    fn test_parses_empty_string_and_not_empty_string() {
        let mut scanner = Scanner::new(
            "<?php
'abc' . '' . 'def';
",
        );

        scanner.scan().unwrap();
        assert_eq!(
            scanner.tokens[1],
            Token::named(TokenType::ConstantEncapsedString, 2, 1, "abc")
        );
        assert_eq!(scanner.tokens[2], Token::new(TokenType::Concat, 2, 7));
        assert_eq!(
            scanner.tokens[3],
            Token::named(TokenType::ConstantEncapsedString, 2, 9, "")
        );
        assert_eq!(scanner.tokens[4], Token::new(TokenType::Concat, 2, 12));

        assert_eq!(
            scanner.tokens[5],
            Token::named(TokenType::ConstantEncapsedString, 2, 14, "def")
        );
    }

    #[test]
    fn test_parses_array() {
        let mut scanner = Scanner::new(
            "<?php
['rofl' => 'copter'];",
        );

        scanner.scan().unwrap();
        assert_eq!(scanner.tokens[1], Token::new(TokenType::OpenBrackets, 2, 1));
        assert_eq!(
            scanner.tokens[2],
            Token::named(TokenType::ConstantEncapsedString, 2, 2, "rofl")
        );
        assert_eq!(scanner.tokens[3], Token::new(TokenType::DoubleArrow, 2, 9));
        assert_eq!(
            scanner.tokens[4],
            Token::named(TokenType::ConstantEncapsedString, 2, 12, "copter")
        );
        assert_eq!(
            scanner.tokens[5],
            Token::new(TokenType::CloseBrackets, 2, 20)
        );
    }

    #[test]
    fn test_parses_string_argument() {
        let mut scanner = Scanner::new(
            "<?php
func('rofl');",
        );

        scanner.scan().unwrap();
        assert_eq!(
            scanner.tokens[1],
            Token::named(TokenType::Identifier, 2, 1, "func")
        );
        assert_eq!(
            scanner.tokens[2],
            Token::new(TokenType::OpenParenthesis, 2, 5)
        );
        assert_eq!(
            scanner.tokens[3],
            Token::named(TokenType::ConstantEncapsedString, 2, 6, "rofl")
        );
        assert_eq!(
            scanner.tokens[4],
            Token::new(TokenType::CloseParenthesis, 2, 12)
        );
        assert_eq!(scanner.tokens[5], Token::new(TokenType::Semicolon, 2, 13));
    }

    #[test]
    fn test_parses_keywords() {
        let mut scanner = Scanner::new(
            "<?php
while (true) {}",
        );

        scanner.scan().unwrap();

        assert_eq!(scanner.tokens[1], Token::new(TokenType::While, 2, 1));
        assert_eq!(
            scanner.tokens[2],
            Token::new(TokenType::OpenParenthesis, 2, 7)
        );
        assert_eq!(scanner.tokens[3], Token::new(TokenType::True, 2, 8));
        assert_eq!(
            scanner.tokens[4],
            Token::new(TokenType::CloseParenthesis, 2, 12)
        );
        assert_eq!(scanner.tokens[5], Token::new(TokenType::OpenCurly, 2, 14));
        assert_eq!(scanner.tokens[6], Token::new(TokenType::CloseCurly, 2, 15));
    }

    #[test]
    fn test_parses_for_loop() {
        let mut scanner = Scanner::new(
            "<?php
for ($i = 0; $i < 100; $i++) {}",
        );

        scanner.scan().unwrap();

        assert_eq!(scanner.tokens[1], Token::new(TokenType::For, 2, 1));
        assert_eq!(
            scanner.tokens[2],
            Token::new(TokenType::OpenParenthesis, 2, 5)
        );
        assert_eq!(
            scanner.tokens[3],
            Token::named(TokenType::Variable, 2, 6, "i")
        );
        assert_eq!(scanner.tokens[4], Token::new(TokenType::Assignment, 2, 9));
        assert_eq!(
            scanner.tokens[5],
            Token::named(TokenType::LongNumber, 2, 11, "0")
        );
        assert_eq!(scanner.tokens[6], Token::new(TokenType::Semicolon, 2, 12));

        assert_eq!(
            scanner.tokens[7],
            Token::named(TokenType::Variable, 2, 14, "i")
        );
        assert_eq!(scanner.tokens[8], Token::new(TokenType::Smaller, 2, 17));
        assert_eq!(
            scanner.tokens[9],
            Token::named(TokenType::LongNumber, 2, 19, "100")
        );
        assert_eq!(scanner.tokens[10], Token::new(TokenType::Semicolon, 2, 22));

        assert_eq!(
            scanner.tokens[11],
            Token::named(TokenType::Variable, 2, 24, "i")
        );
        assert_eq!(scanner.tokens[12], Token::new(TokenType::Increment, 2, 26));
        assert_eq!(
            scanner.tokens[13],
            Token::new(TokenType::CloseParenthesis, 2, 28)
        );

        assert_eq!(scanner.tokens[14], Token::new(TokenType::OpenCurly, 2, 30));
        assert_eq!(scanner.tokens[15], Token::new(TokenType::CloseCurly, 2, 31));
    }
}
