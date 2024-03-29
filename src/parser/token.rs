use std::fmt::{Display, Formatter, Result};

use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

use super::node::NodeRange;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScriptStartType {
    Regular,
    Short,
    Echo,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Eof,

    Linebreak,

    // Special tokentype that is used when the expected tokentype was not found
    Missing,

    // One char
    Plus,
    Minus,
    Multiplication,
    Division,
    Greater,
    Smaller,
    QuestionMark,
    ExclamationMark,
    Assignment,
    Variable,
    OpenParenthesis,
    CloseParenthesis,
    OpenCurly,
    CloseCurly,
    OpenBrackets,
    CloseBrackets,
    Semicolon,
    LogicOr,
    LogicAnd,
    BinaryOr,
    BinaryAnd,
    Negation,
    Colon,
    Comma,
    NamespaceSeparator,
    Concat,
    Silencer,
    BitwiseNegation,
    Modulo,
    BinaryXor,

    // Two chars
    Increment,
    Decrement,
    Power,
    PlusAssign,
    MinusAssign,
    MulAssign,
    DivAssign,
    LineComment,
    MultilineComment,
    RightShift,
    LeftShift,
    ScriptEnd,
    IsNotEqual,
    IsNotEqualAlt,
    IsEqual,
    GreaterOrEqual,
    SmallerOrEqual,
    Coalesce,
    BinaryAndAssignment,
    BinaryOrAssignment,
    ObjectOperator,
    NullsafeObjectOperator,
    ModuloAssignment,
    ConcatAssignment,
    XorAssignment,
    PaamayimNekudayim,
    DoubleArrow,

    // Three chars
    RightShiftAssignment,
    LeftShiftAssignment,
    IsNotIdentical,
    IsIdentical,
    PowerAssignment,
    SpaceShip,
    CoalesceAssignment,
    Elipsis,
    LogicXor,
    ConstNan,
    ConstInf,
    // Four chars

    // Five chars
    ScriptStart(ScriptStartType),

    // Variable length
    Identifier,
    DecimalNumber,
    ExponentialNumber,
    LongNumber,
    HexNumber,
    BinaryNumber,
    ConstantEncapsedString,
    EncapsedAndWhitespaceString,
    ShellEscape,
    HereDocStart,
    HereDocEnd,
    BoolCast,
    BadCast, // For unknown casts
    IntCast,
    StringCast,
    ArrayCast,
    ObjectCast,
    DoubleCast,
    UnsetCast,
    Generator,
    Resource,

    // Keywords
    Exit,
    If,
    Die,
    ElseIf,
    Else,
    EndIf,
    Echo,
    Print,
    Include,
    IncludeOnce,
    Require,
    RequireOnce,
    Do,
    While,
    EndWhile,
    For,
    EndFor,
    Foreach,
    EndForeach,
    Declare,
    EndDeclare,
    As,
    Match,
    Switch,
    EndSwitch,
    Case,
    Default,
    Break,
    Continue,
    Goto,
    Function,
    Fn,
    Const,
    Return,
    Try,
    Catch,
    Finally,
    Throw,
    Use,
    Insteadof,
    InstanceOf,
    Global,
    Static,
    Abstract,
    Final,
    Private,
    Protected,
    Public,
    Var,
    Unset,
    Isset,
    Define,
    Empty,
    HaltCompiler,
    Class,
    Trait,
    Interface,
    Extends,
    Implements,
    List,
    Callable,
    ConstLine,
    ConstFile,
    ConstDir,
    ConstClass,
    ConstTrait,
    ConstMethod,
    ConstFunction,
    New,
    Clone,
    True,
    False,
    Null,
    Namespace,
    Void,
    Yield,
    YieldFrom,
    Parent,

    // Types
    TypeBool,
    TypeInt,
    TypeString,
    TypeArray,
    TypeObject,
    TypeFloat,
    TypeSelf,
    Mixed,

    AttributeStart,
}

impl TokenType {
    pub fn postfix_binding_power(&self) -> Option<u8> {
        match self {
            TokenType::Increment | TokenType::Decrement => Some(180),
            _ => None,
        }
    }

    pub fn prefix_binding_power(&self) -> Option<u8> {
        match self {
            TokenType::BinaryAnd => Some(202),
            TokenType::Clone => Some(201),
            TokenType::New => Some(201),
            TokenType::Increment | TokenType::Decrement => Some(180),
            TokenType::Plus | TokenType::Minus => Some(140),
            TokenType::Silencer
            | TokenType::Negation
            | TokenType::BitwiseNegation
            | TokenType::BoolCast
            | TokenType::BadCast
            | TokenType::IntCast
            | TokenType::StringCast
            | TokenType::ArrayCast
            | TokenType::ObjectCast
            | TokenType::DoubleCast
            | TokenType::Elipsis
            | TokenType::UnsetCast => Some(181),
            _ => None,
        }
    }

    // Via https://www.php.net/manual/en/language.operators.precedence.php
    pub fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let bp = match self {
            TokenType::Power => (190, 191),

            TokenType::InstanceOf => (171, 170),

            // Expressions
            TokenType::Modulo | TokenType::Multiplication | TokenType::Division => (151, 150),

            TokenType::Plus | TokenType::Minus | TokenType::Concat => (141, 140),

            TokenType::RightShift | TokenType::LeftShift => (131, 130),

            TokenType::Greater
            | TokenType::Smaller
            | TokenType::GreaterOrEqual
            | TokenType::SmallerOrEqual => (120, 121),

            TokenType::IsNotIdentical
            | TokenType::IsIdentical
            | TokenType::SpaceShip
            | TokenType::IsEqual
            | TokenType::IsNotEqual
            | TokenType::IsNotEqualAlt => (110, 111),

            TokenType::BinaryAnd => (101, 100),
            TokenType::BinaryXor => (99, 98),
            TokenType::BinaryOr => (97, 96),

            TokenType::LogicAnd => (91, 90),
            TokenType::LogicOr => (87, 86),
            TokenType::Coalesce => (84, 85),

            // Ternary
            TokenType::QuestionMark => (80, 81),

            // Modification
            TokenType::Assignment
            | TokenType::BinaryAndAssignment
            | TokenType::BinaryOrAssignment
            | TokenType::ModuloAssignment
            | TokenType::ConcatAssignment
            | TokenType::XorAssignment
            | TokenType::RightShiftAssignment
            | TokenType::LeftShiftAssignment
            | TokenType::CoalesceAssignment
            | TokenType::PowerAssignment
            | TokenType::PlusAssign
            | TokenType::MinusAssign
            | TokenType::MulAssign
            | TokenType::DivAssign => (70, 71),

            TokenType::YieldFrom => (60, 61),
            TokenType::Yield => (50, 51),
            TokenType::Print => (40, 41),

            // Logic
            TokenType::LogicXor => (21, 20),

            _ => return None,
        };

        Some(bp)
    }

    pub fn is_infix_operator(&self) -> bool {
        self.infix_binding_power().is_some()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub col: u32,
    pub line: u32,
    pub t: TokenType,
    pub label: Option<String>,
    pub offset: Option<usize>,
}

impl Token {
    pub fn new(t: TokenType, line: u32, col: u32, offset: usize) -> Self {
        Token {
            t,
            col,
            line,
            label: None,
            offset: Some(offset),
        }
    }

    pub fn named(t: TokenType, line: u32, col: u32, offset: usize, label: &str) -> Self {
        Token {
            t,
            col,
            line,
            label: Some(label.to_owned()),
            offset: Some(offset),
        }
    }

    pub fn missing(pos_of_prev: (u32, u32)) -> Self {
        Token {
            t: TokenType::Missing,
            line: pos_of_prev.1,
            col: pos_of_prev.0,
            label: None,
            offset: None,
        }
    }

    pub fn is_identifier(&self) -> bool {
        matches!(
            self.t,
            TokenType::Exit
            | TokenType::If
            | TokenType::ElseIf
            | TokenType::Else
            | TokenType::EndIf
            | TokenType::Echo
            | TokenType::Include
            | TokenType::IncludeOnce
            | TokenType::Require
            | TokenType::RequireOnce
            | TokenType::Do
            | TokenType::While
            | TokenType::EndWhile
            | TokenType::For
            | TokenType::EndFor
            | TokenType::Foreach
            | TokenType::EndForeach
            | TokenType::Declare
            | TokenType::EndDeclare
            | TokenType::As
            | TokenType::Switch
            | TokenType::EndSwitch
            | TokenType::Case
            | TokenType::Default
            | TokenType::Break
            | TokenType::Continue
            | TokenType::Goto
            | TokenType::Function
            | TokenType::Fn
            | TokenType::Const
            | TokenType::Return
            | TokenType::Try
            | TokenType::Catch
            | TokenType::Finally
            | TokenType::Throw
            | TokenType::Use
            | TokenType::Insteadof
            | TokenType::InstanceOf
            | TokenType::Global
            | TokenType::Static
            | TokenType::Abstract
            | TokenType::Final
            | TokenType::Private
            | TokenType::Protected
            | TokenType::Public
            | TokenType::Var
            | TokenType::Unset
            | TokenType::Isset
            | TokenType::Define
            | TokenType::Empty
            | TokenType::HaltCompiler
            | TokenType::Class
            | TokenType::Trait
            | TokenType::Interface
            | TokenType::Extends
            | TokenType::Implements
            | TokenType::List
            | TokenType::Callable
            | TokenType::ConstLine
            | TokenType::ConstFile
            | TokenType::ConstDir
            | TokenType::ConstClass
            | TokenType::ConstTrait
            | TokenType::ConstMethod
            | TokenType::ConstFunction
            | TokenType::ConstNan
            | TokenType::ConstInf
            | TokenType::New
            | TokenType::Die
            | TokenType::Clone
            | TokenType::True
            | TokenType::False
            | TokenType::Null
            | TokenType::Namespace
            | TokenType::Void
            | TokenType::Yield
            | TokenType::Parent
                // Types
            | TokenType::TypeBool
            | TokenType::TypeString
            | TokenType::TypeInt
            | TokenType::TypeArray
            | TokenType::TypeObject
            | TokenType::TypeFloat
            | TokenType::TypeSelf
            | TokenType::Print
            | TokenType::LogicAnd
            | TokenType::LogicOr
            | TokenType::LogicXor
            | TokenType::Identifier
            | TokenType::Generator
            | TokenType::Match
        )
    }

    pub fn is_comment(&self) -> bool {
        matches!(self.t, TokenType::LineComment | TokenType::MultilineComment)
    }

    pub fn start(&self) -> (u32, u32) {
        (self.col, self.line)
    }

    pub fn end(&self) -> (u32, u32) {
        let me = self.to_string();

        if self.label.is_some() {
            let lines = me.lines().collect::<Vec<&str>>();

            if lines.len() == 1 {
                (self.col + me.len() as u32, self.line)
            } else {
                (
                    lines.last().unwrap().len() as _,
                    self.line + (lines.len() - 1) as u32,
                )
            }
        } else {
            (self.col + me.len() as u32, self.line)
        }
    }

    pub fn range(&self) -> NodeRange {
        (self.start(), self.end()).into()
    }

    pub fn is_on(&self, line: u32, col: u32) -> bool {
        if self.line != line {
            return false;
        }

        col >= self.col && col <= (self.col + self.label.clone().unwrap_or_default().len() as u32)
    }

    pub fn is_string(&self) -> bool {
        matches!(
            self.t,
            TokenType::ConstantEncapsedString | TokenType::EncapsedAndWhitespaceString
        )
    }

    pub fn is_number(&self) -> bool {
        matches!(
            self.t,
            TokenType::BinaryNumber
                | TokenType::DecimalNumber
                | TokenType::ExponentialNumber
                | TokenType::HexNumber
                | TokenType::LongNumber
        )
    }

    pub fn gap_to(&self, next: &Token) -> (u32, u32) {
        if self.line == next.line {
            (next.start().1 - self.end().1, 0)
        } else {
            (next.start().1, next.start().0 - self.end().0)
        }
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let dis = match self {
            TokenType::Resource => "resource".to_owned(),
            TokenType::AttributeStart => "#[".to_owned(),
            TokenType::Class => "class".to_owned(),
            TokenType::Eof => "".to_owned(),
            TokenType::Plus => "+".to_owned(),
            TokenType::Minus => "-".to_owned(),
            TokenType::Multiplication => "*".to_owned(),
            TokenType::Division => "/".to_owned(),
            TokenType::Greater => ">".to_owned(),
            TokenType::Smaller => "<".to_owned(),
            TokenType::QuestionMark => "?".to_owned(),
            TokenType::ExclamationMark => "!".to_owned(),
            TokenType::Assignment => "=".to_owned(),
            TokenType::OpenParenthesis => "(".to_owned(),
            TokenType::CloseParenthesis => ")".to_owned(),
            TokenType::OpenCurly => "{".to_owned(),
            TokenType::CloseCurly => "}".to_owned(),
            TokenType::OpenBrackets => "[".to_owned(),
            TokenType::CloseBrackets => "]".to_owned(),
            TokenType::Semicolon => ";".to_owned(),
            TokenType::LogicOr => "||".to_owned(),
            TokenType::LogicAnd => "&&".to_owned(),
            TokenType::BinaryOr => "|".to_owned(),
            TokenType::BinaryAnd => "&".to_owned(),
            TokenType::Negation => "!".to_owned(),
            TokenType::Colon => ":".to_owned(),
            TokenType::Comma => ",".to_owned(),
            TokenType::NamespaceSeparator => "\\".to_owned(),
            TokenType::Concat => ".".to_owned(),
            TokenType::Silencer => "@".to_owned(),
            TokenType::BitwiseNegation => "~".to_owned(),
            TokenType::Modulo => "%".to_owned(),
            TokenType::BinaryXor => "^".to_owned(),
            TokenType::Increment => "++".to_owned(),
            TokenType::Decrement => "--".to_owned(),
            TokenType::Power => "**".to_owned(),
            TokenType::PlusAssign => "+=".to_owned(),
            TokenType::MinusAssign => "-=".to_owned(),
            TokenType::MulAssign => "*=".to_owned(),
            TokenType::DivAssign => "/=".to_owned(),
            TokenType::LineComment => "//".to_owned(),
            TokenType::RightShift => ">>".to_owned(),
            TokenType::LeftShift => "<<".to_owned(),
            TokenType::ScriptEnd => "?>".to_owned(),
            TokenType::IsNotEqual => "!=".to_owned(),
            TokenType::IsNotEqualAlt => "<>".to_owned(),
            TokenType::IsEqual => "==".to_owned(),
            TokenType::GreaterOrEqual => ">=".to_owned(),
            TokenType::SmallerOrEqual => "<=".to_owned(),
            TokenType::Coalesce => "??".to_owned(),
            TokenType::BinaryAndAssignment => "&=".to_owned(),
            TokenType::BinaryOrAssignment => "|=".to_owned(),
            TokenType::ObjectOperator => "->".to_owned(),
            TokenType::NullsafeObjectOperator => "?->".to_owned(),
            TokenType::ModuloAssignment => "%=".to_owned(),
            TokenType::ConcatAssignment => ".=".to_owned(),
            TokenType::XorAssignment => "^=".to_owned(),
            TokenType::PaamayimNekudayim => "::".to_owned(),
            TokenType::DoubleArrow => "=>".to_owned(),
            TokenType::RightShiftAssignment => ">>=".to_owned(),
            TokenType::LeftShiftAssignment => "<<=".to_owned(),
            TokenType::IsNotIdentical => "!==".to_owned(),
            TokenType::IsIdentical => "===".to_owned(),
            TokenType::PowerAssignment => "**=".to_owned(),
            TokenType::SpaceShip => "<=>".to_owned(),
            TokenType::CoalesceAssignment => "??=".to_owned(),
            TokenType::Elipsis => "...".to_owned(),
            TokenType::LogicXor => "xor".to_owned(),
            TokenType::ConstNan => "Nan".to_owned(),
            TokenType::ConstInf => "Inf".to_owned(),
            TokenType::ScriptStart(ScriptStartType::Short) => "<?".to_owned(),
            TokenType::ScriptStart(ScriptStartType::Regular) => "<?php".to_owned(),
            TokenType::ScriptStart(ScriptStartType::Echo) => "<?=".to_owned(),
            TokenType::BoolCast => "(bool)".to_owned(),
            TokenType::BadCast => "".to_owned(),
            TokenType::IntCast => "(int)".to_owned(),
            TokenType::StringCast => "(string)".to_owned(),
            TokenType::ArrayCast => "(array)".to_owned(),
            TokenType::ObjectCast => "(object)".to_owned(),
            TokenType::DoubleCast => "(double)".to_owned(),
            TokenType::UnsetCast => "(unset)".to_owned(),
            TokenType::Exit => "exit".to_owned(),
            TokenType::If => "if".to_owned(),
            TokenType::Die => "die".to_owned(),
            TokenType::ElseIf => "elseif".to_owned(),
            TokenType::Else => "else".to_owned(),
            TokenType::EndIf => "endif".to_owned(),
            TokenType::Echo => "echo".to_owned(),
            TokenType::Print => "print".to_owned(),
            TokenType::Include => "include".to_owned(),
            TokenType::IncludeOnce => "include_once".to_owned(),
            TokenType::Require => "require".to_owned(),
            TokenType::RequireOnce => "require_once".to_owned(),
            TokenType::Do => "do".to_owned(),
            TokenType::While => "while".to_owned(),
            TokenType::EndWhile => "endwhile".to_owned(),
            TokenType::For => "for".to_owned(),
            TokenType::EndFor => "endfor".to_owned(),
            TokenType::Foreach => "foreach".to_owned(),
            TokenType::EndForeach => "endforeach".to_owned(),
            TokenType::Declare => "declare".to_owned(),
            TokenType::EndDeclare => "enddeclare".to_owned(),
            TokenType::As => "as".to_owned(),
            TokenType::Switch => "switch".to_owned(),
            TokenType::EndSwitch => "endswitch".to_owned(),
            TokenType::Case => "case".to_owned(),
            TokenType::Default => "default".to_owned(),
            TokenType::Break => "break".to_owned(),
            TokenType::Continue => "continue".to_owned(),
            TokenType::Goto => "goto".to_owned(),
            TokenType::Function => "function".to_owned(),
            TokenType::Fn => "fn".to_owned(),
            TokenType::Const => "const".to_owned(),
            TokenType::Return => "return".to_owned(),
            TokenType::Try => "try".to_owned(),
            TokenType::Catch => "catch".to_owned(),
            TokenType::Finally => "finally".to_owned(),
            TokenType::Throw => "throw".to_owned(),
            TokenType::Use => "use".to_owned(),
            TokenType::Insteadof => "insteadof".to_owned(),
            TokenType::InstanceOf => "instanceof".to_owned(),
            TokenType::Global => "global".to_owned(),
            TokenType::Static => "static".to_owned(),
            TokenType::Abstract => "abstract".to_owned(),
            TokenType::Final => "final".to_owned(),
            TokenType::Private => "private".to_owned(),
            TokenType::Protected => "protected".to_owned(),
            TokenType::Public => "public".to_owned(),
            TokenType::Var => "var".to_owned(),
            TokenType::Unset => "unset".to_owned(),
            TokenType::Isset => "isset".to_owned(),
            TokenType::Define => "define".to_owned(),
            TokenType::Empty => "empty".to_owned(),
            TokenType::HaltCompiler => "__halt_compiler".to_owned(),
            TokenType::Trait => "trait".to_owned(),
            TokenType::Interface => "interface".to_owned(),
            TokenType::Extends => "extends".to_owned(),
            TokenType::Implements => "implements".to_owned(),
            TokenType::List => "list".to_owned(),
            TokenType::Callable => "callable".to_owned(),
            TokenType::ConstLine => "__LINE__".to_owned(),
            TokenType::ConstFile => "__FILE__".to_owned(),
            TokenType::ConstDir => "__DIR__".to_owned(),
            TokenType::ConstClass => "__CLASS__".to_owned(),
            TokenType::ConstTrait => "__TRAIT__".to_owned(),
            TokenType::ConstMethod => "__METHOD__".to_owned(),
            TokenType::ConstFunction => "__FUNCTION__".to_owned(),
            TokenType::New => "new".to_owned(),
            TokenType::Clone => "clone".to_owned(),
            TokenType::True => "true".to_owned(),
            TokenType::False => "false".to_owned(),
            TokenType::Null => "null".to_owned(),
            TokenType::Namespace => "namespace".to_owned(),
            TokenType::Void => "void".to_owned(),
            TokenType::Yield => "yield".to_owned(),
            TokenType::YieldFrom => "yield from".to_owned(),
            TokenType::TypeBool => "bool".to_owned(),
            TokenType::TypeInt => "int".to_owned(),
            TokenType::TypeString => "string".to_owned(),
            TokenType::TypeArray => "array".to_owned(),
            TokenType::TypeObject => "object".to_owned(),
            TokenType::TypeFloat => "float".to_owned(),
            TokenType::TypeSelf => "self".to_owned(),
            TokenType::Mixed => "mixed".to_owned(),
            TokenType::Parent => "parent".to_owned(),
            TokenType::Generator => "Generator".to_owned(),
            TokenType::Missing => "".to_owned(),
            TokenType::Variable => "$".to_owned(),
            TokenType::Linebreak => "".to_owned(),
            TokenType::Match => "match".to_owned(),
            _ => unreachable!("Should have never been called with {:?}", self),
        };

        write!(f, "{}", dis)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.label.is_none() {
            write!(f, "{}", self.t)?;
            return Ok(());
        }

        let dis = match self.t {
            // Variables without a label are aliased variables like $$varname
            TokenType::LineComment => {
                format!("//{}", self.label.as_ref().unwrap_or(&String::from("")))
            }
            TokenType::Variable => format!("${}", self.label.as_ref().unwrap_or(&String::from(""))),
            TokenType::Identifier => self.label.as_ref().unwrap().to_string(),
            TokenType::MultilineComment => {
                format!("/*{}*/", self.label.as_ref().unwrap_or(&String::from("")))
            }
            TokenType::DecimalNumber
            | TokenType::ExponentialNumber
            | TokenType::LongNumber
            | TokenType::HexNumber
            | TokenType::BinaryNumber => self.label.as_ref().unwrap().to_string(),
            TokenType::ConstantEncapsedString => format!("'{}'", self.label.as_ref().unwrap()),
            TokenType::EncapsedAndWhitespaceString => {
                format!("\"{}\"", self.label.as_ref().unwrap())
            }
            TokenType::ShellEscape => format!("`{}`", self.label.as_ref().unwrap()),
            TokenType::HereDocStart => format!("<<<{}", self.label.as_ref().unwrap()),
            TokenType::HereDocEnd => self.label.as_ref().unwrap().to_string(),
            _ => format!("Should have never been called with {:?}", self),
        };

        write!(f, "{}", dis)
    }
}

impl From<TokenType> for CompletionItem {
    fn from(tt: TokenType) -> CompletionItem {
        CompletionItem {
            label: tt.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..CompletionItem::default()
        }
    }
}
