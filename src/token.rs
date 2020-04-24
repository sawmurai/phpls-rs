use tower_lsp::lsp_types::{DocumentSymbol, Position, Range, SymbolKind};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    Eof,

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
    IsEqual,
    GreaterOrEqual,
    SmallerOrEqual,
    Coalesce,
    BinaryAndAssignment,
    BinaryOrAssignment,
    ObjectOperator,
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
    ScriptStart,

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
    Empty,
    HaltCompiler,
    Class,
    Trait,
    Interface,
    Extends,
    Implements,
    List,
    Array,
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

    // Types
    TypeBool,
    TypeInt,
    TypeString,
    TypeArray,
    TypeObject,
    TypeFloat,
    TypeSelf,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub col: u16,
    pub line: u16,
    pub t: TokenType,
    pub label: Option<String>,
}

impl Token {
    pub fn new(t: TokenType, line: u16, col: u16) -> Self {
        Token {
            t,
            col,
            line,
            label: None,
        }
    }

    pub fn named(t: TokenType, line: u16, col: u16, label: &str) -> Self {
        Token {
            t,
            col,
            line,
            label: Some(label.to_owned()),
        }
    }

    pub fn is_identifier(&self) -> bool {
        match self.t {
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
            | TokenType::Empty
            | TokenType::HaltCompiler
            | TokenType::Class
            | TokenType::Trait
            | TokenType::Interface
            | TokenType::Extends
            | TokenType::Implements
            | TokenType::List
            | TokenType::Array
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
                // Types
            | TokenType::TypeBool
            | TokenType::TypeString
            | TokenType::TypeInt
            | TokenType::TypeArray
            | TokenType::TypeObject
            | TokenType::TypeFloat
            | TokenType::TypeSelf
            | TokenType::Print
            | TokenType::Identifier => true,
            _ => false,
        }
    }

    pub fn start(&self) -> (u16, u16) {
        (self.line, self.col)
    }

    pub fn end(&self) -> (u16, u16) {
        (self.line, self.col + self.len())
    }

    pub fn range(&self) -> ((u16, u16), (u16, u16)) {
        (self.start(), self.end())
    }

    pub fn len(&self) -> u16 {
        return 1;
    }

    pub fn is_on(&self, line: u16, col: u16) -> bool {
        if self.line != line {
            return false;
        }

        return col >= self.col
            && col <= (self.col + self.label.clone().unwrap_or_default().len() as u16);
    }
}

impl From<&Token> for DocumentSymbol {
    fn from(token: &Token) -> DocumentSymbol {
        let start = token.start();
        let end = token.end();

        let range = Range {
            start: Position {
                line: start.0 as u64,
                character: start.1 as u64,
            },
            end: Position {
                line: end.0 as u64,
                character: end.1 as u64,
            },
        };

        DocumentSymbol {
            name: token.clone().label.unwrap(),
            kind: SymbolKind::Variable,
            range: range,
            selection_range: range,
            detail: None,
            children: None,
            deprecated: None,
        }
    }
}
