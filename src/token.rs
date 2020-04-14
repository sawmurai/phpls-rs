#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
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
    Xor,

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
    ReferenceAssignment,

    // Three chars
    RightShiftAssignment,
    LeftShiftAssignment,
    IsNotIdentical,
    IsIdentical,
    PowerAssignment,
    SpaceShip,
    CoalesceAssignment,
    Elipsis,

    // Four chars

    // Five chars
    ScriptStart,

    // Variable length
    Identifier,
    DecimalNumber,
    LongNumber,
    HexNumber,
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
    ElseIf,
    Else,
    EndIf,
    Echo,
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
    From,

    // Types
    TypeBool,
    TypeInt,
    TypeString,
    TypeArray,
    TypeObject,
    TypeFloat,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub col: u16,
    pub line: u16,
    pub t: TokenType,
    label: Option<String>,
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
        if self.label.is_some() {
            return true;
        }

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
            | TokenType::New
            | TokenType::Clone
            | TokenType::True
            | TokenType::False
            | TokenType::Null
            | TokenType::Namespace
            | TokenType::Void
            | TokenType::Yield
            | TokenType::From => true,
            _ => false,
        }
    }
}
/*
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.t)
    }
}*/
