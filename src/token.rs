#[derive(Debug)]
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
    DynamicVariable,

    // Three chars
    RightShiftAssignment,
    LeftShiftAssignment,
    IsNotIdentical,
    IsIdentical,
    PowerAssignment,

    // Four chars

    // Five chars
    ScriptStart,
}

#[derive(Debug)]
pub struct Token {
    t: TokenType,
    col: u16,
    line: u16,
    label: Option<String>,
}

impl Token {
    pub fn new(t: TokenType, col: u16, line: u16, label: Option<String>) -> Self {
        Token {
            t,
            col,
            line,
            label,
        }
    }
}
