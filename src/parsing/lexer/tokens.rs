use crate::parsing::Position;

#[derive(Debug, PartialEq, Clone)]
pub struct Token<'a> {
    pub start_pos: Position,
    pub end_pos: Position,
    pub ty: TokenType<'a>,
}

impl<'a> Token<'a> {
    pub fn new<'b>(start_pos: Position, end_pos: Position, ty: TokenType<'b>) -> Token<'b> {
        Token {
            start_pos,
            end_pos,
            ty,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenType<'a> {
    Function,
    Trait,
    If,
    Else,
    Import,
    From,
    With,
    New,
    Is,
    Return,
    Match,
    Try,
    And,
    Or,

    Set,
    Get,

    Ident(&'a str),
    Number(f32),
    StringLiteral(&'a str),
    // Indented(usize),
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,

    QuestionMark,

    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    EqualEquals,
    NotEquals,

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,

    Period,
    Comma,
    DoubleColon,
    Colon,
    SemiColon,
    Exclamation,
    Pipe,

    Equals,
}

macro_rules! generate_is_a_ {
    ($name:tt, $variant:pat) => {
        pub fn $name(&self) -> bool {
            match self {
                $variant => true,
                _ => false
            }
        }
    };
}

impl<'a> TokenType<'a> {
    // generate_is_a_!(is_indented, TokenType::Indented(_));
    generate_is_a_!(is_ident, TokenType::Ident(_));
    generate_is_a_!(is_number, TokenType::Number(_));
    generate_is_a_!(is_string_literal, TokenType::StringLiteral(_));
}
