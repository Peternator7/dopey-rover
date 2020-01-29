#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub enum Token {
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

    Set,
    Get,

    Ident(String),
    Number(f32),
    StringLiteral(String),
    Indented(usize),

    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,

    AndAnd,
    OrOr,

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

impl Token {
    generate_is_a_!(is_indented, Token::Indented(_));
    generate_is_a_!(is_ident, Token::Ident(_));
    generate_is_a_!(is_number, Token::Number(_));
    generate_is_a_!(is_string_literal, Token::StringLiteral(_));
}
