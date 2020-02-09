pub mod expression;
pub mod item;
pub mod pattern;
pub mod statement;

use nom::combinator::map;
use nom::error::ErrorKind;
use nom::Err;
use nom::IResult;

use crate::parsing::lexer::{Token, TokenType};

type TokenSlice<'a> = &'a [Token<'a>];

pub struct Position {
    pub line: usize,
    pub column: usize,
}

pub struct Parsed<T> {
    pub start_pos: Position,
    pub end_pos: Position,
    pub data: T,
}

fn tag(test: TokenType<'static>) -> impl Fn(TokenSlice) -> IResult<TokenSlice, &Token> {
    move |stream| {
        if let Some((a, rem)) = stream.split_first() {
            if test == a.ty {
                Ok((rem, a))
            } else {
                Result::Err(Err::Error((stream, ErrorKind::Tag)))
            }
        } else {
            Result::Err(Err::Error((stream, ErrorKind::Tag)))
        }
    }
}

fn test<'a, F>(test: F) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, &'a Token<'a>>
where
    F: Fn(&'a TokenType<'a>) -> bool,
{
    move |stream: &'a [Token]| match stream.split_first() {
        None => Result::Err(Err::Error((stream, ErrorKind::Tag))),
        Some((a, rem)) if test(&a.ty) => Ok((rem, a)),
        _ => Result::Err(Err::Error((stream, ErrorKind::Tag))),
    }
}

fn extract_identifier<'a>(stream: &'a [Token<'a>]) -> IResult<&'a [Token<'a>], String> {
    map(test(TokenType::is_ident), |tok| match tok.ty {
        TokenType::Ident(s) => s.to_string(),
        _ => unreachable!(),
    })(stream)
}
