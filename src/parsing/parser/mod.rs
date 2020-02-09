pub mod expression;
pub mod item;
pub mod pattern;
pub mod statement;

use nom::combinator::map;
use nom::error::ErrorKind;
use nom::Err;
use nom::IResult;

use super::lexer::{Token, TokenType};
use super::Position;

type TokenSlice<'a> = &'a [Token<'a>];

pub struct Parsed<T> {
    pub start_pos: Position,
    pub end_pos: Option<Position>,
    pub data: T,
}

impl<T> Parsed<T> {
    pub fn new(data: T, start_pos: Position) -> Parsed<T> {
        Parsed {
            data,
            start_pos,
            end_pos: None,
        }
    }

    pub fn with_end(data: T, start_pos: Position, end_pos: Position) -> Parsed<T> {
        Parsed {
            data,
            start_pos,
            end_pos: Some(end_pos),
        }
    }
}

fn tag(test: TokenType<'static>) -> impl Fn(TokenSlice) -> IResult<TokenSlice, &Token> {
    move |stream| match stream.split_first() {
        Some((a, rem)) if test == a.ty => Ok((rem, a)),
        _ => Result::Err(Err::Error((stream, ErrorKind::Tag))),
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
