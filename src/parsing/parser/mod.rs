pub mod expression;
pub mod item;
pub mod pattern;
pub mod statement;

use nom::combinator::map;
use nom::error::ErrorKind;
use nom::Err;
use nom::IResult;

use self::expression::Expression;
use self::pattern::Pattern;
use crate::parsing::lexer::{Token, TokenType};

type TokenSlice<'a> = &'a [Token<'a>];

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub lhs: Pattern,
    pub rhs: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TryStatement(Expression);

pub enum ImportStatement {
    ModuleLevel {
        path_segments: Vec<String>,
    },
    ItemLevel {
        path_segments: Vec<String>,
        items: Vec<String>,
    },
}

const TAB_SIZE: usize = 4;

fn tag<'a>(test: TokenType<'a>) -> impl Fn(TokenSlice) -> IResult<TokenSlice, &'a Token> {
    move |stream| {
        if let Some((a, rem)) = stream.split_first() {
            if test == a.ty {
                Ok((rem, a))
            } else {
                Result::Err(Err::Error((stream, ErrorKind::Tag)))
            }
        }
        else {
            Result::Err(Err::Error((stream, ErrorKind::Tag)))
        }
    }
}

fn test<'a, F>(test: F) -> impl Fn(&'a [Token]) -> IResult<&'a [Token<'a>], &'a Token<'a>>
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
