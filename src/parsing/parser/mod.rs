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
use crate::parsing::lexer::Token;

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

fn tag<'a>(test: Token) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], &'a Token> {
    move |stream: &'a [Token]| match stream.split_first() {
        None => Result::Err(Err::Error((stream, ErrorKind::Tag))),
        Some((a, rem)) if test == *a => Ok((rem, a)),
        _ => Result::Err(Err::Error((stream, ErrorKind::Tag))),
    }
}

fn test<'a, F>(test: F) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], &'a Token>
where
    F: Fn(&'a Token) -> bool,
{
    move |stream: &'a [Token]| match stream.split_first() {
        None => Result::Err(Err::Error((stream, ErrorKind::Tag))),
        Some((a, rem)) if test(a) => Ok((rem, a)),
        _ => Result::Err(Err::Error((stream, ErrorKind::Tag))),
    }
}

fn extract_identifier(stream: &[Token]) -> IResult<&[Token], String> {
    map(test(Token::is_ident), |tok| match tok {
        Token::Ident(s) => s.clone(),
        _ => unreachable!(),
    })(stream)
}
