pub mod expression;

use nom::combinator::map;
use nom::error::ErrorKind;
use nom::Err;
use nom::IResult;

use crate::parsing::lexer::Token;

const TAB_SIZE: usize = 4;

fn tag<'a>(test: Token) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], &'a Token> {
    move |stream: &'a [Token]| match stream.split_first() {
        None => Result::Err(Err::Error((stream, ErrorKind::Tag))),
        Some((a, rem)) if test == *a => Ok((rem, a)),
        _ => Result::Err(Err::Error((stream, ErrorKind::Tag))),
    }
}

pub fn test<'a, F>(test: F) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], &'a Token>
where
    F: Fn(&'a Token) -> bool,
{
    move |stream: &'a [Token]| match stream.split_first() {
        None => Result::Err(Err::Error((stream, ErrorKind::Tag))),
        Some((a, rem)) if test(a) => Ok((rem, a)),
        _ => Result::Err(Err::Error((stream, ErrorKind::Tag))),
    }
}

pub fn extract_identifier<'a>(stream: &'a [Token]) -> IResult<&[Token], String> {
    map(test(Token::is_ident), |tok| match tok {
        Token::Ident(s) => s.clone(),
        _ => unreachable!(),
    })(stream)
}
