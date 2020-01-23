use nom::branch::alt;
use nom::combinator::map;
use nom::multi::separated_list;
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use super::{parse_top_level_expression, Expression, ParsedExpression};
use crate::parsing::lexer::Token;
use crate::parsing::parser::{extract_identifier, tag, test};

pub fn parse_basic_expression<'a>(stream: &'a [Token]) -> ParsedExpression<'a> {
    alt((
        parse_new_object,
        parse_string_literal,
        parse_number_literal,
        parse_ident_expression,
        parse_get_expression,
        parse_parens_expression,
    ))(stream)
}

pub fn parse_ident_expression<'a>(stream: &'a [Token]) -> ParsedExpression<'a> {
    map(extract_identifier, |ident| Expression::Variable(ident))(stream)
}

pub fn parse_string_literal<'a>(stream: &'a [Token]) -> ParsedExpression<'a> {
    map(test(Token::is_string_literal), |tok| match tok {
        Token::StringLiteral(s) => Expression::StringLiteral(s.clone()),
        _ => unreachable!(),
    })(stream)
}

pub fn parse_number_literal<'a>(stream: &'a [Token]) -> ParsedExpression<'a> {
    map(test(Token::is_number), |tok| match tok {
        Token::Number(f) => Expression::Number(*f),
        _ => unreachable!(),
    })(stream)
}

pub fn parse_get_expression(stream: &[Token]) -> ParsedExpression {
    map(
        tuple((
            tag(Token::Get),
            tag(Token::OpenParen),
            extract_identifier,
            tag(Token::Comma),
            parse_top_level_expression,
            tag(Token::CloseParen),
        )),
        |(_, _, ident, _, expr, _)| Expression::GetExpression(ident, Box::new(expr)),
    )(stream)
}

pub fn parse_parens_expression(stream: &[Token]) -> ParsedExpression {
    map(
        tuple((
            tag(Token::OpenParen),
            parse_top_level_expression,
            tag(Token::CloseParen),
        )),
        |(_, expr, _)| expr,
    )(stream)
}

pub fn parse_new_object(stream: &[Token]) -> ParsedExpression {
    map(
        tuple((
            tag(Token::New),
            tag(Token::OpenBrace),
            parse_object_property_list,
            tag(Token::CloseBrace),
        )),
        |(_, _, result, _)| result,
    )(stream)
}

pub fn parse_object_property_list(stream: &[Token]) -> ParsedExpression {
    map(
        separated_list(tag(Token::Comma), parse_object_property),
        |props| Expression::NewObject(props.len()),
    )(stream)
}

pub fn parse_object_property<'a>(
    stream: &'a [Token],
) -> IResult<&'a [Token], (String, Expression)> {
    map(
        tuple((
            test(Token::is_ident),
            tag(Token::Colon),
            parse_top_level_expression,
        )),
        |(ident, _, expr)| match ident {
            Token::Ident(s) => (s.clone(), expr),
            _ => unreachable!(),
        },
    )(stream)
}
