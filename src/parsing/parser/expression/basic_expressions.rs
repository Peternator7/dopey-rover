use nom::branch::alt;
use nom::combinator::map;
use nom::multi::separated_list;
use nom::sequence::{preceded, tuple};
use nom::IResult;

use super::{
    parse_object_lookup_expression, parse_top_level_expression, Expression, ObjectProperty,
    ParsedExpression,
};
use crate::parsing::lexer::Token;
use crate::parsing::parser::{extract_identifier, tag, test};

pub fn parse_basic_expression(allow_new_object: bool) -> impl Fn(&[Token]) -> ParsedExpression {
    move |stream| {
        if allow_new_object {
            alt((
                parse_new_object,
                parse_new_trait_object,
                parse_string_literal,
                parse_number_literal,
                parse_ident_expression,
                parse_get_expression,
                parse_parens_expression,
                parse_array_expression,
            ))(stream)
        } else {
            alt((
                parse_string_literal,
                parse_number_literal,
                parse_ident_expression,
                parse_get_expression,
                parse_parens_expression,
                parse_array_expression,
            ))(stream)
        }
    }
}

pub fn parse_ident_expression(stream: &[Token]) -> ParsedExpression {
    map(extract_identifier, Expression::Variable)(stream)
}

pub fn parse_string_literal(stream: &[Token]) -> ParsedExpression {
    map(test(Token::is_string_literal), |tok| match tok {
        Token::StringLiteral(s) => Expression::StringLiteral(s.clone()),
        _ => unreachable!(),
    })(stream)
}

pub fn parse_number_literal(stream: &[Token]) -> ParsedExpression {
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
        |props| Expression::NewObject(props),
    )(stream)
}

pub fn parse_object_property<'a>(stream: &'a [Token]) -> IResult<&'a [Token], ObjectProperty> {
    map(
        tuple((
            test(Token::is_ident),
            tag(Token::Colon),
            parse_top_level_expression,
        )),
        |(ident, _, expr)| match ident {
            Token::Ident(s) => ObjectProperty {
                name: s.clone(),
                value: expr,
            },
            _ => unreachable!(),
        },
    )(stream)
}

pub fn parse_array_expression(stream: &[Token]) -> ParsedExpression {
    fn parse_array_expression_inner(stream: &[Token]) -> ParsedExpression {
        use Expression::*;

        let res = parse_top_level_expression(stream);
        if res.is_err() {
            return Ok((stream, NilArrayExpression));
        }

        let (rem, head) = res.unwrap();
        let res = preceded(tag(Token::Comma), parse_array_expression_inner)(rem);
        if let Ok((rem, tail)) = res {
            Ok((rem, ConsArrayExpression(Box::new(head), Box::new(tail))))
        } else {
            Ok((
                rem,
                ConsArrayExpression(Box::new(head), Box::new(NilArrayExpression)),
            ))
        }
    }

    map(
        tuple((
            tag(Token::OpenBracket),
            parse_array_expression_inner,
            tag(Token::CloseBracket),
        )),
        |(_, elements, _)| elements,
    )(stream)
}

pub fn parse_new_trait_object(stream: &[Token]) -> ParsedExpression {
    map(
        tuple((
            tag(Token::New),
            parse_object_lookup_expression(false),
            tag(Token::OpenBrace),
            parse_object_property_list,
            tag(Token::CloseBrace),
        )),
        |(_, tr, _, result, _)| result,
    )(stream)
}

// pub fn parse_trait_object_property_list(stream: &[Token]) -> ParsedExpression {
//     map(
//         separated_list(tag(Token::Comma), parse_object_property),
//         |props| Expression::NewObject(props.len()),
//     )(stream)
// }
