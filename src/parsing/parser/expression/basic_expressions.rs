use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::separated_nonempty_list;
use nom::sequence::{preceded, tuple};
use nom::IResult;

use super::{parse_top_level_expression, Expression, ParsedExpression};
use crate::parsing::lexer::Token;
use crate::parsing::parser::pattern::parse_object_creation_property_pattern;
use crate::parsing::parser::{extract_identifier, tag, test, Assignment};

pub fn parse_basic_expression(stream: &[Token]) -> ParsedExpression {
    alt((
        parse_new_object,
        parse_string_literal,
        parse_number_literal,
        parse_ident_expression,
        parse_get_expression,
        parse_parens_expression,
        parse_array_expression,
    ))(stream)
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
            tag(Token::OpenBrace),
            parse_object_property_list,
            tag(Token::CloseBrace),
        )),
        |(_, result, _)| result,
    )(stream)
}

pub fn parse_object_property_list(stream: &[Token]) -> ParsedExpression {
    map(
        tuple((
            separated_nonempty_list(tag(Token::Comma), parse_object_property),
            opt(tag(Token::Comma)),
        )),
        |(props, _)| Expression::NewObject(props),
    )(stream)
    .or_else(|_| Ok((stream, Expression::NewObject(Vec::new()))))
}

pub fn parse_object_property<'a>(stream: &'a [Token]) -> IResult<&'a [Token], Assignment> {
    map(
        tuple((
            parse_object_creation_property_pattern,
            tag(Token::Equals),
            parse_top_level_expression,
        )),
        |(patt, _, expr)| Assignment {
            lhs: patt,
            rhs: expr,
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

// pub fn parse_trait_object_property_list(stream: &[Token]) -> ParsedExpression {
//     map(
//         separated_list(tag(Token::Comma), parse_object_property),
//         |props| Expression::NewObject(props.len()),
//     )(stream)
// }
