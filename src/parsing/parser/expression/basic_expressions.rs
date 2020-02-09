use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::separated_nonempty_list;
use nom::sequence::{preceded, tuple};
use nom::IResult;

use super::{
    parse_top_level_expression, BinaryExpression, BinaryOperator, Expression, ParsedExpression,
};
use crate::parsing::lexer::{Token, TokenType};
use crate::parsing::parser::pattern::parse_object_creation_property_pattern;
use crate::parsing::parser::{extract_identifier, tag, test, Assignment, TokenSlice};

pub fn parse_basic_expression(stream: TokenSlice) -> ParsedExpression {
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

fn parse_ident_expression(stream: TokenSlice) -> ParsedExpression {
    map(extract_identifier, Expression::Variable)(stream)
}

fn parse_string_literal(stream: TokenSlice) -> ParsedExpression {
    map(test(TokenType::is_string_literal), |tok| match tok.ty {
        TokenType::StringLiteral(s) => Expression::StringLiteral(s.to_string()),
        _ => unreachable!(),
    })(stream)
}

fn parse_number_literal(stream: TokenSlice) -> ParsedExpression {
    map(test(TokenType::is_number), |tok| match tok.ty {
        TokenType::Number(f) => Expression::Number(f),
        _ => unreachable!(),
    })(stream)
}

fn parse_get_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::Get),
            tag(TokenType::OpenParen),
            extract_identifier,
            tag(TokenType::Comma),
            parse_top_level_expression,
            tag(TokenType::CloseParen),
        )),
        |(_, _, ident, _, expr, _)| Expression::GetExpression(ident, Box::new(expr)),
    )(stream)
}

fn parse_parens_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::OpenParen),
            parse_top_level_expression,
            tag(TokenType::CloseParen),
        )),
        |(_, expr, _)| expr,
    )(stream)
}

fn parse_new_object(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::OpenBrace),
            parse_object_property_list,
            tag(TokenType::CloseBrace),
        )),
        |(_, result, _)| result,
    )(stream)
}

fn parse_object_property_list(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            separated_nonempty_list(tag(TokenType::Comma), parse_object_property),
            opt(tag(TokenType::Comma)),
        )),
        |(props, _)| Expression::NewObject(props),
    )(stream)
    .or_else(|_| Ok((stream, Expression::NewObject(Vec::new()))))
}

fn parse_object_property(stream: TokenSlice) -> IResult<TokenSlice, Assignment> {
    map(
        tuple((
            parse_object_creation_property_pattern,
            tag(TokenType::Equals),
            parse_top_level_expression,
        )),
        |(patt, _, expr)| Assignment {
            lhs: patt,
            rhs: expr,
        },
    )(stream)
}

fn parse_array_expression(stream: TokenSlice) -> ParsedExpression {
    fn parse_array_expression_inner(stream: TokenSlice) -> ParsedExpression {
        let res = parse_top_level_expression(stream);
        if res.is_err() {
            return Ok((stream, Expression::NilArrayExpression));
        }

        let (rem, head) = res.unwrap();
        let res = preceded(tag(TokenType::Comma), parse_array_expression_inner)(rem);
        if let Ok((rem, tail)) = res {
            Ok((
                rem,
                Expression::BinaryExpression(Box::new(BinaryExpression::new(
                    BinaryOperator::Cons,
                    head,
                    tail,
                ))),
            ))
        } else {
            Ok((
                rem,
                Expression::BinaryExpression(Box::new(BinaryExpression::new(
                    BinaryOperator::Cons,
                    head,
                    Expression::NilArrayExpression,
                ))),
            ))
        }
    }

    map(
        tuple((
            tag(TokenType::OpenBracket),
            parse_array_expression_inner,
            tag(TokenType::CloseBracket),
        )),
        |(_, elements, _)| elements,
    )(stream)
}
