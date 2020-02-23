use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::separated_nonempty_list;
use nom::sequence::{preceded, tuple};
use nom::IResult;

use super::{parse_top_level_expression, BinaryOperator, Expression, ParsedExpression};
use crate::parsing::{
    lexer::TokenType,
    parser::{extract_identifier, pattern::parse_item_pattern, tag, test, TokenSlice},
    Assignment, ItemPattern, Parsed, Position,
};

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
    map(extract_identifier, |ident| {
        ident.map(|value| Expression::Variable { value })
    })(stream)
}

fn parse_string_literal(stream: TokenSlice) -> ParsedExpression {
    map(test(TokenType::is_string_literal), |tok| match tok.ty {
        TokenType::StringLiteral(s) => Parsed::new(
            Expression::StringLiteral {
                value: s.to_string(),
            },
            tok.start_pos,
            Some(tok.end_pos),
        ),
        _ => unreachable!(),
    })(stream)
}

fn parse_number_literal(stream: TokenSlice) -> ParsedExpression {
    map(test(TokenType::is_number), |tok| match tok.ty {
        TokenType::Number(f) => Parsed::new(
            Expression::Number { value: f },
            tok.start_pos,
            Some(tok.end_pos),
        ),
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
        |(start, _, ident, _, expr, end)| {
            Parsed::new(
                Expression::GetExpression {
                    trait_name: ident.data,
                    object: Box::new(expr),
                },
                start.start_pos,
                Some(end.end_pos),
            )
        },
    )(stream)
}

fn parse_parens_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::OpenParen),
            parse_top_level_expression,
            tag(TokenType::CloseParen),
        )),
        |(start, expr, end)| Parsed::new(expr.data, start.start_pos, Some(end.end_pos)),
    )(stream)
}

fn parse_new_object(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::OpenBrace),
            parse_object_property_list,
            tag(TokenType::CloseBrace),
        )),
        |(start, fields, end)| {
            Parsed::new(
                Expression::NewObject { fields },
                start.start_pos,
                Some(end.end_pos),
            )
        },
    )(stream)
}

fn parse_object_property_list(
    stream: TokenSlice,
) -> IResult<TokenSlice, Vec<Parsed<Assignment<ItemPattern>>>> {
    map(
        tuple((
            separated_nonempty_list(tag(TokenType::Comma), parse_object_property),
            opt(tag(TokenType::Comma)),
        )),
        |(props, _)| props,
    )(stream)
    .or_else(|_| Ok((stream, Vec::new())))
}

fn parse_object_property(
    stream: TokenSlice,
) -> IResult<TokenSlice, Parsed<Assignment<ItemPattern>>> {
    map(
        tuple((
            parse_item_pattern,
            tag(TokenType::Equals),
            parse_top_level_expression,
        )),
        |(patt, _, expr)| {
            let start_pos = patt.start_pos;
            let end_pos = expr.end_pos;
            Parsed::new(
                Assignment {
                    lhs: patt,
                    rhs: expr,
                },
                start_pos,
                end_pos,
            )
        },
    )(stream)
}

fn parse_array_expression(stream: TokenSlice) -> ParsedExpression {
    fn parse_array_expression_inner(stream: TokenSlice) -> ParsedExpression {
        let res = parse_top_level_expression(stream);
        if res.is_err() {
            return Ok((
                stream,
                Parsed::new(Expression::NilArrayExpression, Position::new(0, 0), None),
            ));
        }

        let (rem, head) = res.unwrap();
        let res = preceded(tag(TokenType::Comma), parse_array_expression_inner)(rem);
        if let Ok((rem, tail)) = res {
            let start_pos = head.start_pos;
            let end_pos = tail.end_pos;
            Ok((
                rem,
                Parsed::new(
                    Expression::BinaryExpression {
                        op: BinaryOperator::Cons,
                        lhs: Box::new(head),
                        rhs: Box::new(tail),
                    },
                    start_pos,
                    end_pos,
                ),
            ))
        } else {
            let start_pos = head.start_pos;
            let end_pos = head.end_pos;
            Ok((
                rem,
                Parsed::new(
                    Expression::BinaryExpression {
                        op: BinaryOperator::Cons,
                        lhs: Box::new(head),
                        rhs: Box::new(Parsed::new(
                            Expression::NilArrayExpression,
                            Position::new(0, 0),
                            None,
                        )),
                    },
                    start_pos,
                    end_pos,
                ),
            ))
        }
    }

    map(
        tuple((
            tag(TokenType::OpenBracket),
            parse_array_expression_inner,
            tag(TokenType::CloseBracket),
        )),
        |(start, mut elements, end)| {
            elements.start_pos = start.start_pos;
            elements.end_pos = Some(end.end_pos);
            elements
        },
    )(stream)
}
