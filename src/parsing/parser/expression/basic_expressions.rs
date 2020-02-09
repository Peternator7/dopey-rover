use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::separated_nonempty_list;
use nom::sequence::{preceded, tuple};
use nom::IResult;

use super::{
    parse_top_level_expression, BinaryExpression, BinaryOperator, Expression, ParsedExpression,
};
use crate::parsing::{
    lexer::{Token, TokenType},
    parser::{
        extract_identifier, pattern::parse_object_creation_property_pattern, statement::Assignment,
        tag, test, Parsed, TokenSlice,
    },
    Position,
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
    map(extract_identifier, |ident| ident.map(Expression::Variable))(stream)
}

fn parse_string_literal(stream: TokenSlice) -> ParsedExpression {
    map(test(TokenType::is_string_literal), |tok| match tok.ty {
        TokenType::StringLiteral(s) => Parsed::new(
            Expression::StringLiteral(s.to_string()),
            tok.start_pos,
            Some(tok.end_pos),
        ),
        _ => unreachable!(),
    })(stream)
}

fn parse_number_literal(stream: TokenSlice) -> ParsedExpression {
    map(test(TokenType::is_number), |tok| match tok.ty {
        TokenType::Number(f) => {
            Parsed::new(Expression::Number(f), tok.start_pos, Some(tok.end_pos))
        }
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
                Expression::GetExpression(ident.data, Box::new(expr)),
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
        |(start, expr, end)| {
            Parsed::new(
                Expression::NewObject(expr),
                start.start_pos,
                Some(end.end_pos),
            )
        },
    )(stream)
}

fn parse_object_property_list(stream: TokenSlice) -> IResult<TokenSlice, Vec<Parsed<Assignment>>> {
    map(
        tuple((
            separated_nonempty_list(tag(TokenType::Comma), parse_object_property),
            opt(tag(TokenType::Comma)),
        )),
        |(props, _)| props,
    )(stream)
    .or_else(|_| Ok((stream, Vec::new())))
}

fn parse_object_property(stream: TokenSlice) -> IResult<TokenSlice, Parsed<Assignment>> {
    map(
        tuple((
            parse_object_creation_property_pattern,
            tag(TokenType::Equals),
            parse_top_level_expression,
        )),
        |(patt, _, expr)| {
            Parsed::new(
                Assignment {
                    lhs: patt,
                    rhs: expr,
                },
                patt.start_pos,
                expr.end_pos,
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
            Ok((
                rem,
                Parsed::new(
                    Expression::BinaryExpression(Box::new(BinaryExpression::new(
                        BinaryOperator::Cons,
                        head,
                        tail,
                    ))),
                    head.start_pos,
                    tail.end_pos,
                ),
            ))
        } else {
            Ok((
                rem,
                Parsed::new(
                    Expression::BinaryExpression(Box::new(BinaryExpression::new(
                        BinaryOperator::Cons,
                        head,
                        Parsed::new(Expression::NilArrayExpression, Position::new(0, 0), None),
                    ))),
                    head.start_pos,
                    head.end_pos,
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
        |(start, elements, end)| elements,
    )(stream)
}
