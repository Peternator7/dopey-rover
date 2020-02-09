use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::tuple;

use super::ParsedExpression;

use super::super::{statement::parse_statement, tag, Parsed, TokenSlice};
use super::{parse_boolean_expression, parse_top_level_expression, Expression};
use crate::parsing::lexer::TokenType;

pub fn parse_blocklike_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::OpenBrace),
            many0(parse_statement),
            opt(parse_top_level_expression),
            tag(TokenType::CloseBrace),
        )),
        |(start, stmts, expr, end)| {
            Parsed::new(
                Expression::BlockExpression(stmts, expr.map(Box::new)),
                start.start_pos,
                Some(end.end_pos),
            )
        },
    )(stream)
}

pub fn parse_block_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::OpenBrace),
            many0(parse_statement),
            opt(parse_top_level_expression),
            tag(TokenType::CloseBrace),
        )),
        |(start, stmts, expr, end)| {
            Parsed::new(
                Expression::BlockExpression(stmts, expr.map(Box::new)),
                start.start_pos,
                Some(end.end_pos),
            )
        },
    )(stream)
}

fn parse_if_else_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::If),
            parse_boolean_expression,
            parse_block_expression,
            tag(TokenType::Else),
            parse_block_expression,
        )),
        |(start, cond, if_block, _, else_block)| {
            Parsed::new(
                Expression::IfElseExpression(
                    Box::new(cond),
                    Box::new(if_block),
                    Box::new(else_block),
                ),
                start.start_pos,
                else_block.end_pos,
            )
        },
    )(stream)
}
