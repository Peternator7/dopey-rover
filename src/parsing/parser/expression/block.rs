use nom::combinator::{map, opt, value};
use nom::multi::{fold_many0, many0};
use nom::sequence::{pair, preceded, tuple};

use super::ParsedExpression;

use super::super::{statement::parse_statement, tag, TokenSlice};
use super::{parse_boolean_expression, parse_top_level_expression, Expression};
use crate::parsing::lexer::{TokenType};

pub fn parse_blocklike_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        tuple((
            tag(TokenType::OpenBrace),
            many0(parse_statement),
            opt(parse_top_level_expression),
            tag(TokenType::CloseBrace),
        )),
        |(_, stmts, expr, _)| Expression::BlockExpression(stmts, expr.map(Box::new)),
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
        |(_, stmts, expr, _)| Expression::BlockExpression(stmts, expr.map(Box::new)),
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
        |(_, cond, if_block, _, else_block)| {
            Expression::IfElseExpression(Box::new(cond), Box::new(if_block), Box::new(else_block))
        },
    )(stream)
}
