use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::tuple;
use nom::IResult;

use super::pattern::parse_assignable_pattern;
use super::{tag, TokenSlice};
use crate::parsing::{
    lexer::TokenType, parser::expression::parse_top_level_expression, Assignment, Expression,
    Parsed, Pattern, Statement, TryStatement,
};

pub type ParsedStatement<'a> = IResult<TokenSlice<'a>, Parsed<Statement>>;

pub fn parse_statement(stream: TokenSlice) -> ParsedStatement {
    alt((
        map(
            tuple((
                parse_assignment(parse_assignable_pattern, parse_top_level_expression),
                tag(TokenType::SemiColon),
            )),
            |(assignment, end)| {
                Parsed::new(
                    Statement::Assignment(assignment.data),
                    assignment.start_pos,
                    Some(end.end_pos),
                )
            },
        ),
        map(parse_try_statement, |stmt| {
            stmt.map(Statement::TryStatement)
        }),
        parse_set_statement,
    ))(stream)
}

pub fn parse_assignment<'a, P, F>(
    pat_fn: P,
    expr_fn: F,
) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Parsed<Assignment>>
where
    P: Fn(TokenSlice) -> IResult<TokenSlice, Parsed<Pattern>>,
    F: Fn(TokenSlice) -> IResult<TokenSlice, Parsed<Expression>>,
{
    map(
        tuple((pat_fn, tag(TokenType::Equals), expr_fn)),
        |(lhs, _, rhs)| {
            let start_pos = lhs.start_pos;
            let end_pos = rhs.end_pos;
            Parsed::new(Assignment { lhs, rhs }, start_pos, end_pos)
        },
    )
}

pub fn parse_try_statement(stream: TokenSlice) -> IResult<TokenSlice, Parsed<TryStatement>> {
    map(
        tuple((
            tag(TokenType::Try),
            parse_top_level_expression,
            tag(TokenType::SemiColon),
        )),
        |(start, expr, end)| {
            Parsed::new(TryStatement(expr.data), start.start_pos, Some(end.end_pos))
        },
    )(stream)
}

pub fn parse_set_statement(stream: TokenSlice) -> ParsedStatement {
    map(
        tuple((
            tag(TokenType::Set),
            tag(TokenType::OpenParen),
            parse_top_level_expression,
            tag(TokenType::Comma),
            parse_top_level_expression,
            tag(TokenType::CloseParen),
            tag(TokenType::SemiColon),
        )),
        |(start, _, lhs, _, rhs, _, end)| {
            Parsed::new(
                Statement::SetStatement {
                    trait_object: lhs,
                    target: rhs,
                },
                start.start_pos,
                Some(end.end_pos),
            )
        },
    )(stream)
}
