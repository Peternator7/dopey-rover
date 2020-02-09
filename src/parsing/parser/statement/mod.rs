use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::{terminated, tuple};
use nom::IResult;

use super::pattern::{parse_assignable_pattern, Pattern};
use super::{expression::Expression, tag, Assignment, TryStatement, TokenSlice};
use crate::parsing::lexer::{Token, TokenType};
use crate::parsing::parser::expression::parse_top_level_expression;

pub type ParsedStatement<'a> = IResult<TokenSlice<'a>, Statement>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assignment(Assignment),
    TryStatement(TryStatement),
    SetStatement(Expression, Expression),
    ModuleImport,
}

pub fn parse_statement(stream: TokenSlice) -> ParsedStatement {
    alt((
        map(
            terminated(
                parse_assignment(parse_assignable_pattern, parse_top_level_expression),
                tag(TokenType::SemiColon),
            ),
            Statement::Assignment,
        ),
        map(parse_try_statement, Statement::TryStatement),
        parse_set_statement,
    ))(stream)
}

pub fn parse_assignment<'a, P, F>(
    pat_fn: P,
    expr_fn: F,
) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Assignment>
where
    P: Fn(TokenSlice) -> IResult<TokenSlice, Pattern>,
    F: Fn(TokenSlice) -> IResult<TokenSlice, Expression>,
{
    map(
        tuple((pat_fn, tag(TokenType::Equals), expr_fn)),
        |(lhs, _, rhs)| Assignment { lhs, rhs },
    )
}

pub fn parse_try_statement(stream: TokenSlice) -> IResult<TokenSlice, TryStatement> {
    map(
        tuple((
            tag(TokenType::Try),
            parse_top_level_expression,
            tag(TokenType::SemiColon),
        )),
        |(_, expr, _)| TryStatement(expr),
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
        |(_, _, lhs, _, rhs, _, _)| Statement::SetStatement(lhs, rhs),
    )(stream)
}
