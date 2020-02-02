use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::{terminated, tuple};
use nom::IResult;

use super::pattern::{parse_assignable_pattern, Pattern};
use super::{expression::Expression, tag, Assignment, TryStatement};
use crate::parsing::lexer::Token;
use crate::parsing::parser::expression::parse_top_level_expression;

pub type ParsedStatement<'a> = IResult<&'a [Token], Statement>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assignment(Assignment),
    TryStatement(TryStatement),
    SetStatement(Expression, Expression),
    ModuleImport,
}

pub fn parse_statement(stream: &[Token]) -> ParsedStatement {
    alt((
        map(
            terminated(
                parse_assignment(parse_assignable_pattern, parse_top_level_expression),
                tag(Token::SemiColon),
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
) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], Assignment>
where
    P: Fn(&[Token]) -> IResult<&[Token], Pattern>,
    F: Fn(&[Token]) -> IResult<&[Token], Expression>,
{
    map(
        tuple((pat_fn, tag(Token::Equals), expr_fn)),
        |(lhs, _, rhs)| Assignment { lhs, rhs },
    )
}

pub fn parse_try_statement(stream: &[Token]) -> IResult<&[Token], TryStatement> {
    map(
        tuple((
            tag(Token::Try),
            parse_top_level_expression,
            tag(Token::SemiColon),
        )),
        |(_, expr, _)| TryStatement(expr),
    )(stream)
}

pub fn parse_set_statement(stream: &[Token]) -> ParsedStatement {
    map(
        tuple((
            tag(Token::Set),
            tag(Token::OpenParen),
            parse_top_level_expression,
            tag(Token::Comma),
            parse_top_level_expression,
            tag(Token::CloseParen),
            tag(Token::SemiColon),
        )),
        |(_, _, lhs, _, rhs, _, _)| Statement::SetStatement(lhs, rhs),
    )(stream)
}
