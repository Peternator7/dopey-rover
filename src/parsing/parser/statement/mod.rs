use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::tuple;
use nom::IResult;

use super::{expression::Expression, tag, Assignment, TryStatement};
use crate::parsing::lexer::Token;
use crate::parsing::parser::expression::parse_top_level_expression;
use crate::parsing::parser::pattern::parse_assignable_pattern;

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
        map(parse_assignment, Statement::Assignment),
        map(parse_try_statement, Statement::TryStatement),
        parse_set_statement,
    ))(stream)
}

pub fn parse_assignment(stream: &[Token]) -> IResult<&[Token], Assignment> {
    map(
        tuple((
            parse_assignable_pattern,
            tag(Token::Equals),
            parse_top_level_expression,
            tag(Token::SemiColon),
        )),
        |(lhs, _, rhs, _)| Assignment { lhs, rhs },
    )(stream)
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
