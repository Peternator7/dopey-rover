use nom::branch::alt;
use nom::combinator::map;
use nom::sequence::tuple;
use nom::IResult;

use super::tag;
use crate::parsing::lexer::Token;
use crate::parsing::parser::expression::parse_top_level_expression;
use crate::parsing::parser::expression::Expression;
use crate::parsing::parser::pattern::{parse_assignable_pattern, Pattern};

pub type ParsedStatement<'a> = IResult<&'a [Token], Statement>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assignment(Pattern, Expression),
    Try(Expression),
    ModuleImport,
}

impl Statement {
    pub fn requires_trailing_semicolon(&self) -> bool {
        match self {
            Statement::Assignment(_, expr) | Statement::Try(expr) => {
                expr.requires_trailing_semicolon()
            }
            _ => true,
        }
    }
}

pub fn parse_statement(stream: &[Token]) -> ParsedStatement {
    alt((parse_assignment, parse_try_statement))(stream)
}

pub fn parse_assignment(stream: &[Token]) -> ParsedStatement {
    map(
        tuple((
            parse_assignable_pattern,
            tag(Token::Equals),
            parse_top_level_expression,
            tag(Token::SemiColon),
        )),
        |(ident, _, expr, _)| Statement::Assignment(ident, expr),
    )(stream)
}

pub fn parse_try_statement(stream: &[Token]) -> ParsedStatement {
    map(
        tuple((
            tag(Token::Try),
            parse_top_level_expression,
            tag(Token::SemiColon),
        )),
        |(_, expr, _)| Statement::Try(expr),
    )(stream)
}
