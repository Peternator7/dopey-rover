use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{many0, many1, separated_list};
use nom::sequence::tuple;
use nom::IResult;

use super::{extract_identifier, tag};
use crate::parsing::lexer::Token;
use crate::parsing::parser::expression::parse_top_level_expression;
use crate::parsing::parser::expression::Expression;
use crate::parsing::parser::pattern::{parse_top_level_pattern, Pattern};

pub type ParsedStatement<'a> = IResult<&'a [Token], Statement>;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    FunctionDeclaration(String, Vec<String>, Expression),
    ObjectAssignment(Pattern, Expression),
    Try(Expression),
    ModuleImport,
}

impl Statement {
    pub fn requires_trailing_semicolon(&self) -> bool {
        match self {
            Statement::FunctionDeclaration(_, _, expr) | 
            Statement::ObjectAssignment(_, expr) |
            Statement::Try(expr) => expr.requires_trailing_semicolon(),
            _ => true
        }
    }
}

pub fn parse_statement(stream: &[Token]) -> ParsedStatement {
    alt((parse_function_declaration, parse_object_assignment, parse_try_statement))(stream)
}

pub fn parse_object_assignment(stream: &[Token]) -> ParsedStatement {
    map(
        tuple((
            parse_top_level_pattern,
            tag(Token::Equals),
            parse_top_level_expression,
            tag(Token::SemiColon),
        )),
        |(ident, _, expr, _)| Statement::ObjectAssignment(ident, expr),
    )(stream)
}

pub fn parse_function_declaration(stream: &[Token]) -> ParsedStatement {
    map(
        tuple((
            extract_identifier,
            many1(extract_identifier),
            tag(Token::Equals),
            parse_top_level_expression,
            tag(Token::SemiColon)
        )),
        |(ident, args, _, expr, _)| Statement::FunctionDeclaration(ident, args, expr),
    )(stream)
}

pub fn parse_try_statement(stream: &[Token]) -> ParsedStatement {
    map(
        tuple((tag(Token::Try), parse_top_level_expression, tag(Token::SemiColon))),
        |(_, expr,_)| Statement::Try(expr),
    )(stream)
}
