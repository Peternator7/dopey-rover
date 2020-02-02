pub mod basic_expressions;
pub mod operators;

use nom::branch::alt;
use nom::combinator::{map, opt, value};
use nom::multi::{fold_many0, many0};
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use super::statement::{parse_statement, Statement};
use super::Assignment;
use super::{extract_identifier, tag};
use crate::parsing::lexer::Token;

use self::basic_expressions::parse_basic_expression;
use self::operators::*;

pub type ParsedExpression<'a> = IResult<&'a [Token], Expression>;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Number(f32),
    StringLiteral(String),
    NewObject(Vec<Assignment>),
    NewTraitObject(Box<Expression>, usize),
    ConsArrayExpression(Box<Expression>, Box<Expression>),
    NilArrayExpression,
    Variable(String),
    AlgebraicExpression(AlgebraicOperator, Box<Expression>, Box<Expression>),
    ComparisonExpression(ComparisonOperator, Box<Expression>, Box<Expression>),
    BooleanExpression(BooleanOperator, Box<Expression>, Box<Expression>),
    BooleanReturnExpression(BooleanOperator, Box<Expression>, Option<Box<Expression>>),
    GetExpression(String, Box<Expression>),
    TestExpression(Box<Expression>, Box<Expression>),
    // Lhs = Function Object
    // Rhs = Argument
    CallExpression(Box<Expression>, Box<Expression>),
    ObjectLookupExpression(Box<Expression>, String),
    IfElseExpression(Box<Expression>, Box<Expression>, Box<Expression>),
    MatchExpression(Box<Expression>, Vec<()>),
    BlockExpression(Vec<Statement>, Option<Box<Expression>>),
    IfLetExpression,
}

impl Expression {
    pub fn requires_trailing_semicolon(&self) -> bool {
        match self {
            Expression::IfElseExpression(_, _, _) => false,
            Expression::IfLetExpression => false,
            Expression::MatchExpression(_, _) => false,
            Expression::BlockExpression(_, _) => false,
            _ => true,
        }
    }
}

pub fn parse_top_level_expression(stream: &[Token]) -> ParsedExpression {
    alt((parse_boolean_expression, parse_block_expression))(stream)
}

pub fn parse_block_expression(stream: &[Token]) -> ParsedExpression {
    map(
        tuple((
            tag(Token::OpenBrace),
            many0(parse_statement),
            opt(parse_top_level_expression),
            tag(Token::CloseBrace),
        )),
        |(_, stmts, expr, _)| Expression::BlockExpression(stmts, expr.map(Box::new)),
    )(stream)
}

pub fn parse_boolean_operator<'a>(stream: &'a [Token]) -> IResult<&'a [Token], BooleanOperator> {
    alt((
        value(BooleanOperator::And, tag(Token::AndAnd)),
        value(BooleanOperator::Or, tag(Token::OrOr)),
    ))(stream)
}

pub fn parse_boolean_expression_2(stream: &[Token]) -> ParsedExpression {
    // In reality, we shouldn't let you 'return' multiple times
    // False || return True || return True
    // is nonsensical, but we can figure that out later. It's dead code
    let (rem, init) = parse_test_expression(stream)?;
    fold_many0(
        pair(parse_boolean_operator, parse_test_expression),
        init,
        |acc, (op, rhs)| Expression::BooleanExpression(op, Box::new(acc), Box::new(rhs)),
    )(rem)
}

pub fn parse_boolean_expression(stream: &[Token]) -> ParsedExpression {
    // In reality, we shouldn't let you 'return' multiple times
    // False || return True || return True
    // is nonsensical, but we can figure that out later. It's dead code
    let (mut remainder, mut output) = parse_test_expression(stream)?;

    enum Rhs {
        TestExpr(Box<Expression>),
        Return(Option<Box<Expression>>),
    }

    loop {
        let options = alt((
            map(parse_test_expression, |expr| Rhs::TestExpr(Box::new(expr))),
            map(
                tuple((tag(Token::Return), opt(parse_test_expression))),
                |(_, expr)| Rhs::Return(expr.map(Box::new)),
            ),
        ));

        match pair(parse_boolean_operator, options)(remainder) {
            Ok((rem, (op, Rhs::TestExpr(rhs)))) => {
                remainder = rem;
                output = Expression::BooleanExpression(op, Box::new(output), rhs)
            }
            Ok((rem, (op, Rhs::Return(rhs)))) => {
                remainder = rem;
                output = Expression::BooleanReturnExpression(op, Box::new(output), rhs);
                break;
            }
            Err(_) => break,
        }
    }

    Ok((remainder, output))
}

pub fn parse_test_expression(stream: &[Token]) -> ParsedExpression {
    map(
        pair(
            parse_comparison_expression,
            opt(preceded(tag(Token::Is), parse_comparison_expression)),
        ),
        |(lhs, rhs)| {
            if let Some(rhs) = rhs {
                Expression::TestExpression(Box::new(lhs), Box::new(rhs))
            } else {
                lhs
            }
        },
    )(stream)
}

pub fn parse_comparison_operator(stream: &[Token]) -> IResult<&[Token], ComparisonOperator> {
    alt((
        value(ComparisonOperator::EqualTo, tag(Token::EqualEquals)),
        value(ComparisonOperator::NotEqualTo, tag(Token::NotEquals)),
        value(ComparisonOperator::LessThan, tag(Token::LessThan)),
        value(ComparisonOperator::GreaterThan, tag(Token::GreaterThan)),
        value(
            ComparisonOperator::LessThanOrEqualTo,
            tag(Token::LessThanOrEqualTo),
        ),
        value(
            ComparisonOperator::GreaterThanOrEqualTo,
            tag(Token::GreaterThanOrEqualTo),
        ),
    ))(stream)
}

pub fn parse_comparison_expression(stream: &[Token]) -> ParsedExpression {
    map(
        pair(
            parse_addition_expression,
            opt(pair(parse_comparison_operator, parse_addition_expression)),
        ),
        |(lhs, rhs)| {
            if let Some((op, rhs)) = rhs {
                Expression::ComparisonExpression(op, Box::new(lhs), Box::new(rhs))
            } else {
                lhs
            }
        },
    )(stream)
}

pub fn parse_addition_operator(stream: &[Token]) -> IResult<&[Token], AlgebraicOperator> {
    alt((
        value(AlgebraicOperator::Add, tag(Token::Plus)),
        value(AlgebraicOperator::Sub, tag(Token::Minus)),
    ))(stream)
}

pub fn parse_addition_expression(stream: &[Token]) -> ParsedExpression {
    let (rem, init) = parse_multiplication_expression(stream)?;
    fold_many0(
        pair(parse_addition_operator, parse_multiplication_expression),
        init,
        |acc, (op, rhs)| Expression::AlgebraicExpression(op, Box::new(acc), Box::new(rhs)),
    )(rem)
}

pub fn parse_multiplication_operator(stream: &[Token]) -> IResult<&[Token], AlgebraicOperator> {
    alt((
        value(AlgebraicOperator::Divide, tag(Token::Divide)),
        value(AlgebraicOperator::Mult, tag(Token::Multiply)),
        value(AlgebraicOperator::Modulus, tag(Token::Modulus)),
    ))(stream)
}

pub fn parse_multiplication_expression(stream: &[Token]) -> ParsedExpression {
    let (rem, init) = parse_arithmatic_factor(stream)?;
    fold_many0(
        pair(parse_multiplication_operator, parse_arithmatic_factor),
        init,
        |acc, (op, rhs)| Expression::AlgebraicExpression(op, Box::new(acc), Box::new(rhs)),
    )(rem)
}

pub fn parse_arithmatic_factor(stream: &[Token]) -> ParsedExpression {
    parse_call_expression(stream)
}

pub fn parse_call_expression(stream: &[Token]) -> ParsedExpression {
    let (rem, init) = parse_call_lhs_expression(stream)?;
    fold_many0(parse_call_lhs_expression, init, |acc, arg| {
        Expression::CallExpression(Box::new(acc), Box::new(arg))
    })(rem)
}

pub fn parse_call_lhs_expression(stream: &[Token]) -> ParsedExpression {
    alt((parse_head_expression, parse_if_else_expression))(stream)
}

pub fn parse_head_expression(stream: &[Token]) -> ParsedExpression {
    let (rem, init) = parse_object_lookup_expression(stream)?;
    fold_many0(
        pair(tag(Token::DoubleColon), parse_head_expression),
        init,
        |acc, (_, rhs)| Expression::ConsArrayExpression(Box::new(acc), Box::new(rhs)),
    )(rem)
}

pub fn parse_object_lookup_expression(stream: &[Token]) -> ParsedExpression {
    let (rem, init) = parse_basic_expression(stream)?;
    fold_many0(
        preceded(tag(Token::Period), extract_identifier),
        init,
        |acc, property| Expression::ObjectLookupExpression(Box::new(acc), property),
    )(rem)
}

pub fn parse_if_else_expression(stream: &[Token]) -> ParsedExpression {
    map(
        tuple((
            tag(Token::If),
            parse_top_level_expression,
            parse_block_expression,
            tag(Token::Else),
            parse_block_expression,
        )),
        |(_, cond, if_block, _, else_block)| {
            Expression::IfElseExpression(Box::new(cond), Box::new(if_block), Box::new(else_block))
        },
    )(stream)
}
