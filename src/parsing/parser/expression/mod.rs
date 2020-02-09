pub mod basic_expressions;
pub mod block;
pub mod operators;

use nom::branch::alt;
use nom::combinator::{map, opt, value};
use nom::multi::fold_many0;
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use super::{
    extract_identifier,
    statement::{Assignment, Statement},
    tag, TokenSlice,
};

use crate::parsing::lexer::{Token, TokenType};

pub use super::Parsed;

use self::basic_expressions::parse_basic_expression;
use self::operators::*;

pub type ParsedExpression<'a> = IResult<TokenSlice<'a>, Expression>;

#[derive(Clone, Debug)]
pub enum Expression {
    Number(f32),
    StringLiteral(String),
    NewObject(Vec<Assignment>),
    NilArrayExpression,
    Variable(String),
    BinaryExpression(Box<BinaryExpression>),
    BooleanReturnExpression(BinaryOperator, Box<Expression>, Option<Box<Expression>>),
    GetExpression(String, Box<Expression>),
    ObjectLookupExpression(Box<Expression>, String),
    IfElseExpression(Box<Expression>, Box<Expression>, Box<Expression>),
    MatchExpression(Box<Expression>, Vec<()>),
    BlockExpression(Vec<Statement>, Option<Box<Expression>>),
    IfLetExpression,
}

#[derive(Clone, Debug)]
pub struct BinaryExpression {
    pub op: BinaryOperator,
    pub lhs: Expression,
    pub rhs: Expression,
}

impl BinaryExpression {
    pub fn new(op: BinaryOperator, lhs: Expression, rhs: Expression) -> BinaryExpression {
        BinaryExpression { op, lhs, rhs }
    }
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

pub fn parse_top_level_expression(stream: TokenSlice) -> ParsedExpression {
    alt((parse_boolean_expression, block::parse_block_expression))(stream)
}

fn parse_boolean_operator(stream: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    alt((
        value(BinaryOperator::And, tag(TokenType::And)),
        value(BinaryOperator::Or, tag(TokenType::Or)),
    ))(stream)
}

fn parse_boolean_expression(stream: TokenSlice) -> ParsedExpression {
    // In reality, we shouldn't let you 'return' multiple times
    // False || return True || return True
    // is nonsensical, but we can figure that out later. It's dead code
    let (mut remainder, mut output) = parse_test_expression(stream)?;

    enum Rhs {
        TestExpr(Expression),
        Return(Option<Box<Expression>>),
    }

    loop {
        let options = alt((
            map(parse_test_expression, Rhs::TestExpr),
            map(
                tuple((tag(TokenType::Return), opt(parse_test_expression))),
                |(_, expr)| Rhs::Return(expr.map(Box::new)),
            ),
        ));

        match pair(parse_boolean_operator, options)(remainder) {
            Ok((rem, (op, Rhs::TestExpr(rhs)))) => {
                remainder = rem;
                output =
                    Expression::BinaryExpression(Box::new(BinaryExpression::new(op, output, rhs)))
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

fn parse_test_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        pair(
            parse_comparison_expression,
            opt(preceded(tag(TokenType::Is), parse_comparison_expression)),
        ),
        |(lhs, rhs)| {
            if let Some(rhs) = rhs {
                Expression::BinaryExpression(Box::new(BinaryExpression::new(
                    BinaryOperator::Test,
                    lhs,
                    rhs,
                )))
            } else {
                lhs
            }
        },
    )(stream)
}

fn parse_comparison_operator(stream: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    alt((
        value(BinaryOperator::EqualTo, tag(TokenType::EqualEquals)),
        value(BinaryOperator::NotEqualTo, tag(TokenType::NotEquals)),
        value(BinaryOperator::LessThan, tag(TokenType::LessThan)),
        value(BinaryOperator::GreaterThan, tag(TokenType::GreaterThan)),
        value(
            BinaryOperator::LessThanOrEqualTo,
            tag(TokenType::LessThanOrEqualTo),
        ),
        value(
            BinaryOperator::GreaterThanOrEqualTo,
            tag(TokenType::GreaterThanOrEqualTo),
        ),
    ))(stream)
}

fn parse_comparison_expression(stream: TokenSlice) -> ParsedExpression {
    map(
        pair(
            parse_addition_expression,
            opt(pair(parse_comparison_operator, parse_addition_expression)),
        ),
        |(lhs, rhs)| {
            if let Some((op, rhs)) = rhs {
                Expression::BinaryExpression(Box::new(BinaryExpression::new(op, lhs, rhs)))
            } else {
                lhs
            }
        },
    )(stream)
}

fn parse_addition_operator(stream: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    alt((
        value(BinaryOperator::Add, tag(TokenType::Plus)),
        value(BinaryOperator::Sub, tag(TokenType::Minus)),
    ))(stream)
}

fn parse_addition_expression(stream: TokenSlice) -> ParsedExpression {
    let (rem, init) = parse_multiplication_expression(stream)?;
    fold_many0(
        pair(parse_addition_operator, parse_multiplication_expression),
        init,
        |acc, (op, rhs)| {
            Expression::BinaryExpression(Box::new(BinaryExpression::new(op, acc, rhs)))
        },
    )(rem)
}

fn parse_multiplication_operator(stream: TokenSlice) -> IResult<TokenSlice, BinaryOperator> {
    alt((
        value(BinaryOperator::Divide, tag(TokenType::Divide)),
        value(BinaryOperator::Mult, tag(TokenType::Multiply)),
        value(BinaryOperator::Modulus, tag(TokenType::Modulus)),
    ))(stream)
}

fn parse_multiplication_expression(stream: TokenSlice) -> ParsedExpression {
    let (rem, init) = parse_arithmatic_factor(stream)?;
    fold_many0(
        pair(parse_multiplication_operator, parse_arithmatic_factor),
        init,
        |acc, (op, rhs)| {
            Expression::BinaryExpression(Box::new(BinaryExpression::new(op, acc, rhs)))
        },
    )(rem)
}

fn parse_arithmatic_factor(stream: TokenSlice) -> ParsedExpression {
    parse_call_expression(stream)
}

fn parse_call_expression(stream: TokenSlice) -> ParsedExpression {
    let (rem, init) = parse_call_lhs_expression(stream)?;
    fold_many0(parse_call_lhs_expression, init, |acc, arg| {
        Expression::BinaryExpression(Box::new(BinaryExpression::new(
            BinaryOperator::Call,
            acc,
            arg,
        )))
    })(rem)
}

fn parse_call_lhs_expression(stream: TokenSlice) -> ParsedExpression {
    parse_head_expression(stream)
}

fn parse_head_expression(stream: TokenSlice) -> ParsedExpression {
    let (rem, init) = parse_object_lookup_expression(stream)?;
    fold_many0(
        pair(tag(TokenType::DoubleColon), parse_head_expression),
        init,
        |acc, (_, rhs)| {
            Expression::BinaryExpression(Box::new(BinaryExpression::new(
                BinaryOperator::Cons,
                acc,
                rhs,
            )))
        },
    )(rem)
}

pub fn parse_object_lookup_expression(stream: TokenSlice) -> ParsedExpression {
    let (rem, init) = parse_basic_expression(stream)?;
    fold_many0(
        preceded(tag(TokenType::Period), extract_identifier),
        init,
        |acc, property| Expression::ObjectLookupExpression(Box::new(acc), property),
    )(rem)
}
