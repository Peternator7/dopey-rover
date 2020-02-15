pub mod basic_expressions;
pub mod block;

use nom::branch::alt;
use nom::combinator::{map, opt, value};
use nom::multi::fold_many0;
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use crate::parsing::{Assignment, BinaryExpression, BinaryOperator, Expression, Parsed, Statement};

use super::{extract_identifier, tag, TokenSlice};

use crate::parsing::lexer::TokenType;

use self::basic_expressions::parse_basic_expression;

pub type ParsedExpression<'a> = IResult<TokenSlice<'a>, Parsed<Expression>>;

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
        TestExpr(Parsed<Expression>),
        Return(Parsed<Option<Box<Expression>>>),
    }

    loop {
        let options = alt((
            map(parse_test_expression, Rhs::TestExpr),
            map(
                tuple((tag(TokenType::Return), opt(parse_test_expression))),
                |(start, expr)| {
                    let start_pos = start.start_pos;
                    if let Some(expr) = expr {
                        let data = Some(Box::new(expr.data));
                        Rhs::Return(Parsed::new(data, start_pos, expr.end_pos))
                    } else {
                        Rhs::Return(Parsed::new(None, start_pos, Some(start.end_pos)))
                    }
                },
            ),
        ));

        match pair(parse_boolean_operator, options)(remainder) {
            Ok((rem, (op, Rhs::TestExpr(rhs)))) => {
                remainder = rem;
                let start_pos = output.start_pos;
                let end_pos = rhs.end_pos;
                output = Parsed::new(
                    Expression::BinaryExpression(Box::new(BinaryExpression::new(op, output, rhs))),
                    start_pos,
                    end_pos,
                )
            }
            Ok((rem, (op, Rhs::Return(rhs)))) => {
                remainder = rem;
                let start_pos = output.start_pos;
                let end_pos = rhs.end_pos;
                output = Parsed::new(
                    Expression::BooleanReturnExpression(op, Box::new(output), rhs),
                    start_pos,
                    end_pos,
                );
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
                let start_pos = lhs.start_pos;
                let end_pos = rhs.end_pos;
                Parsed::new(
                    Expression::BinaryExpression(Box::new(BinaryExpression::new(
                        BinaryOperator::Test,
                        lhs,
                        rhs,
                    ))),
                    start_pos,
                    end_pos,
                )
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
                let start_pos = lhs.start_pos;
                let end_pos = rhs.end_pos;
                Parsed::new(
                    Expression::BinaryExpression(Box::new(BinaryExpression::new(op, lhs, rhs))),
                    start_pos,
                    end_pos,
                )
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
            let start_pos = acc.start_pos;
            let end_pos = rhs.end_pos;
            Parsed::new(
                Expression::BinaryExpression(Box::new(BinaryExpression::new(op, acc, rhs))),
                start_pos,
                end_pos,
            )
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
            let start_pos = acc.start_pos;
            let end_pos = rhs.end_pos;
            Parsed::new(
                Expression::BinaryExpression(Box::new(BinaryExpression::new(op, acc, rhs))),
                start_pos,
                end_pos,
            )
        },
    )(rem)
}

fn parse_arithmatic_factor(stream: TokenSlice) -> ParsedExpression {
    parse_call_expression(stream)
}

fn parse_call_expression(stream: TokenSlice) -> ParsedExpression {
    let (rem, init) = parse_call_lhs_expression(stream)?;
    fold_many0(parse_call_lhs_expression, init, |acc, arg| {
        let start_pos = acc.start_pos;
        let end_pos = arg.end_pos;
        Parsed::new(
            Expression::BinaryExpression(Box::new(BinaryExpression {
                op: BinaryOperator::Call,
                lhs: acc,
                rhs: arg,
            })),
            start_pos,
            end_pos,
        )
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
            let start_pos = acc.start_pos;
            let end_pos = rhs.end_pos;
            Parsed::new(
                Expression::BinaryExpression(Box::new(BinaryExpression {
                    op: BinaryOperator::Cons,
                    lhs: acc,
                    rhs,
                })),
                start_pos,
                end_pos,
            )
        },
    )(rem)
}

pub fn parse_object_lookup_expression(stream: TokenSlice) -> ParsedExpression {
    let (rem, init) = parse_basic_expression(stream)?;
    fold_many0(
        preceded(tag(TokenType::Period), extract_identifier),
        init,
        |acc, property| {
            let start_pos = acc.start_pos;
            let end_pos = property.end_pos;
            Parsed::new(
                Expression::ObjectLookupExpression(Box::new(acc), property),
                start_pos,
                end_pos,
            )
        },
    )(rem)
}
