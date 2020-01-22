pub mod expr;

use nom::branch::alt;
use nom::combinator::{map, opt, value};
use nom::error::ErrorKind;
use nom::multi::{fold_many0, fold_many1, many0, separated_list};
use nom::sequence::{pair, tuple};
use nom::Err;
use nom::IResult;

use crate::parsing::parser::expr::{
    AlgebraicOperator, BooleanOperator, ComparisonOperator, Expression,
};

use crate::parsing::lexer::Token;

type ParsedTree<'a> = IResult<&'a [Token], Expression>;

const TAB_SIZE: usize = 4;

fn tag<'a>(test: Token) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], &'a Token> {
    move |stream: &'a [Token]| match stream.split_first() {
        None => Result::Err(Err::Error((stream, ErrorKind::Tag))),
        Some((a, rem)) if test == *a => Ok((rem, a)),
        _ => Result::Err(Err::Error((stream, ErrorKind::Tag))),
    }
}

pub fn test<'a, F>(test: F) -> impl Fn(&'a [Token]) -> IResult<&'a [Token], &'a Token>
where
    F: Fn(&'a Token) -> bool,
{
    move |stream: &'a [Token]| match stream.split_first() {
        None => Result::Err(Err::Error((stream, ErrorKind::Tag))),
        Some((a, rem)) if test(a) => Ok((rem, a)),
        _ => Result::Err(Err::Error((stream, ErrorKind::Tag))),
    }
}

pub fn parse_term_operator<'a>(stream: &'a [Token]) -> IResult<&'a [Token], AlgebraicOperator> {
    alt((
        value(AlgebraicOperator::Add, tag(Token::Plus)),
        value(AlgebraicOperator::Sub, tag(Token::Minus)),
    ))(stream)
}

pub fn parse_factor_operator<'a>(stream: &'a [Token]) -> IResult<&'a [Token], AlgebraicOperator> {
    alt((
        value(AlgebraicOperator::Divide, tag(Token::Divide)),
        value(AlgebraicOperator::Mult, tag(Token::Multiply)),
    ))(stream)
}

pub fn parse_comparison_operator<'a>(
    stream: &'a [Token],
) -> IResult<&'a [Token], ComparisonOperator> {
    alt((
        value(ComparisonOperator::LessThan, tag(Token::LessThan)),
        value(
            ComparisonOperator::LessThanOrEqualTo,
            tag(Token::LessThanOrEqualTo),
        ),
        value(ComparisonOperator::GreaterThan, tag(Token::GreaterThan)),
        value(
            ComparisonOperator::GreaterThanOrEqualTo,
            tag(Token::GreaterThanOrEqualTo),
        ),
        value(ComparisonOperator::EqualTo, tag(Token::EqualEquals)),
        value(ComparisonOperator::NotEqualTo, tag(Token::NotEquals)),
    ))(stream)
}

pub fn parse_boolean_operator<'a>(stream: &'a [Token]) -> IResult<&'a [Token], BooleanOperator> {
    alt((
        value(BooleanOperator::And, tag(Token::AndAnd)),
        value(BooleanOperator::Or, tag(Token::OrOr)),
    ))(stream)
}

pub fn parse_arithmatic_expression<'a>(stream: &'a [Token]) -> IResult<&'a [Token], Expression> {
    let (rem, init) = parse_arithmatic_term(stream)?;
    fold_many0(
        pair(parse_term_operator, parse_arithmatic_expression),
        init,
        |acc, (op, rhs)| Expression::AlgebraicExpression(op, Box::new(acc), Box::new(rhs)),
    )(rem)
}

pub fn parse_arithmatic_term<'a>(stream: &'a [Token]) -> IResult<&'a [Token], Expression> {
    let (rem, init) = parse_arithmatic_factor(stream)?;
    fold_many0(
        pair(parse_factor_operator, parse_arithmatic_factor),
        init,
        |acc, (op, rhs)| Expression::AlgebraicExpression(op, Box::new(acc), Box::new(rhs)),
    )(rem)
}

pub fn parse_arithmatic_factor<'a>(stream: &'a [Token]) -> IResult<&'a [Token], Expression> {
    alt((parse_bool_literal, parse_new_object, parse_string_literal))(stream)
}

pub fn parse_bool_literal<'a>(stream: &'a [Token]) -> IResult<&'a [Token], Expression> {
    alt((
        value(Expression::Bool(true), tag(Token::True)),
        value(Expression::Bool(false), tag(Token::False)),
    ))(stream)
}

pub fn parse_string_literal<'a>(stream: &'a [Token]) -> IResult<&'a [Token], Expression> {
    map(test(Token::is_string), |tok| match tok {
        Token::StringLiteral(s) => Expression::StringLiteral(s.clone()),
        _ => unreachable!(),
    })(stream)
}

pub fn parse_object_property_list(stream: &[Token]) -> ParsedTree {
    map(
        separated_list(tag(Token::Comma), parse_object_property),
        |props| Expression::NewObject(props.len()),
    )(stream)
}

pub fn parse_object_property<'a>(
    stream: &'a [Token],
) -> IResult<&'a [Token], (String, Expression)> {
    map(
        tuple((
            test(Token::is_ident),
            tag(Token::Colon),
            parse_arithmatic_expression,
        )),
        |(ident, _, expr)| match ident {
            Token::Ident(s) => (s.clone(), expr),
            _ => unreachable!(),
        },
    )(stream)
}

pub fn parse_new_object(stream: &[Token]) -> ParsedTree {
    map(
        tuple((
            tag(Token::New),
            tag(Token::OpenBrace),
            parse_object_property_list,
            tag(Token::CloseBrace),
        )),
        |(_, _, result, _)| result,
    )(stream)
}
