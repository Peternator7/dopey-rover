use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{fold_many0, separated_list};
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use super::{extract_identifier, tag, test};
use crate::parsing::lexer::Token;
use crate::parsing::parser::expression::parse_object_lookup_expression;
use crate::parsing::parser::expression::Expression;

pub type ParsedPattern<'a> = IResult<&'a [Token], Pattern>;

#[derive(Clone, PartialEq, Debug)]
pub enum Pattern {
    Number(f32),
    StringLiteral(String),
    Identifier(String),
    ObjectDestructuring(Vec<PropertyPattern>),
    ConsPattern(Box<Pattern>, Box<Pattern>),
    NilArrayPattern,
    TestPattern(Expression, Option<Box<Pattern>>),
}

impl Pattern {
    pub fn is_assignable(&self) -> bool {
        match self {
            Pattern::Number(_) => false,
            Pattern::StringLiteral(_) => false,
            Pattern::Identifier(_) => true,
            Pattern::ObjectDestructuring(props) => !props.is_empty(),
            Pattern::ConsPattern(head, tail) => head.is_assignable() || tail.is_assignable(),
            Pattern::NilArrayPattern => false,
            Pattern::TestPattern(_, None) => false,
            Pattern::TestPattern(_, Some(pat)) => pat.is_assignable(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum PropertyPattern {
    Existance(String),
    Test(String, Box<Pattern>),
}

pub fn parse_top_level_pattern(stream: &[Token]) -> ParsedPattern {
    parse_head_pattern(stream)
}

pub fn parse_head_pattern(stream: &[Token]) -> ParsedPattern {
    let (rem, init) = parse_basic_pattern(stream)?;
    fold_many0(
        pair(tag(Token::DoubleColon), parse_head_pattern),
        init,
        |acc, (_, rhs)| Pattern::ConsPattern(Box::new(acc), Box::new(rhs)),
    )(rem)
}

pub fn parse_basic_pattern(stream: &[Token]) -> ParsedPattern {
    // In reality, we shouldn't let you 'return' multiple times
    // False || return True || return True
    // is nonsensical, but we can figure that out later. It's dead code
    alt((
        parse_ident_pattern,
        parse_object_pattern,
        parse_test_pattern,
        parse_string_literal_pattern,
        parse_number_literal_pattern,
        parse_array_pattern,
    ))(stream)
}

pub fn parse_string_literal_pattern(stream: &[Token]) -> ParsedPattern {
    map(test(Token::is_string_literal), |tok| {
        if let Token::StringLiteral(s) = tok {
            Pattern::StringLiteral(s.clone())
        } else {
            unreachable!()
        }
    })(stream)
}

pub fn parse_number_literal_pattern(stream: &[Token]) -> ParsedPattern {
    map(test(Token::is_number), |tok| {
        if let Token::Number(f) = tok {
            Pattern::Number(*f)
        } else {
            unreachable!()
        }
    })(stream)
}

pub fn parse_test_pattern(stream: &[Token]) -> ParsedPattern {
    map(
        tuple((
            tag(Token::QuestionMark),
            parse_object_lookup_expression(false),
            opt(parse_basic_pattern),
        )),
        |(_, parent, unpacked)| Pattern::TestPattern(parent, unpacked.map(Box::new)),
    )(stream)
}

pub fn parse_ident_pattern(stream: &[Token]) -> ParsedPattern {
    map(extract_identifier, Pattern::Identifier)(stream)
}

pub fn parse_object_property_pattern(stream: &[Token]) -> IResult<&[Token], PropertyPattern> {
    map(
        tuple((
            extract_identifier,
            opt(preceded(tag(Token::Colon), parse_top_level_pattern)),
        )),
        |(ident, patt)| match patt {
            Some(patt) => PropertyPattern::Test(ident, Box::new(patt)),
            None => PropertyPattern::Existance(ident),
        },
    )(stream)
}

pub fn parse_object_pattern(stream: &[Token]) -> ParsedPattern {
    map(
        tuple((
            tag(Token::OpenBrace),
            separated_list(tag(Token::Comma), parse_object_property_pattern),
            tag(Token::CloseBrace),
        )),
        |(_, props, _)| Pattern::ObjectDestructuring(props),
    )(stream)
}

pub fn parse_array_pattern(stream: &[Token]) -> ParsedPattern {
    fn parse_array_pattern_inner(stream: &[Token]) -> ParsedPattern {
        use Pattern::*;

        let res = parse_top_level_pattern(stream);
        if res.is_err() {
            return Ok((stream, NilArrayPattern));
        }

        let (rem, head) = res.unwrap();
        let res = preceded(tag(Token::Comma), parse_array_pattern_inner)(rem);
        if let Ok((rem, tail)) = res {
            Ok((rem, ConsPattern(Box::new(head), Box::new(tail))))
        } else {
            Ok((rem, ConsPattern(Box::new(head), Box::new(NilArrayPattern))))
        }
    }

    map(
        tuple((
            tag(Token::OpenBracket),
            parse_array_pattern_inner,
            tag(Token::CloseBracket),
        )),
        |(_, elements, _)| elements,
    )(stream)
}
