use nom::branch::alt;
use nom::combinator::{map, opt, verify};
use nom::multi::{fold_many0, many0, separated_list};
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use super::{extract_identifier, tag, test, Parsed, TokenSlice};
use crate::parsing::lexer::TokenType;
use crate::parsing::parser::expression::parse_object_lookup_expression;
use crate::parsing::parser::expression::Expression;
use crate::parsing::Position;

pub type ParsedPattern<'a> = IResult<TokenSlice<'a>, Parsed<Pattern>>;

#[derive(Clone, Debug)]
pub enum Pattern {
    Number(f32),
    StringLiteral(String),
    Identifier(String),
    Function(Parsed<String>, Vec<Parsed<String>>),
    ObjectDestructuring(Vec<Parsed<PropertyPattern>>),
    ConsPattern(Box<Parsed<Pattern>>, Box<Parsed<Pattern>>),
    NilArrayPattern,
    TestPattern(Parsed<Expression>, Option<Box<Parsed<Pattern>>>),
}

impl Pattern {
    pub fn is_assignable(&self) -> bool {
        match self {
            Pattern::Identifier(_) => true,
            Pattern::Function(_, _) => true,
            Pattern::ObjectDestructuring(props) => !props.is_empty(),
            Pattern::ConsPattern(head, tail) => {
                head.data.is_assignable() || tail.data.is_assignable()
            }
            Pattern::TestPattern(_, Some(pat)) => pat.data.is_assignable(),
            _ => false,
        }
    }

    pub fn is_valid_object_property(&self) -> bool {
        match self {
            Pattern::Identifier(_) => true,
            Pattern::Function(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum PropertyPattern {
    Existance(String),
    Test(Parsed<String>, Box<Parsed<Pattern>>),
}

pub fn parse_assignable_pattern(stream: TokenSlice) -> ParsedPattern {
    verify(parse_top_level_pattern, |p| p.data.is_assignable())(stream)
}

pub fn parse_object_creation_property_pattern(stream: TokenSlice) -> ParsedPattern {
    verify(parse_top_level_pattern, |p| {
        p.data.is_valid_object_property()
    })(stream)
}

pub fn parse_top_level_pattern(stream: TokenSlice) -> ParsedPattern {
    parse_head_pattern(stream)
}

pub fn parse_head_pattern(stream: TokenSlice) -> ParsedPattern {
    let (rem, init) = parse_basic_pattern(stream)?;
    fold_many0(
        pair(tag(TokenType::DoubleColon), parse_head_pattern),
        init,
        |acc, (_, rhs)| {
            let start_pos = acc.start_pos;
            let end_pos = rhs.end_pos;
            Parsed::new(
                Pattern::ConsPattern(Box::new(acc), Box::new(rhs)),
                start_pos,
                end_pos,
            )
        },
    )(rem)
}

pub fn parse_basic_pattern(stream: TokenSlice) -> ParsedPattern {
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

pub fn parse_string_literal_pattern(stream: TokenSlice) -> ParsedPattern {
    map(test(TokenType::is_string_literal), |tok| {
        if let TokenType::StringLiteral(s) = tok.ty {
            Parsed::new(
                Pattern::StringLiteral(s.to_string()),
                tok.start_pos,
                Some(tok.end_pos),
            )
        } else {
            unreachable!()
        }
    })(stream)
}

fn parse_number_literal_pattern(stream: TokenSlice) -> ParsedPattern {
    map(test(TokenType::is_number), |tok| {
        if let TokenType::Number(f) = tok.ty {
            Parsed::new(Pattern::Number(f), tok.start_pos, Some(tok.end_pos))
        } else {
            unreachable!()
        }
    })(stream)
}

fn parse_test_pattern(stream: TokenSlice) -> ParsedPattern {
    map(
        tuple((
            tag(TokenType::QuestionMark),
            parse_object_lookup_expression,
            opt(parse_basic_pattern),
        )),
        |(start, parent, unpacked)| {
            let end_pos = unpacked
                .as_ref()
                .map(|patt| patt.end_pos)
                .unwrap_or(parent.end_pos);
            Parsed::new(
                Pattern::TestPattern(parent, unpacked.map(Box::new)),
                start.start_pos,
                end_pos,
            )
        },
    )(stream)
}

fn parse_ident_pattern(stream: TokenSlice) -> ParsedPattern {
    map(
        pair(extract_identifier, many0(extract_identifier)),
        |(ident, args)| {
            let start_pos = ident.start_pos;
            if !args.is_empty() {
                let end_pos = args.last().expect("We just checked it's not empty").end_pos;
                Parsed::new(Pattern::Function(ident, args), start_pos, end_pos)
            } else {
                Parsed::new(Pattern::Identifier(ident.data), start_pos, ident.end_pos)
            }
        },
    )(stream)
}

fn parse_object_property_pattern(
    stream: TokenSlice,
) -> IResult<TokenSlice, Parsed<PropertyPattern>> {
    map(
        tuple((
            extract_identifier,
            opt(preceded(tag(TokenType::Colon), parse_top_level_pattern)),
        )),
        |(ident, patt)| match patt {
            Some(patt) => {
                let start_pos = ident.start_pos;
                let end_pos = patt.end_pos;
                Parsed::new(
                    PropertyPattern::Test(ident, Box::new(patt)),
                    start_pos,
                    end_pos,
                )
            }
            None => Parsed::new(
                PropertyPattern::Existance(ident.data),
                ident.start_pos,
                ident.end_pos,
            ),
        },
    )(stream)
}

fn parse_object_pattern(stream: TokenSlice) -> ParsedPattern {
    map(
        tuple((
            tag(TokenType::OpenBrace),
            separated_list(tag(TokenType::Comma), parse_object_property_pattern),
            tag(TokenType::CloseBrace),
        )),
        |(start, props, end)| {
            Parsed::new(
                Pattern::ObjectDestructuring(props),
                start.start_pos,
                Some(end.end_pos),
            )
        },
    )(stream)
}

fn parse_array_pattern(stream: TokenSlice) -> ParsedPattern {
    fn parse_array_pattern_inner(stream: TokenSlice) -> ParsedPattern {
        use Pattern::*;

        let res = parse_top_level_pattern(stream);
        if res.is_err() {
            let nil = Parsed::new(NilArrayPattern, Position::new(0, 0), None);
            return Ok((stream, nil));
        }

        let (rem, head) = res.unwrap();
        let head = Box::new(head);

        let res = preceded(tag(TokenType::Comma), parse_array_pattern_inner)(rem);
        if let Ok((rem, tail)) = res {
            let start_pos = head.start_pos;
            let end_pos = tail.end_pos;
            let output = Parsed::new(ConsPattern(head, Box::new(tail)), start_pos, end_pos);
            Ok((rem, output))
        } else {
            let start_pos = head.start_pos;
            let end_pos = head.end_pos;
            let nil = Parsed::new(NilArrayPattern, Position::new(0, 0), None);
            let output = Parsed::new(ConsPattern(head, Box::new(nil)), start_pos, end_pos);
            Ok((rem, output))
        }
    }

    map(
        tuple((
            tag(TokenType::OpenBracket),
            parse_array_pattern_inner,
            tag(TokenType::CloseBracket),
        )),
        |(_, elements, _)| elements,
    )(stream)
}
