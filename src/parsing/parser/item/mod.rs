use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_nonempty_list};
use nom::sequence::{terminated, tuple};
use nom::IResult;

use super::statement::{parse_statement, Statement};
use super::{extract_identifier, tag};
use crate::parsing::lexer::Token;

pub type ParsedItem<'a> = IResult<&'a [Token], Item>;

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Statement(Statement),
    TraitDeclaration(String, Vec<TraitItem>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TraitItem {
    Property { name: String },
    Function { name: String, args: usize },
}

pub fn parse_item(stream: &[Token]) -> ParsedItem {
    alt((
        parse_trait_declaration,
        map(parse_statement, Item::Statement),
    ))(stream)
}

fn parse_trait_declaration(stream: &[Token]) -> ParsedItem {
    map(
        tuple((
            extract_identifier,
            tag(Token::Equals),
            tag(Token::Trait),
            tag(Token::OpenBrace),
            parse_trait_item_list,
            tag(Token::CloseBrace),
            tag(Token::SemiColon),
        )),
        |(ident, _, _, _, expr, _, _)| Item::TraitDeclaration(ident, expr),
    )(stream)
}

fn parse_trait_item_list(stream: &[Token]) -> IResult<&[Token], Vec<TraitItem>> {
    terminated(
        separated_nonempty_list(tag(Token::Comma), parse_trait_item),
        opt(tag(Token::Comma)),
    )(stream)
    .or_else(|_| Ok((stream, Vec::new())))
}

fn parse_trait_item(stream: &[Token]) -> IResult<&[Token], TraitItem> {
    map(
        tuple((extract_identifier, many0(extract_identifier))),
        |(ident, args)| {
            if args.is_empty() {
                TraitItem::Property { name: ident }
            } else {
                TraitItem::Function {
                    name: ident,
                    args: args.len(),
                }
            }
        },
    )(stream)
}
