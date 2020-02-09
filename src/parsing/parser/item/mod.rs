use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_nonempty_list};
use nom::sequence::{terminated, tuple};
use nom::IResult;

use super::expression::parse_top_level_expression;
use super::pattern::parse_assignable_pattern;
use super::statement::{parse_assignment, parse_try_statement};
use super::{extract_identifier, tag, TokenSlice};
use super::{Assignment, TryStatement};
use crate::parsing::lexer::{Token, TokenType};

pub type ParsedItem<'a> = IResult<TokenSlice<'a>, Item>;

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Assignment(Assignment),
    TryStatement(TryStatement),
    TraitDeclaration(String, Vec<TraitItem>),
    ModuleImport,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TraitItem {
    Property { name: String },
    Function { name: String, args: usize },
}

pub fn parse_item<'a>(stream: TokenSlice<'a>) -> ParsedItem<'a> {
    alt((
        parse_trait_declaration,
        map(
            terminated(
                parse_assignment(parse_assignable_pattern, parse_top_level_expression),
                tag(TokenType::SemiColon),
            ),
            Item::Assignment,
        ),
        map(parse_try_statement, Item::TryStatement),
    ))(stream)
}

fn parse_trait_declaration<'a>(stream: TokenSlice<'a>) -> ParsedItem<'a> {
    map(
        tuple((
            extract_identifier,
            tag(TokenType::Equals),
            tag(TokenType::Trait),
            tag(TokenType::OpenBrace),
            parse_trait_item_list,
            tag(TokenType::CloseBrace),
            tag(TokenType::SemiColon),
        )),
        |(ident, _, _, _, expr, _, _)| Item::TraitDeclaration(ident, expr),
    )(stream)
}

fn parse_trait_item_list(stream: TokenSlice) -> IResult<TokenSlice, Vec<TraitItem>> {
    terminated(
        separated_nonempty_list(tag(TokenType::Comma), parse_trait_item),
        opt(tag(TokenType::Comma)),
    )(stream)
    .or_else(|_| Ok((stream, Vec::new())))
}

fn parse_trait_item<'a>(stream: TokenSlice<'a>) -> IResult<TokenSlice<'a>, TraitItem> {
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
