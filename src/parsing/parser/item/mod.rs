use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_nonempty_list};
use nom::sequence::{terminated, tuple};
use nom::IResult;

use serde::Serialize;

use super::expression::parse_top_level_expression;
use super::pattern::parse_assignable_pattern;
use super::statement::{parse_assignment, parse_try_statement};
use super::statement::{Assignment, TryStatement};
use super::{extract_identifier, tag, Parsed, TokenSlice};
use crate::parsing::lexer::TokenType;

pub type ParsedItem<'a> = IResult<TokenSlice<'a>, Parsed<Item>>;

#[derive(Debug, Clone, Serialize)]
pub enum Item {
    Assignment(Assignment),
    TryStatement(TryStatement),
    TraitDeclaration(String, Vec<TraitItem>),
    ModuleImport,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub enum TraitItem {
    Property { name: String },
    Function { name: String, args: usize },
}

pub fn parse_item(stream: TokenSlice) -> ParsedItem {
    alt((
        parse_trait_declaration,
        map(
            tuple((
                parse_assignment(parse_assignable_pattern, parse_top_level_expression),
                tag(TokenType::SemiColon),
            )),
            |(stmt, end)| {
                Parsed::new(
                    Item::Assignment(stmt.data),
                    stmt.start_pos,
                    Some(end.end_pos),
                )
            },
        ),
        map(parse_try_statement, |t| {
            Parsed::new(Item::TryStatement(t.data), t.start_pos, t.end_pos)
        }),
    ))(stream)
}

fn parse_trait_declaration(stream: TokenSlice) -> ParsedItem {
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
        |(ident, _, _, _, expr, _, end)| {
            Parsed::new(
                Item::TraitDeclaration(ident.data, expr),
                ident.start_pos,
                Some(end.end_pos),
            )
        },
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
                TraitItem::Property { name: ident.data }
            } else {
                TraitItem::Function {
                    name: ident.data,
                    args: args.len(),
                }
            }
        },
    )(stream)
}
