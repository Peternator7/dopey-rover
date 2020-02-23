use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_nonempty_list};
use nom::sequence::{terminated, tuple};
use nom::IResult;

use super::expression::parse_top_level_expression;
use super::pattern::parse_item_pattern;
use super::statement::parse_assignment;
use super::{extract_identifier, tag, TokenSlice};
use crate::parsing::{
    lexer::TokenType, Assignment, Item, ItemPattern, Parsed, TraitDeclaration, TraitItem,
};

pub type ParsedItem<'a> = IResult<TokenSlice<'a>, Parsed<Item>>;

pub fn parse_item(stream: TokenSlice) -> ParsedItem {
    alt((
        parse_trait_declaration,
        map(
            tuple((
                parse_assignment(parse_item_pattern, parse_top_level_expression),
                tag(TokenType::SemiColon),
            )),
            |(stmt, end)| {
                let start_pos = stmt.start_pos;
                let end_pos = Some(end.end_pos);
                let rhs = stmt.data.rhs;
                let lhs = stmt.data.lhs;
                match lhs.data {
                    ItemPattern::Function(data) => {
                        let data = Assignment {
                            lhs: Parsed::new(data, lhs.start_pos, lhs.end_pos),
                            rhs,
                        };
                        Parsed::new(Item::FunctionDeclaration(data), start_pos, end_pos)
                    }
                    ItemPattern::Identifier { value } => {
                        let data = Assignment {
                            lhs: Parsed::new(value, lhs.start_pos, lhs.end_pos),
                            rhs,
                        };
                        Parsed::new(Item::ObjectDeclaration(data), start_pos, end_pos)
                    }
                }
            },
        ),
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
                Item::TraitDeclaration(TraitDeclaration {
                    name: ident.data,
                    items: expr,
                }),
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
