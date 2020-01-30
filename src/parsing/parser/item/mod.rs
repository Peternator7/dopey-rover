use nom::combinator::map;
use nom::multi::{many0, separated_list};
use nom::sequence::tuple;
use nom::IResult;

use super::statement::Statement;
use super::{extract_identifier, tag};
use crate::parsing::lexer::Token;

pub type ParsedItem<'a> = IResult<&'a [Token], Item>;

pub enum Item {
    Statement(Statement),
    TraitDeclaration(String, Vec<TraitItem>),
}

pub enum TraitItem {
    Property { name: String },
    Function { name: String, args: usize },
}

pub fn parse_trait_declaration(stream: &[Token]) -> ParsedItem {
    map(
        tuple((
            extract_identifier,
            tag(Token::Equals),
            tag(Token::Trait),
            tag(Token::OpenBrace),
            separated_list(tag(Token::SemiColon), parse_trait_item),
            tag(Token::CloseBrace),
        )),
        |(ident, _, _, _, expr, _)| Item::TraitDeclaration(ident, expr),
    )(stream)
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
