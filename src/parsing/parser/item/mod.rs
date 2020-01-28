use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list};
use nom::sequence::{pair, preceded, tuple};
use nom::IResult;

use super::{extract_identifier, tag, test};
use crate::parsing::lexer::Token;
use crate::parsing::parser::expression::parse_top_level_expression;
use crate::parsing::parser::expression::Expression;

pub type ParsedItem<'a> = IResult<&'a [Token], Item>;

pub enum Item {
    Unknown,
    FunctionDeclaration(String, Vec<String>, Expression),
    ObjectDeclaration(String, Expression),
    ModuleImport,
    TraitDeclaration(String, Vec<TraitItem>),
    TraitObjectDeclaration(String),
}

pub enum TraitItem {
    Property { name: String },
    Function { name: String, args: usize },
}

pub fn parse_object(stream: &[Token]) -> ParsedItem {
    map(
        tuple((
            extract_identifier,
            tag(Token::Equals),
            parse_top_level_expression,
        )),
        |(ident, _, expr)| Item::ObjectDeclaration(ident, expr),
    )(stream)
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
