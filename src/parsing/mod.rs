pub mod lexer;
pub mod parser;
pub mod types;

pub use types::*;

pub use std::collections::HashMap;

use nom::multi::many0;
use nom::AsBytes;
use nom_locate;

impl<T: AsBytes> std::convert::From<nom_locate::LocatedSpan<T>> for Position {
    fn from(span: nom_locate::LocatedSpan<T>) -> Position {
        Position::new(span.line, span.get_column())
    }
}

pub fn parse_module(stream: &str) -> Result<ParsedModule, ()> {
    let stream = nom_locate::LocatedSpan::new(stream);
    if let Ok((_, toks)) = lexer::tokenize(stream) {
        if let Ok((_, items)) = many0(parser::item::parse_item)(&*toks) {
            let objects = HashMap::new();
            let functions = HashMap::new();
            let traits = HashMap::new();

            Ok(ParsedModule {
                objects,
                functions,
                traits,
            })
        } else {
            Err(())
        }
    } else {
        Err(())
    }
}
