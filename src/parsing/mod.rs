pub mod lexer;
pub mod parser;

use nom::multi::many0;
use nom::AsBytes;
use nom_locate;

use serde::Serialize;

type ParsedModule = Vec<parser::Parsed<parser::item::Item>>;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug, Serialize)]
pub struct Position {
    pub line: u32,
    pub column: usize,
}

impl Position {
    pub fn new(line: u32, column: usize) -> Position {
        Position { line, column }
    }
}

impl<T: AsBytes> std::convert::From<nom_locate::LocatedSpan<T>> for Position {
    fn from(span: nom_locate::LocatedSpan<T>) -> Position {
        Position::new(span.line, span.get_column())
    }
}

pub fn parse_module(stream: &str) -> Result<ParsedModule, ()> {
    let stream = nom_locate::LocatedSpan::new(stream);
    if let Ok((_, toks)) = lexer::tokenize(stream) {
        if let Ok((_, output)) = many0(parser::item::parse_item)(&*toks) {
            Ok(output)
        } else {
            Err(())
        }
    } else {
        Err(())
    }
}
