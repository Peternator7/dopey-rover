pub mod lexer;
pub mod parser;

use nom::multi::many0;

type ParsedModule = Vec<parser::item::Item>;

pub fn parse_module(stream: &str) -> Result<ParsedModule, ()> {
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
