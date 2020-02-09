pub mod lexer;
pub mod parser;

use nom::multi::many0;
use nom_locate;

type ParsedModule = Vec<parser::item::Item>;

pub fn parse_module<'a>(stream: &'a str) -> Result<ParsedModule, ()> {
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
