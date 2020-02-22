pub mod lexer;
pub mod parser;

use super::types::*;

pub use std::collections::HashMap;

use nom::multi::many0;
use nom::AsBytes;
use nom_locate;

impl<T: AsBytes> std::convert::From<nom_locate::LocatedSpan<T>> for Position {
    fn from(span: nom_locate::LocatedSpan<T>) -> Position {
        Position::new(span.line, span.get_column())
    }
}

pub fn parse_module(name: &str, stream: &str) -> Result<ParsedModule, ()> {
    let stream = nom_locate::LocatedSpan::new(stream);
    if let Ok((_, toks)) = lexer::tokenize(stream) {
        if let Ok((_, items)) = many0(parser::item::parse_item)(&*toks) {
            let mut objects = HashMap::new();
            let mut functions = HashMap::new();
            let mut traits = HashMap::new();
            let mut imports = Vec::new();
            let sub_modules = HashMap::new();

            for item in items {
                let start_pos = item.start_pos;
                let end_pos = item.end_pos;
                match item.data {
                    Item::Assignment(data) => match data.lhs.data {
                        Pattern::Identifier { ref value } => {
                            objects.insert(
                                value.clone(),
                                Parsed {
                                    data,
                                    start_pos,
                                    end_pos,
                                },
                            );
                        }
                        Pattern::Function(ref func) => {
                            functions.insert(
                                func.name.clone(),
                                Parsed {
                                    data,
                                    start_pos,
                                    end_pos,
                                },
                            );
                        }
                        _ => unreachable!(),
                    },
                    Item::TraitDeclaration(data) => {
                        traits.insert(
                            data.name.clone(),
                            Parsed {
                                data,
                                start_pos,
                                end_pos,
                            },
                        );
                    }
                    Item::ModuleImport(data) => imports.push(Parsed {
                        data,
                        start_pos,
                        end_pos,
                    }),
                }
            }

            Ok(ParsedModule {
                name: name.to_string(),
                objects,
                functions,
                traits,
                imports,
                sub_modules,
            })
        } else {
            Err(())
        }
    } else {
        Err(())
    }
}
