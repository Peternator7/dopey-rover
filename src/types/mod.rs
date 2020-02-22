use serde::{Serialize, Serializer};
use std::collections::HashMap;

mod error;
mod expressions;
mod items;
mod patterns;
mod statements;

pub use error::*;
pub use expressions::*;
pub use items::*;
pub use patterns::*;
pub use statements::*;

#[derive(Serialize, Clone, Debug, Default)]
pub struct ParsedModule {
    pub name: String,
    pub objects: HashMap<String, Parsed<Assignment>>,
    pub functions: HashMap<String, Parsed<Assignment>>,
    pub traits: HashMap<String, Parsed<TraitDeclaration>>,
    pub imports: Vec<Parsed<ImportStatement>>,
    pub sub_modules: HashMap<String, ParsedModule>,
}

enum ItemLookUpError<'a> {
    SymbolNotFound,
    ModuleNotFound(&'a str),
}

impl ParsedModule {
    pub fn find_trait<'a>(
        &'a self,
        module_path: &'a str,
        trait_name: &'a str,
    ) -> Result<&Parsed<TraitDeclaration>, CompilationError> {
        self.find_inner(module_path.split('.'), |module| {
            module.traits.get(trait_name)
        })
        .map_err(|err| match err {
            ItemLookUpError::SymbolNotFound => CompilationError::TraitNotFound {
                module: module_path.to_string(),
                trait_name: trait_name.to_string(),
            },
            ItemLookUpError::ModuleNotFound(sub_module) => CompilationError::SubModuleNotFound {
                module: module_path.to_string(),
                sub_module: sub_module.to_string(),
            },
        })
    }

    pub fn find_symbol<'a>(
        &'a self,
        module_path: &'a str,
        symbol: &'a str,
    ) -> Result<&Parsed<Assignment>, CompilationError> {
        self.find_inner(module_path.split('.'), |module| {
            module
                .objects
                .get(symbol)
                .or_else(|| module.functions.get(symbol))
        })
        .map_err(|err| match err {
            ItemLookUpError::SymbolNotFound => CompilationError::SymbolNotFound {
                module: module_path.to_string(),
                symbol: symbol.to_string(),
            },
            ItemLookUpError::ModuleNotFound(sub_module) => CompilationError::SubModuleNotFound {
                module: module_path.to_string(),
                sub_module: sub_module.to_string(),
            },
        })
    }

    fn find_inner<'a, T>(
        &'a self,
        mut it: impl std::iter::Iterator<Item = &'a str>,
        lookup: impl FnOnce(&ParsedModule) -> Option<&Parsed<T>>,
    ) -> Result<&'a Parsed<T>, ItemLookUpError> {
        if let Some(segment) = it.next() {
            self.sub_modules
                .get(segment)
                .ok_or_else(|| ItemLookUpError::ModuleNotFound(segment))
                .and_then(|module| module.find_inner(it, lookup))
        } else {
            lookup(&self).ok_or_else(|| ItemLookUpError::SymbolNotFound)
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub struct Position {
    pub line: u32,
    pub column: usize,
}

impl Position {
    pub fn new(line: u32, column: usize) -> Position {
        Position { line, column }
    }
}

impl Serialize for Position {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&*format!("line:{}, column: {}", self.line, self.column))
    }
}

// Parsing types

#[derive(Debug, Clone, Serialize)]
pub struct Parsed<T> {
    pub start_pos: Position,
    pub end_pos: Option<Position>,
    #[serde(flatten)]
    pub data: T,
}

impl<T> std::ops::Deref for Parsed<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> std::ops::DerefMut for Parsed<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> Parsed<T> {
    pub fn new(data: T, start_pos: Position, end_pos: Option<Position>) -> Parsed<T> {
        Parsed {
            data,
            start_pos,
            end_pos,
        }
    }

    pub fn map<F, U>(self, mapping: F) -> Parsed<U>
    where
        F: FnOnce(T) -> U,
    {
        Parsed {
            start_pos: self.start_pos,
            end_pos: self.end_pos,
            data: mapping(self.data),
        }
    }
}
