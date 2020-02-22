use serde::{Serialize, Serializer};
use std::collections::HashMap;

mod expressions;
mod items;
mod patterns;
mod statements;

pub use expressions::*;
pub use items::*;
pub use patterns::*;
pub use statements::*;

#[derive(Serialize, Clone, Debug, Default)]
pub struct ParsedModule {
    pub objects: HashMap<String, Parsed<Assignment>>,
    pub functions: HashMap<String, Parsed<Assignment>>,
    pub traits: HashMap<String, Parsed<TraitDeclaration>>,
    pub imports: Vec<Parsed<ImportStatement>>,
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
