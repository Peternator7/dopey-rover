use serde::Serialize;
use std::collections::HashMap;

mod expressions;
mod patterns;

pub use expressions::*;
pub use patterns::*;

// High level constructs.

pub struct ParsedModule {
    pub objects: HashMap<String, Assignment>,
    pub functions: HashMap<String, Assignment>,
    pub traits: std::collections::HashMap<String, String>,
}

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

// Parsing types

#[derive(Debug, Clone, Serialize)]
pub struct Parsed<T> {
    pub start_pos: Position,
    pub end_pos: Option<Position>,
    pub data: T,
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

// Statement like items.

#[derive(Clone, Debug, Serialize)]
pub struct Assignment {
    pub lhs: Parsed<Pattern>,
    pub rhs: Parsed<Expression>,
}

#[derive(Clone, Debug, Serialize)]
pub struct TryStatement(pub Expression);

#[derive(Clone, Debug, Serialize)]
pub enum ImportStatement {
    ModuleLevel {
        path_segments: Vec<String>,
    },
    ItemLevel {
        path_segments: Vec<String>,
        items: Vec<String>,
    },
}

#[derive(Clone, Debug, Serialize)]
pub enum Statement {
    Assignment(Assignment),
    TryStatement(TryStatement),
    SetStatement(Parsed<Expression>, Parsed<Expression>),
    ModuleImport,
}
