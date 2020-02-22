// Statement like items.
use super::{Expression, Parsed, Pattern};
use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
pub struct Assignment<T> {
    pub lhs: Parsed<T>,
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
#[serde(tag = "type")]
pub enum Statement {
    Assignment(Assignment<Pattern>),
    TryStatement(TryStatement),
    SetStatement {
        trait_object: Parsed<Expression>,
        target: Parsed<Expression>,
    },
    ModuleImport(ImportStatement),
}
