use serde::Serialize;
use super::{Parsed, Expression};

#[derive(Clone, Debug, Serialize)]
pub enum Pattern {
    Number(f32),
    StringLiteral(String),
    Identifier(String),
    Function(Parsed<String>, Vec<Parsed<String>>),
    ObjectDestructuring(Vec<Parsed<PropertyPattern>>),
    ConsPattern(Box<Parsed<Pattern>>, Box<Parsed<Pattern>>),
    NilArrayPattern,
    TestPattern(Parsed<Expression>, Option<Box<Parsed<Pattern>>>),
}

impl Pattern {
    pub fn is_assignable(&self) -> bool {
        match self {
            Pattern::Identifier(_) => true,
            Pattern::Function(_, _) => true,
            Pattern::ObjectDestructuring(props) => !props.is_empty(),
            Pattern::ConsPattern(head, tail) => {
                head.data.is_assignable() || tail.data.is_assignable()
            }
            Pattern::TestPattern(_, Some(pat)) => pat.data.is_assignable(),
            _ => false,
        }
    }

    pub fn is_valid_object_property(&self) -> bool {
        match self {
            Pattern::Identifier(_) => true,
            Pattern::Function(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub enum PropertyPattern {
    Existance(String),
    Test(Parsed<String>, Box<Parsed<Pattern>>),
}
