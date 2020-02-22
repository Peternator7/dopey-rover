use super::{Expression, Parsed};
use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "type")]
pub enum Pattern {
    Number { value: f32 },
    StringLiteral { value: String },
    Identifier { value: String },
    Function(FunctionPattern),
    ObjectDestructuring(Vec<Parsed<PropertyPattern>>),
    ConsPattern(ConsPattern),
    NilArrayPattern,
    TestPattern(TestPattern),
}

impl Pattern {
    pub fn is_assignable(&self) -> bool {
        match self {
            Pattern::Identifier { .. } => true,
            Pattern::Function(_) => true,
            Pattern::ObjectDestructuring(props) => !props.is_empty(),
            Pattern::ConsPattern(ConsPattern { head, tail }) => {
                head.data.is_assignable() || tail.data.is_assignable()
            }
            Pattern::TestPattern(TestPattern {
                field_pats: Some(pat),
                ..
            }) => pat.data.is_assignable(),
            _ => false,
        }
    }

    pub fn is_valid_object_property(&self) -> bool {
        match self {
            Pattern::Identifier { .. } => true,
            Pattern::Function(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub enum PropertyPattern {
    Existance(String),
    Test(String, Box<Parsed<Pattern>>),
}

#[derive(Clone, Debug, Serialize)]
pub struct FunctionPattern {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ConsPattern {
    pub head: Box<Parsed<Pattern>>,
    pub tail: Box<Parsed<Pattern>>,
}

#[derive(Clone, Debug, Serialize)]
pub struct TestPattern {
    pub ty_test: Parsed<Expression>,
    pub field_pats: Option<Box<Parsed<Pattern>>>,
}
