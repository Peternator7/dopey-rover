
use super::{Parsed, Assignment, Statement};
use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
pub enum Expression {
    Number(f32),
    StringLiteral(String),
    NewObject(Vec<Parsed<Assignment>>),
    NilArrayExpression,
    Variable(String),
    BinaryExpression(Box<BinaryExpression>),
    BooleanReturnExpression(
        BinaryOperator,
        Box<Parsed<Expression>>,
        Parsed<Option<Box<Expression>>>,
    ),
    GetExpression(String, Box<Parsed<Expression>>),
    ObjectLookupExpression(Box<Parsed<Expression>>, Parsed<String>),
    IfElseExpression(
        Box<Parsed<Expression>>,
        Box<Parsed<Expression>>,
        Box<Parsed<Expression>>,
    ),
    MatchExpression(Box<Parsed<Expression>>, Vec<()>),
    BlockExpression(Vec<Parsed<Statement>>, Option<Box<Parsed<Expression>>>),
    IfLetExpression,
}

#[derive(Clone, Debug, Serialize)]
pub struct BinaryExpression {
    pub op: BinaryOperator,
    pub lhs: Parsed<Expression>,
    pub rhs: Parsed<Expression>,
}

impl BinaryExpression {
    pub fn new(
        op: BinaryOperator,
        lhs: Parsed<Expression>,
        rhs: Parsed<Expression>,
    ) -> BinaryExpression {
        BinaryExpression { op, lhs, rhs }
    }
}

impl Expression {
    pub fn requires_trailing_semicolon(&self) -> bool {
        match self {
            Expression::IfElseExpression(_, _, _) => false,
            Expression::IfLetExpression => false,
            Expression::MatchExpression(_, _) => false,
            Expression::BlockExpression(_, _) => false,
            _ => true,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Divide,
    Modulus,

    And,
    Or,

    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,
    EqualTo,
    NotEqualTo,

    Test,
    Cons,
    Call,
}
