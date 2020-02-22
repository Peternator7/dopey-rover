use super::{Assignment, Parsed, Statement};
use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "type")]
pub enum Expression {
    Number {
        value: f32,
    },
    StringLiteral {
        value: String,
    },
    NewObject {
        fields: Vec<Parsed<Assignment>>,
    },
    NilArrayExpression,
    Variable {
        value: String,
    },
    BinaryExpression {
        op: BinaryOperator,
        lhs: Box<Parsed<Expression>>,
        rhs: Box<Parsed<Expression>>,
    },
    BooleanReturnExpression {
        op: BinaryOperator,
        lhs: Box<Parsed<Expression>>,
        rhs: Parsed<Option<Box<Expression>>>,
    },
    GetExpression {
        trait_name: String,
        object: Box<Parsed<Expression>>,
    },
    ObjectLookupExpression {
        object: Box<Parsed<Expression>>,
        property: String,
    },
    // IfElseExpression(
    //     Box<Parsed<Expression>>,
    //     Box<Parsed<Expression>>,
    //     Box<Parsed<Expression>>,
    // ),
    // MatchExpression(Box<Parsed<Expression>>, Vec<()>),
    BlockExpression {
        stmts: Vec<Parsed<Statement>>,
        return_value: Option<Box<Parsed<Expression>>>,
    },
    IfLetExpression,
}

impl Expression {
    pub fn requires_trailing_semicolon(&self) -> bool {
        match self {
            // Expression::IfElseExpression(_, _, _) => false,
            Expression::IfLetExpression => false,
            // Expression::MatchExpression(_, _) => false,
            Expression::BlockExpression { .. } => false,
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
