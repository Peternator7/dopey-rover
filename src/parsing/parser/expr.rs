#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum AlgebraicOperator {
    Add,
    Sub,
    Mult,
    Divide,
    Modulus,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum BooleanOperator {
    And,
    Or,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum ComparisonOperator {
    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,
    EqualTo,
    NotEqualTo,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Bool(bool),
    Number(f32),
    StringLiteral(String),
    NewObject(usize),
    AlgebraicExpression(AlgebraicOperator, Box<Expression>, Box<Expression>),
    ComparisonExpression(ComparisonOperator, Box<Expression>, Box<Expression>),
    BooleanExpression(BooleanOperator, Box<Expression>, Box<Expression>),
    Debug(Vec<Expression>),
    TestExpression(Box<Expression>, Box<Expression>),
}
