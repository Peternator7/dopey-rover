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
