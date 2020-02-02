#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
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
