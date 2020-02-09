#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Opcode {
    // BinaryOps
    Add,
    Sub,
    Mult,
    Div,

    Cons,
    Test,

    And,
    Or,

    GreaterThan,
    GreaterThanOrEqualTo,
    LessThan,
    LessThanOrEqualTo,
    EqualTo,
    NotEqualTo,

    // MemoryReads
    PopAndStoreLocal,
    Pop,
    PushConstant,
    PushLocal,
    PushGlobal,

    // Control Flow
    Call,
    Return,
    JumpTrue,
    JumpFalse,

    // Object Expressions
    PropertyLookup,
}
