use derive_more::Display;

pub enum Opcode {
    Add,
    Sub,
    Imul,
    Idiv,
    Mov,
    Lea,
    Jmp,
    Test,
    Jcc,
}

// The terms "above" and "below" are associated with the CF flag and refer to
// the relationship between two unsigned integer values. The terms "greater" and
// "less" are associated with the SF and OF flags and refer to the relationship
// between two signed integer values.

#[derive(Debug, Display, Eq, PartialEq)]
pub enum Condition {
    #[display("a")]
    Above,
    #[display("ae")]
    AboveEqual,
    #[display("b")]
    Below,
    #[display("be")]
    BelowEqual,
    #[display("e")]
    Equal,
    #[display("ne")]
    NotEqual,
    #[display("g")]
    Greater,
    #[display("ge")]
    GreaterEqual,
    #[display("l")]
    Less,
    #[display("le")]
    LessEqual,
}

pub enum RegisterClass {
    Gpr64,
    Gpr32,
    Gpr16,
    Gpr8,
}
