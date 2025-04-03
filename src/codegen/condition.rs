use crate::repr::op::CmpOp;
use derive_more::Display;

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

impl From<&CmpOp> for Condition {
    fn from(value: &CmpOp) -> Self {
        match value {
            CmpOp::Equal => Self::Equal,
            CmpOp::NotEqual => Self::NotEqual,
            CmpOp::UnsignedLessEqual => Self::BelowEqual,
            CmpOp::UnsignedLessThan => Self::Below,
            CmpOp::UnsignedGreaterEqual => Self::AboveEqual,
            CmpOp::UnsignedGreaterThan => Self::Above,
            CmpOp::SignedLessEqual => Self::LessEqual,
            CmpOp::SignedLessThan => Self::Less,
            CmpOp::SignedGreaterEqual => Self::GreaterEqual,
            CmpOp::SignedGreaterThan => Self::Greater,
        }
    }
}
