#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    SDiv,
    UDiv,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CmpOp {
    Equal,
    NotEqual,

    UnsignedLessEqual,
    UnsignedLessThan,
    UnsignedGreaterEqual,
    UnsignedGreaterThan,

    SignedLessEqual,
    SignedLessThan,
    SignedGreaterEqual,
    SignedGreaterThan,
}
