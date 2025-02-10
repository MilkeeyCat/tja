pub mod allocator;
mod register;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum OperandSize {
    Byte,
    Word,
    Dword,
    Qword,
}

impl std::fmt::Display for OperandSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Byte => "byte ptr",
                Self::Word => "word ptr",
                Self::Dword => "dword ptr",
                Self::Qword => "qword ptr",
            }
        )
    }
}
