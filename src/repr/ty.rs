#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    I8,
    I16,
    I32,
    I64,
    Ptr,
    Struct(Vec<Self>),
}

impl Ty {
    pub fn size(&self) -> usize {
        match self {
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 | Self::Ptr => 8,
            // aggregate types size is determined by abi
            Self::Struct(_) => unreachable!(),
        }
    }
}
