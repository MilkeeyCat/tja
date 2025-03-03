#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Ptr,
    Struct(Vec<Self>),
}

impl Ty {
    pub fn size(&self) -> usize {
        match self {
            Self::I8 | Self::U8 => 1,
            Self::I16 | Self::U16 => 2,
            Self::I32 | Self::U32 => 4,
            Self::I64 | Self::U64 | Self::Ptr => 8,
            // aggregate types size is determined by abi
            Self::Struct(_) => unreachable!(),
        }
    }

    pub fn signed(&self) -> bool {
        match self {
            Self::I8 | Self::I16 | Self::I32 | Self::I64 => true,
            Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::Ptr | Self::Struct(_) => false,
        }
    }
}
