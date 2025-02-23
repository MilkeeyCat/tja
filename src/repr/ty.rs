#[derive(Debug, Clone)]
pub enum Ty {
    I8,
    U8,
}

impl Ty {
    pub fn size(&self) -> usize {
        match self {
            Self::I8 | Self::U8 => 1,
        }
    }

    pub fn signed(&self) -> bool {
        match self {
            Self::I8 => true,
            Self::U8 => false,
        }
    }
}
