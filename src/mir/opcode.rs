pub type Opcode = usize;

#[repr(usize)]
pub enum GenericOpcode {
    Add,
    Sub,
    Mul,
    SDiv,
    UDiv,
    FrameIndex,
    Copy,

    Num,
}

impl From<usize> for GenericOpcode {
    fn from(value: usize) -> Self {
        assert!(value < Self::Num as usize);

        unsafe { std::mem::transmute::<_, Self>(value) }
    }
}
