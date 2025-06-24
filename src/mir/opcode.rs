pub type Opcode = usize;

#[repr(usize)]
pub enum GenericOpcode {
    Add,
    Sub,
    Mul,
    SDiv,
    UDiv,
    FrameIndex,
    PtrAdd,
    Load,
    Store,
    Br,
    Return,
    GlobalValue,
    Copy,

    Num,
}

impl TryFrom<usize> for GenericOpcode {
    type Error = ();

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value < Self::Num as usize {
            Ok(unsafe { std::mem::transmute::<_, Self>(value) })
        } else {
            Err(())
        }
    }
}
