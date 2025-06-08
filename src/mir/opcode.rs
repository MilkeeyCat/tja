pub type Opcode = usize;

#[repr(usize)]
pub enum GenericOpcode {
    Add,
    Sub,
    Mul,
    SDiv,
    UDiv,
    FrameIndex,
}
