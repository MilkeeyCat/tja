use crate::macros::usize_wrapper;

usize_wrapper! {Opcode}

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
    GlobalValue,
    Copy,

    Num,
}

impl TryFrom<Opcode> for GenericOpcode {
    type Error = ();

    fn try_from(value: Opcode) -> Result<Self, Self::Error> {
        if *value < Self::Num as usize {
            Ok(unsafe { std::mem::transmute::<_, Self>(value) })
        } else {
            Err(())
        }
    }
}

impl Into<Opcode> for GenericOpcode {
    fn into(self) -> Opcode {
        Opcode(self as usize)
    }
}
