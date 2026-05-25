use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq)]
pub(crate) struct Ty(u8);

impl Ty {
    pub(crate) const I8: Ty = Ty(0);
    pub(crate) const I16: Ty = Ty(1);
    pub(crate) const I32: Ty = Ty(2);
    pub(crate) const I64: Ty = Ty(3);
    pub(crate) const PTR: Ty = Ty(4);
}

impl Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::PTR => write!(f, "ptr"),
            _ => unreachable!(),
        }
    }
}
