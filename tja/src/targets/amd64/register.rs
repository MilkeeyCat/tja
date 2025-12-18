use crate::mir::PhysicalRegister;

include!(concat!(env!("OUT_DIR"), "/amd64/register.rs"));

pub(super) struct RegisterDesc {
    pub(super) name: &'static str,
    pub(super) size: usize,
    pub(super) subregs: &'static [Register],
}

impl Register {
    pub const fn into_physical_reg(self) -> PhysicalRegister {
        PhysicalRegister(self as usize)
    }
}

impl From<PhysicalRegister> for Register {
    fn from(value: PhysicalRegister) -> Self {
        assert!(*value < Self::num());

        unsafe { std::mem::transmute::<_, Self>(value) }
    }
}

impl Into<PhysicalRegister> for Register {
    fn into(self) -> PhysicalRegister {
        self.into_physical_reg()
    }
}
