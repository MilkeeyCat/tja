use crate::mir::{self, GenericOpcode};

include!(concat!(env!("OUT_DIR"), "/amd64/opcode.rs"));

impl From<mir::Opcode> for Opcode {
    fn from(value: mir::Opcode) -> Self {
        assert!(*value >= GenericOpcode::num() && *value <= GenericOpcode::num() + Self::num());

        unsafe { std::mem::transmute::<_, Self>(value) }
    }
}

impl Into<mir::Opcode> for Opcode {
    fn into(self) -> mir::Opcode {
        mir::Opcode(self as usize)
    }
}

pub fn get_load_op(size: usize) -> Opcode {
    match size {
        1 => Opcode::Mov8rm,
        2 => Opcode::Mov16rm,
        4 => Opcode::Mov32rm,
        8 => Opcode::Mov64rm,
        _ => unreachable!(),
    }
}

pub fn get_store_op(size: usize) -> Opcode {
    match size {
        1 => Opcode::Mov8mr,
        2 => Opcode::Mov16mr,
        4 => Opcode::Mov32mr,
        8 => Opcode::Mov64mr,
        _ => unreachable!(),
    }
}
