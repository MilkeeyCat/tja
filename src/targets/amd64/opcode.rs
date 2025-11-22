use crate::{
    mir::{self, GenericOpcode},
    targets::amd64::OperandKind,
};

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

pub fn get_add_op(dest: OperandKind, src: OperandKind, size: usize) -> mir::Opcode {
    (match (dest, src, size) {
        (OperandKind::Register, OperandKind::Register, 1) => Opcode::Add8rr,
        (OperandKind::Register, OperandKind::Memory, 1) => Opcode::Add8rm,
        (OperandKind::Memory, OperandKind::Register, 1) => Opcode::Add8mr,
        (OperandKind::Memory, OperandKind::Immediate, 1) => Opcode::Add8mi,
        (OperandKind::Register, OperandKind::Immediate, 1) => Opcode::Add8ri,

        (OperandKind::Register, OperandKind::Register, 2) => Opcode::Add16rr,
        (OperandKind::Register, OperandKind::Memory, 2) => Opcode::Add16rm,
        (OperandKind::Memory, OperandKind::Register, 2) => Opcode::Add16mr,
        (OperandKind::Memory, OperandKind::Immediate, 2) => Opcode::Add16mi,
        (OperandKind::Register, OperandKind::Immediate, 2) => Opcode::Add16ri,

        (OperandKind::Register, OperandKind::Register, 4) => Opcode::Add32rr,
        (OperandKind::Register, OperandKind::Memory, 4) => Opcode::Add32rm,
        (OperandKind::Memory, OperandKind::Register, 4) => Opcode::Add32mr,
        (OperandKind::Memory, OperandKind::Immediate, 4) => Opcode::Add32mi,
        (OperandKind::Register, OperandKind::Immediate, 4) => Opcode::Add32ri,

        (OperandKind::Register, OperandKind::Register, 8) => Opcode::Add64rr,
        (OperandKind::Register, OperandKind::Memory, 8) => Opcode::Add64rm,
        (OperandKind::Memory, OperandKind::Register, 8) => Opcode::Add64mr,
        (OperandKind::Memory, OperandKind::Immediate, 8) => Opcode::Add64mi,
        (OperandKind::Register, OperandKind::Immediate, 8) => Opcode::Add64ri,

        _ => unreachable!(),
    })
    .into()
}
