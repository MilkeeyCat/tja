use super::OperandKind;
use crate::{mir, targets::amd64};

pub fn mov(
    bb: &mut mir::BasicBlock,
    operands: Vec<mir::Operand>,
    dest: OperandKind,
    src: OperandKind,
    size: usize,
) {
    let opcode = match (dest, src, size) {
        (OperandKind::Register, OperandKind::Register, 1) => amd64::Opcode::Mov8rr,
        (OperandKind::Register, OperandKind::Memory, 1) => amd64::Opcode::Mov8rm,
        (OperandKind::Memory, OperandKind::Register, 1) => amd64::Opcode::Mov8mr,
        (OperandKind::Memory, OperandKind::Immediate, 1) => amd64::Opcode::Mov8mi,
        (OperandKind::Register, OperandKind::Immediate, 1) => amd64::Opcode::Mov8ri,

        (OperandKind::Register, OperandKind::Register, 2) => amd64::Opcode::Mov16rr,
        (OperandKind::Register, OperandKind::Memory, 2) => amd64::Opcode::Mov16rm,
        (OperandKind::Memory, OperandKind::Register, 2) => amd64::Opcode::Mov16mr,
        (OperandKind::Memory, OperandKind::Immediate, 2) => amd64::Opcode::Mov16mi,
        (OperandKind::Register, OperandKind::Immediate, 2) => amd64::Opcode::Mov16ri,

        (OperandKind::Register, OperandKind::Register, 4) => amd64::Opcode::Mov32rr,
        (OperandKind::Register, OperandKind::Memory, 4) => amd64::Opcode::Mov32rm,
        (OperandKind::Memory, OperandKind::Register, 4) => amd64::Opcode::Mov32mr,
        (OperandKind::Memory, OperandKind::Immediate, 4) => amd64::Opcode::Mov32mi,
        (OperandKind::Register, OperandKind::Immediate, 4) => amd64::Opcode::Mov32ri,

        (OperandKind::Register, OperandKind::Register, 8) => amd64::Opcode::Mov64rr,
        (OperandKind::Register, OperandKind::Memory, 8) => amd64::Opcode::Mov64rm,
        (OperandKind::Memory, OperandKind::Register, 8) => amd64::Opcode::Mov64mr,
        (OperandKind::Memory, OperandKind::Immediate, 8) => amd64::Opcode::Mov64mi,
        (OperandKind::Register, OperandKind::Immediate, 8) => amd64::Opcode::Mov64ri,

        _ => unreachable!(),
    };

    bb.instructions.push(mir::Instruction {
        opcode: opcode as mir::Opcode,
        operands,
    });
}

pub fn test(
    bb: &mut mir::BasicBlock,
    operands: Vec<mir::Operand>,
    dest: OperandKind,
    src: OperandKind,
    size: usize,
) {
    let opcode = match (dest, src, size) {
        (OperandKind::Register, OperandKind::Immediate, 1) => amd64::Opcode::Test8ri,
        (OperandKind::Memory, OperandKind::Immediate, 1) => amd64::Opcode::Test8mi,
        (OperandKind::Register, OperandKind::Register, 1) => amd64::Opcode::Test8rr,
        (OperandKind::Memory, OperandKind::Register, 1) => amd64::Opcode::Test8mr,

        (OperandKind::Register, OperandKind::Immediate, 2) => amd64::Opcode::Test16ri,
        (OperandKind::Memory, OperandKind::Immediate, 2) => amd64::Opcode::Test16mi,
        (OperandKind::Register, OperandKind::Register, 2) => amd64::Opcode::Test16rr,
        (OperandKind::Memory, OperandKind::Register, 2) => amd64::Opcode::Test16mr,

        (OperandKind::Register, OperandKind::Immediate, 4) => amd64::Opcode::Test32ri,
        (OperandKind::Memory, OperandKind::Immediate, 4) => amd64::Opcode::Test32mi,
        (OperandKind::Register, OperandKind::Register, 4) => amd64::Opcode::Test32rr,
        (OperandKind::Memory, OperandKind::Register, 4) => amd64::Opcode::Test32mr,

        (OperandKind::Register, OperandKind::Immediate, 8) => amd64::Opcode::Test64ri,
        (OperandKind::Memory, OperandKind::Immediate, 8) => amd64::Opcode::Test64mi,
        (OperandKind::Register, OperandKind::Register, 8) => amd64::Opcode::Test64rr,
        (OperandKind::Memory, OperandKind::Register, 8) => amd64::Opcode::Test64mr,

        _ => unreachable!(),
    };

    bb.instructions.push(mir::Instruction {
        opcode: opcode as mir::Opcode,
        operands,
    });
}

pub fn add(
    bb: &mut mir::BasicBlock,
    operands: Vec<mir::Operand>,
    dest: OperandKind,
    src: OperandKind,
    size: usize,
) {
    let opcode = match (dest, src, size) {
        (OperandKind::Register, OperandKind::Register, 1) => amd64::Opcode::Add8rr,
        (OperandKind::Register, OperandKind::Memory, 1) => amd64::Opcode::Add8rm,
        (OperandKind::Memory, OperandKind::Register, 1) => amd64::Opcode::Add8mr,
        (OperandKind::Memory, OperandKind::Immediate, 1) => amd64::Opcode::Add8mi,
        (OperandKind::Register, OperandKind::Immediate, 1) => amd64::Opcode::Add8ri,

        (OperandKind::Register, OperandKind::Register, 2) => amd64::Opcode::Add16rr,
        (OperandKind::Register, OperandKind::Memory, 2) => amd64::Opcode::Add16rm,
        (OperandKind::Memory, OperandKind::Register, 2) => amd64::Opcode::Add16mr,
        (OperandKind::Memory, OperandKind::Immediate, 2) => amd64::Opcode::Add16mi,
        (OperandKind::Register, OperandKind::Immediate, 2) => amd64::Opcode::Add16ri,

        (OperandKind::Register, OperandKind::Register, 4) => amd64::Opcode::Add32rr,
        (OperandKind::Register, OperandKind::Memory, 4) => amd64::Opcode::Add32rm,
        (OperandKind::Memory, OperandKind::Register, 4) => amd64::Opcode::Add32mr,
        (OperandKind::Memory, OperandKind::Immediate, 4) => amd64::Opcode::Add32mi,
        (OperandKind::Register, OperandKind::Immediate, 4) => amd64::Opcode::Add32ri,

        (OperandKind::Register, OperandKind::Register, 8) => amd64::Opcode::Add64rr,
        (OperandKind::Register, OperandKind::Memory, 8) => amd64::Opcode::Add64rm,
        (OperandKind::Memory, OperandKind::Register, 8) => amd64::Opcode::Add64mr,
        (OperandKind::Memory, OperandKind::Immediate, 8) => amd64::Opcode::Add64mi,
        (OperandKind::Register, OperandKind::Immediate, 8) => amd64::Opcode::Add64ri,

        _ => unreachable!(),
    };

    bb.instructions.push(mir::Instruction {
        opcode: opcode as mir::Opcode,
        operands,
    });
}
