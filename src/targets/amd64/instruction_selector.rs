use crate::{
    hir::ty,
    mir::{GenericOpcode, Mir, Opcode, Operand},
    targets::Abi,
};

#[derive(Debug, Clone, Copy)]
enum OperandKind {
    Register,
    Memory,
    Immediate,
}

impl From<&Operand> for OperandKind {
    fn from(value: &Operand) -> Self {
        match value {
            Operand::Register(_, _) => Self::Register,
            Operand::Frame(_) => Self::Memory,
            Operand::Immediate(_) => Self::Immediate,
            Operand::Global(_) | Operand::Function(_) | Operand::Block(_) => unimplemented!(),
        }
    }
}

fn get_add_op(dest: OperandKind, src: OperandKind, size: usize) -> Opcode {
    (match (dest, src, size) {
        (OperandKind::Register, OperandKind::Register, 1) => super::Opcode::Add8rr,
        (OperandKind::Register, OperandKind::Memory, 1) => super::Opcode::Add8rm,
        (OperandKind::Memory, OperandKind::Register, 1) => super::Opcode::Add8mr,
        (OperandKind::Memory, OperandKind::Immediate, 1) => super::Opcode::Add8mi,
        (OperandKind::Register, OperandKind::Immediate, 1) => super::Opcode::Add8ri,

        (OperandKind::Register, OperandKind::Register, 2) => super::Opcode::Add16rr,
        (OperandKind::Register, OperandKind::Memory, 2) => super::Opcode::Add16rm,
        (OperandKind::Memory, OperandKind::Register, 2) => super::Opcode::Add16mr,
        (OperandKind::Memory, OperandKind::Immediate, 2) => super::Opcode::Add16mi,
        (OperandKind::Register, OperandKind::Immediate, 2) => super::Opcode::Add16ri,

        (OperandKind::Register, OperandKind::Register, 4) => super::Opcode::Add32rr,
        (OperandKind::Register, OperandKind::Memory, 4) => super::Opcode::Add32rm,
        (OperandKind::Memory, OperandKind::Register, 4) => super::Opcode::Add32mr,
        (OperandKind::Memory, OperandKind::Immediate, 4) => super::Opcode::Add32mi,
        (OperandKind::Register, OperandKind::Immediate, 4) => super::Opcode::Add32ri,

        (OperandKind::Register, OperandKind::Register, 8) => super::Opcode::Add64rr,
        (OperandKind::Register, OperandKind::Memory, 8) => super::Opcode::Add64rm,
        (OperandKind::Memory, OperandKind::Register, 8) => super::Opcode::Add64mr,
        (OperandKind::Memory, OperandKind::Immediate, 8) => super::Opcode::Add64mi,
        (OperandKind::Register, OperandKind::Immediate, 8) => super::Opcode::Add64ri,

        _ => unreachable!(),
    }) as Opcode
}

pub fn select_instructions(mir: &mut Mir, abi: &dyn Abi, ty_storage: &ty::Storage) {
    for module in &mut mir.0 {
        for func in &mut module.functions {
            for bb in &mut func.blocks {
                for instr in &mut bb.instructions {
                    match unsafe { std::mem::transmute::<_, GenericOpcode>(instr.opcode) } {
                        GenericOpcode::Add => {
                            let vreg_idx = &instr.operands[0].get_vreg_idx().unwrap();
                            let ty = func.vreg_types[vreg_idx];
                            let size = abi.ty_size(ty_storage, ty);

                            instr.opcode = get_add_op(
                                (&instr.operands[0]).into(),
                                (&instr.operands[1]).into(),
                                size,
                            );
                        }
                        GenericOpcode::Sub => unimplemented!(),
                        GenericOpcode::Mul => unimplemented!(),
                        GenericOpcode::SDiv => unimplemented!(),
                        GenericOpcode::UDiv => unimplemented!(),
                        GenericOpcode::FrameIndex => unimplemented!(),
                    }
                }
            }
        }
    }
}
