use super::OperandKind;
use crate::{
    hir::ty,
    mir::{Function, Opcode, Operand, Register},
    targets::{Abi, RegisterInfo, Target},
};

pub fn materialize_copy<T: Target>(func: &mut Function, target: &T, ty_storage: &ty::Storage) {
    for bb in &mut func.blocks {
        for instr in &mut bb.instructions {
            if instr.is_copy() {
                let size = match &instr.operands[0] {
                    Operand::Register(r, _) => match r {
                        Register::Virtual(idx) => {
                            let ty = func.vreg_types[idx];

                            target.abi().ty_size(ty_storage, ty)
                        }
                        Register::Physical(r) => target.register_info().get_register_size(r),
                    },
                    _ => unreachable!(),
                };
                let opcode = match (
                    (&instr.operands[0]).into(),
                    (&instr.operands[1]).into(),
                    size,
                ) {
                    (OperandKind::Register, OperandKind::Register, 1) => super::Opcode::Mov8rr,
                    (OperandKind::Register, OperandKind::Memory, 1) => todo!("lea8"),
                    (OperandKind::Memory, OperandKind::Register, 1) => super::Opcode::Mov8mr,
                    (OperandKind::Memory, OperandKind::Immediate, 1) => super::Opcode::Mov8mi,
                    (OperandKind::Register, OperandKind::Immediate, 1) => super::Opcode::Mov8ri,

                    (OperandKind::Register, OperandKind::Register, 2) => super::Opcode::Mov16rr,
                    (OperandKind::Register, OperandKind::Memory, 2) => todo!("lea16"),
                    (OperandKind::Memory, OperandKind::Register, 2) => super::Opcode::Mov16mr,
                    (OperandKind::Memory, OperandKind::Immediate, 2) => super::Opcode::Mov16mi,
                    (OperandKind::Register, OperandKind::Immediate, 2) => super::Opcode::Mov16ri,

                    (OperandKind::Register, OperandKind::Register, 4) => super::Opcode::Mov32rr,
                    (OperandKind::Register, OperandKind::Memory, 4) => todo!("lea32"),
                    (OperandKind::Memory, OperandKind::Register, 4) => super::Opcode::Mov32mr,
                    (OperandKind::Memory, OperandKind::Immediate, 4) => super::Opcode::Mov32mi,
                    (OperandKind::Register, OperandKind::Immediate, 4) => super::Opcode::Mov32ri,

                    (OperandKind::Register, OperandKind::Register, 8) => super::Opcode::Mov64rr,
                    (OperandKind::Register, OperandKind::Memory, 8) => todo!("lea64"),
                    (OperandKind::Memory, OperandKind::Register, 8) => super::Opcode::Mov64mr,
                    (OperandKind::Memory, OperandKind::Immediate, 8) => super::Opcode::Mov64mi,
                    (OperandKind::Register, OperandKind::Immediate, 8) => super::Opcode::Mov64ri,

                    _ => unreachable!(),
                };

                instr.opcode = opcode as Opcode;
            }
        }
    }
}
