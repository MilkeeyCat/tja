use super::OperandKind;
use crate::{
    mir::{Function, Operand, Register},
    pass::{Context, Pass},
    targets::{Abi, RegisterInfo, Target},
};

#[derive(Default)]
pub struct MaterializeCopy;

impl<'a, T: Target> Pass<'a, Function, T> for MaterializeCopy {
    fn run(&self, func: &mut Function, ctx: &mut Context<'a, T>) {
        if func.is_declaration() {
            return;
        }

        for bb in &mut func.blocks {
            for instr in &mut bb.instructions {
                if instr.is_copy() {
                    let size = match &instr.operands[0] {
                        Operand::Register(reg, _) => match reg {
                            Register::Virtual(idx) => {
                                let ty = func.vreg_info.get_vreg(*idx).ty;

                                ctx.target.abi().ty_size(ctx.ty_storage, ty)
                            }
                            Register::Physical(reg) => {
                                ctx.target.register_info().get_register_size(reg)
                            }
                        },
                        _ => unreachable!(),
                    };
                    let opcode = match (
                        (&instr.operands[0]).into(),
                        (&instr.operands[1]).into(),
                        size,
                    ) {
                        (OperandKind::Register, OperandKind::Register, 1) => super::Opcode::Mov8rr,
                        (OperandKind::Register, OperandKind::Immediate, 1) => super::Opcode::Mov8ri,

                        (OperandKind::Register, OperandKind::Register, 2) => super::Opcode::Mov16rr,
                        (OperandKind::Register, OperandKind::Immediate, 2) => {
                            super::Opcode::Mov16ri
                        }

                        (OperandKind::Register, OperandKind::Register, 4) => super::Opcode::Mov32rr,
                        (OperandKind::Register, OperandKind::Immediate, 4) => {
                            super::Opcode::Mov32ri
                        }

                        (OperandKind::Register, OperandKind::Register, 8) => super::Opcode::Mov64rr,
                        (OperandKind::Register, OperandKind::Immediate, 8) => {
                            super::Opcode::Mov64ri
                        }

                        _ => unreachable!(),
                    };

                    instr.opcode = opcode.into();
                }
            }
        }
    }
}
