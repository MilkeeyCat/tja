use super::OperandKind;
use crate::{
    mir::{Function, GenericOpcode, Opcode, Operand, OperandIdx, RegisterRole},
    pass::{Context, Pass},
    targets::{
        Abi, Target,
        amd64::{
            address_mode::{AddressMode, Base},
            opcode::{get_load_op, get_store_op},
        },
    },
};

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
    })
    .into()
}

#[derive(Default)]
pub struct InstructionSelection;

impl<'a, T: Target> Pass<'a, Function, T> for InstructionSelection {
    fn run(&self, func: &mut Function, ctx: &mut Context<'a, T>) {
        if func.is_declaration() {
            return;
        }

        for bb in &mut func.blocks {
            for instr in &mut bb.instructions {
                match GenericOpcode::try_from(instr.opcode) {
                    Ok(opcode) => match opcode {
                        GenericOpcode::Add => {
                            let vreg_idx = &instr.operands[0].get_vreg_idx().unwrap();
                            let ty = func.vreg_info.get_vreg(**vreg_idx).ty;
                            let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);

                            instr.opcode = get_add_op(
                                (&instr.operands[0]).into(),
                                (&instr.operands[1]).into(),
                                size,
                            );
                            instr.tied_operands = Some((OperandIdx(0), OperandIdx(1)));
                        }
                        GenericOpcode::Sub => unimplemented!(),
                        GenericOpcode::Mul => unimplemented!(),
                        GenericOpcode::SDiv => unimplemented!(),
                        GenericOpcode::UDiv => unimplemented!(),
                        GenericOpcode::FrameIndex => {
                            let address_mode = AddressMode {
                                base: match &instr.operands[1] {
                                    Operand::Frame(idx) => Base::Frame(*idx),
                                    _ => unreachable!(),
                                },
                                index: None,
                                scale: 1,
                                displacement: None,
                            };

                            instr.opcode = super::Opcode::Lea64.into();
                            instr.operands.remove(1);
                            address_mode.write(&mut instr.operands, 1);
                        }
                        GenericOpcode::PtrAdd => {
                            let base = match &instr.operands[1] {
                                Operand::Register(r, _) => Base::Register(r.clone()),
                                _ => unreachable!(),
                            };
                            let address_mode = match &instr.operands[2] {
                                Operand::Immediate(offset) => AddressMode {
                                    base: base,
                                    index: None,
                                    scale: 1,
                                    displacement: Some(*offset as isize),
                                },
                                Operand::Register(r, RegisterRole::Use) => AddressMode {
                                    base: base,
                                    index: Some(r.clone()),
                                    scale: 1,
                                    displacement: None,
                                },
                                _ => unreachable!(),
                            };

                            instr.opcode = super::Opcode::Lea64.into();
                            instr.operands.remove(2);
                            instr.operands.remove(1);
                            address_mode.write(&mut instr.operands, 1);
                        }
                        GenericOpcode::Load => {
                            let vreg_idx = &instr.operands[0].get_vreg_idx().unwrap();
                            let ty = func.vreg_info.get_vreg(**vreg_idx).ty;
                            let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);
                            let address_mode = AddressMode {
                                base: match &instr.operands[1] {
                                    Operand::Register(r, _) => Base::Register(r.clone()),
                                    _ => unreachable!(),
                                },
                                index: None,
                                scale: 1,
                                displacement: None,
                            };

                            instr.opcode = get_load_op(size).into();
                            instr.operands.remove(1);
                            address_mode.write(&mut instr.operands, 1);
                        }
                        GenericOpcode::Store => {
                            let vreg_idx = &instr.operands[0].get_vreg_idx().unwrap();
                            let ty = func.vreg_info.get_vreg(**vreg_idx).ty;
                            let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);
                            let address_mode = AddressMode {
                                base: match &instr.operands[1] {
                                    Operand::Register(r, _) => Base::Register(r.clone()),
                                    _ => unreachable!(),
                                },
                                index: None,
                                scale: 1,
                                displacement: None,
                            };

                            instr.opcode = get_store_op(size).into();
                            instr.operands.remove(1);
                            address_mode.write(&mut instr.operands, 0);
                        }
                        GenericOpcode::Br => {
                            instr.opcode = super::Opcode::Jmp.into();
                        }
                        GenericOpcode::Return => {
                            instr.opcode = super::Opcode::Ret.into();
                        }
                        GenericOpcode::GlobalValue => match instr.operands[1] {
                            Operand::Function(idx) => {
                                let address_mode = AddressMode {
                                    base: Base::Function(idx),
                                    index: None,
                                    scale: 1,
                                    displacement: None,
                                };

                                instr.opcode = super::Opcode::Lea64.into();
                                instr.operands.remove(1);
                                address_mode.write(&mut instr.operands, 1);
                            }
                            Operand::Global(idx) => {
                                let address_mode = AddressMode {
                                    base: Base::Global(idx),
                                    index: None,
                                    scale: 1,
                                    displacement: None,
                                };

                                instr.opcode = super::Opcode::Lea64.into();
                                instr.operands.remove(1);
                                address_mode.write(&mut instr.operands, 1);
                            }
                            _ => unreachable!(),
                        },
                        GenericOpcode::Copy => (), // skip copy instructions at this step

                        GenericOpcode::Num => unreachable!(),
                    },
                    Err(_) => (),
                }
            }
        }
    }
}
