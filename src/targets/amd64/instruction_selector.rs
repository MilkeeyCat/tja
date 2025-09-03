use crate::{
    mir::{Function, GenericOpcode, Operand, RegisterRole},
    pass::{Context, Pass},
    targets::{
        Abi, Target,
        amd64::{
            address_mode::{AddressMode, Base},
            opcode::{get_add_op, get_load_op, get_store_op},
        },
    },
};

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
                            instr.tied_operands = Some((0.into(), 1.into()));
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
                            instr.operands.remove(1.into());
                            address_mode.write(&mut instr.operands, 1.into());
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
                            instr.operands.remove(2.into());
                            instr.operands.remove(1.into());
                            address_mode.write(&mut instr.operands, 1.into());
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
                            instr.operands.remove(1.into());
                            address_mode.write(&mut instr.operands, 1.into());
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
                            instr.operands.remove(1.into());
                            address_mode.write(&mut instr.operands, 0.into());
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
                                instr.operands.remove(1.into());
                                address_mode.write(&mut instr.operands, 1.into());
                            }
                            Operand::Global(idx) => {
                                let address_mode = AddressMode {
                                    base: Base::Global(idx),
                                    index: None,
                                    scale: 1,
                                    displacement: None,
                                };

                                instr.opcode = super::Opcode::Lea64.into();
                                instr.operands.remove(1.into());
                                address_mode.write(&mut instr.operands, 1.into());
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
