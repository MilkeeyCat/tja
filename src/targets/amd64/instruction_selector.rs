use crate::{
    mir::{Function, GenericOpcode, Operand, RegisterRole},
    pass::{Context, Pass},
    targets::{
        Abi, Target,
        amd64::{
            ConditionCode,
            address_mode::{AddressMode, Base},
            opcode::{get_load_op, get_store_op},
        },
    },
};

#[derive(Default)]
pub struct InstructionSelection;

mod generated {
    use super::InstructionSelection;
    use crate::{
        mir::{
            GenericOpcode, InstructionBuilder, InstructionCursorMut,
            pattern_match::{operands::*, predicates::*, *},
        },
        pass::Context,
        targets::{
            Target,
            amd64::{Opcode, RegisterClass},
        },
    };

    include!(concat!(env!("OUT_DIR"), "/amd64/instruction_selector.rs"));
}

impl<'a, T: Target> Pass<'a, Function, T> for InstructionSelection {
    fn run(&self, func: &mut Function, ctx: &mut Context<'a, T>) {
        if func.is_declaration() {
            return;
        }

        let mut bb_cursor = func.block_cursor_mut();

        while let Some(bb_idx) = bb_cursor.move_next() {
            let mut instr_cursor = bb_cursor.func.instr_cursor_mut(bb_idx);

            while let Some(instr_idx) = instr_cursor.move_next() {
                let instr = instr_cursor.func.instructions.get_mut(instr_idx).unwrap();

                match GenericOpcode::try_from(instr.opcode) {
                    Ok(opcode) => match opcode {
                        GenericOpcode::Mul => unimplemented!(),
                        GenericOpcode::SDiv => unimplemented!(),
                        GenericOpcode::UDiv => unimplemented!(),
                        GenericOpcode::FrameIndex => {
                            let address_mode = AddressMode {
                                base: Base::Frame(*instr.operands[1].expect_frame_idx()),
                                index: None,
                                scale: 1,
                                displacement: None,
                            };

                            instr.opcode = super::Opcode::Lea64.into();
                            instr.operands.remove(1.into());
                            address_mode.write(&mut instr.operands, 1.into());
                        }
                        GenericOpcode::PtrAdd => {
                            let base =
                                Base::Register(instr.operands[1].expect_register().0.clone());
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
                            let vreg_idx = instr.operands[0].expect_register().0.expect_virtual();
                            let ty = instr_cursor.func.vreg_info.get_vreg(*vreg_idx).ty;
                            let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);
                            let address_mode = AddressMode {
                                base: Base::Register(instr.operands[1].expect_register().0.clone()),
                                index: None,
                                scale: 1,
                                displacement: None,
                            };

                            instr.opcode = get_load_op(size).into();
                            instr.operands.remove(1.into());
                            address_mode.write(&mut instr.operands, 1.into());
                        }
                        GenericOpcode::Store => {
                            let vreg_idx = instr.operands[0].expect_register().0.expect_virtual();
                            let ty = instr_cursor.func.vreg_info.get_vreg(*vreg_idx).ty;
                            let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);
                            let address_mode = AddressMode {
                                base: Base::Register(instr.operands[1].expect_register().0.clone()),
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
                        GenericOpcode::BrCond => {
                            instr.opcode = super::Opcode::Jcc.into();
                            instr
                                .operands
                                .push(Operand::Immediate(ConditionCode::Equal as u64));

                            let cond = instr.operands.remove(0.into()).expect_register().0.clone();
                            let idx = instr_cursor
                                .func
                                .create_instr()
                                .with_opcode(super::Opcode::Cmp8ri.into())
                                .add_use(cond)
                                .add_operand(Operand::Immediate(0))
                                .idx();

                            instr_cursor.insert_before(idx);
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
                        GenericOpcode::ICmp => {
                            let ty = instr_cursor
                                .func
                                .vreg_info
                                .get_vreg(*instr.operands[2].expect_register().0.expect_virtual())
                                .ty;
                            let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);
                            let cond_code = crate::ConditionCode::from(
                                *instr.operands.remove(1.into()).expect_immediate(),
                            );
                            let result =
                                instr.operands.remove(0.into()).expect_register().0.clone();

                            instr.opcode = match size {
                                1 => super::Opcode::Cmp8rr,
                                2 => super::Opcode::Cmp16rr,
                                4 => super::Opcode::Cmp32rr,
                                8 => super::Opcode::Cmp64rr,

                                _ => unreachable!(),
                            }
                            .into();

                            let idx = instr_cursor
                                .func
                                .create_instr()
                                .with_opcode(super::Opcode::Setccr.into())
                                .add_def(result)
                                .add_operand(Operand::Immediate(
                                    ConditionCode::from(cond_code) as u64
                                ))
                                .idx();

                            instr_cursor.insert_after(idx);
                            instr_cursor.move_prev();
                        }
                        GenericOpcode::Copy => (), // skip copy instructions at this step
                        _ => {
                            assert!(
                                self.select_instr(ctx, &mut instr_cursor),
                                "failed to select instruction for opcode {:?}",
                                instr_cursor.current().unwrap().opcode
                            );
                        }
                    },
                    Err(_) => (),
                }
            }
        }
    }
}
