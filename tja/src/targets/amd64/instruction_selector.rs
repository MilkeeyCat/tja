use crate::{
    mir::{self, Function, GenericOpcode, Operand, RegisterRole},
    pass::{Context, Pass},
    targets::{
        Abi, Target,
        amd64::{
            ConditionCode, Register,
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
                        GenericOpcode::Mul => {
                            let ty = instr_cursor
                                .func
                                .vreg_info
                                .get_vreg(*instr.operands[0].expect_register().0.expect_virtual())
                                .ty;
                            let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);
                            let lhs = instr.operands.remove(1.into()).expect_register().0.clone();
                            let rhs = instr.operands.remove(1.into()).expect_register().0.clone();
                            let idx = match size {
                                1 => 0,
                                2 => 1,
                                4 => 2,
                                8 => 3,
                                _ => unreachable!(),
                            };

                            struct InstructionInfo {
                                /// Multiplicand and result register.
                                reg: Register,
                                mul_op: super::Opcode,
                            }

                            const TABLE: [InstructionInfo; 4] = [
                                InstructionInfo {
                                    reg: Register::Al,
                                    mul_op: super::Opcode::IMul8rr,
                                },
                                InstructionInfo {
                                    reg: Register::Ax,
                                    mul_op: super::Opcode::IMul16rr,
                                },
                                InstructionInfo {
                                    reg: Register::Eax,
                                    mul_op: super::Opcode::IMul32rr,
                                },
                                InstructionInfo {
                                    reg: Register::Rax,
                                    mul_op: super::Opcode::IMul64rr,
                                },
                            ];

                            let entry = &TABLE[idx];

                            instr.opcode = GenericOpcode::Copy.into();
                            instr.operands.push(Operand::Register(
                                mir::Register::Physical(entry.reg.into_physical_reg()),
                                RegisterRole::Use,
                            ));

                            let copy_lhs_instr_idx = instr_cursor.func.create_instr().copy(
                                mir::Register::Physical(entry.reg.into_physical_reg()),
                                Operand::Register(lhs.clone(), RegisterRole::Use),
                            );
                            let mul_instr_idx = instr_cursor
                                .func
                                .create_instr()
                                .with_opcode(entry.mul_op.into())
                                .add_use(rhs)
                                .idx();

                            instr_cursor.insert_before(mul_instr_idx);
                            instr_cursor.insert_before(copy_lhs_instr_idx);
                        }
                        GenericOpcode::SDiv | GenericOpcode::UDiv => {
                            let ty = instr_cursor
                                .func
                                .vreg_info
                                .get_vreg(*instr.operands[0].expect_register().0.expect_virtual())
                                .ty;
                            let size = ctx.target.abi().ty_size(ctx.ty_storage, ty);
                            let lhs = instr.operands.remove(1.into()).expect_register().0.clone();
                            let rhs = instr.operands.remove(1.into()).expect_register().0.clone();

                            struct OperandsInfo {
                                low_reg: Register,
                                high_reg: Register,
                                sign_extend_op: Option<super::Opcode>,
                                zero_extend_op: super::Opcode,
                                div_op: super::Opcode,
                            }

                            const TABLE: [OperandsInfo; 4] = [
                                OperandsInfo {
                                    low_reg: Register::Al,
                                    high_reg: Register::Ah,
                                    sign_extend_op: None,
                                    zero_extend_op: super::Opcode::Xor8rr,
                                    div_op: super::Opcode::IDiv8rr,
                                },
                                OperandsInfo {
                                    low_reg: Register::Ax,
                                    high_reg: Register::Dx,
                                    sign_extend_op: Some(super::Opcode::Cwd),
                                    zero_extend_op: super::Opcode::Xor16rr,
                                    div_op: super::Opcode::IDiv16rr,
                                },
                                OperandsInfo {
                                    low_reg: Register::Eax,
                                    high_reg: Register::Edx,
                                    sign_extend_op: Some(super::Opcode::Cdq),
                                    zero_extend_op: super::Opcode::Xor32rr,
                                    div_op: super::Opcode::IDiv32rr,
                                },
                                OperandsInfo {
                                    low_reg: Register::Rax,
                                    high_reg: Register::Rdx,
                                    sign_extend_op: Some(super::Opcode::Cqo),
                                    zero_extend_op: super::Opcode::Xor64rr,
                                    div_op: super::Opcode::IDiv64rr,
                                },
                            ];

                            let entry_idx = match size {
                                1 => 0,
                                2 => 1,
                                4 => 2,
                                8 => 3,
                                _ => unreachable!(),
                            };
                            let entry = &TABLE[entry_idx];

                            instr.opcode = GenericOpcode::Copy.into();
                            instr.operands.push(Operand::Register(
                                mir::Register::Physical(entry.low_reg.into_physical_reg()),
                                RegisterRole::Use,
                            ));

                            let copy_lhs_instr_idx = instr_cursor.func.create_instr().copy(
                                mir::Register::Physical(entry.low_reg.into_physical_reg()),
                                Operand::Register(lhs.clone(), RegisterRole::Use),
                            );
                            let extend_instr_idx = match opcode {
                                GenericOpcode::SDiv => match entry.sign_extend_op {
                                    Some(opcode) => instr_cursor
                                        .func
                                        .create_instr()
                                        .with_opcode(opcode.into())
                                        .idx(),
                                    None => {
                                        // this will only be reached for IDiv8rr
                                        instr_cursor
                                            .func
                                            .create_instr()
                                            .with_opcode(super::Opcode::Movsx16rr8.into())
                                            .add_def(mir::Register::Physical(
                                                Register::Ax.into_physical_reg(),
                                            ))
                                            .add_use(mir::Register::Physical(
                                                Register::Al.into_physical_reg(),
                                            ))
                                            .idx()
                                    }
                                },
                                GenericOpcode::UDiv => instr_cursor
                                    .func
                                    .create_instr()
                                    .with_opcode(entry.zero_extend_op.into())
                                    .add_def(mir::Register::Physical(
                                        entry.high_reg.into_physical_reg(),
                                    ))
                                    .add_use(mir::Register::Physical(
                                        entry.high_reg.into_physical_reg(),
                                    ))
                                    .add_use(mir::Register::Physical(
                                        entry.high_reg.into_physical_reg(),
                                    ))
                                    .set_tied_operands(0.into(), 1.into())
                                    .idx(),
                                _ => unreachable!(),
                            };
                            let div_instr_idx = instr_cursor
                                .func
                                .create_instr()
                                .with_opcode(entry.div_op.into())
                                .add_use(rhs)
                                .idx();

                            instr_cursor.insert_before(div_instr_idx);
                            instr_cursor.insert_before(extend_instr_idx);
                            instr_cursor.insert_before(copy_lhs_instr_idx);
                        }
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
