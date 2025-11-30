use super::Opcode;
use crate::{
    mir::{self, FrameIdx, Function, InstructionIdx, Operand, PhysicalRegister},
    pass::{Context, Pass},
    targets::{
        Abi, RegisterInfo, Target,
        amd64::{
            Register,
            address_mode::{AddressMode, Base},
        },
    },
};

#[derive(Default)]
pub struct PrologEpilogInserter;

impl<'a, T: Target> Pass<'a, Function, T> for PrologEpilogInserter {
    fn run(&self, func: &mut Function, ctx: &mut Context<'a, T>) {
        if func.is_declaration() {
            return;
        }

        let regs = func.registers();
        let used_callee_saved_regs: Vec<PhysicalRegister> = ctx
            .target
            .abi()
            .callee_saved_regs()
            .iter()
            .filter(|reg| **reg != Register::Rbp.into())
            .filter_map(|reg1| {
                regs.iter()
                    .any(|reg| match reg {
                        mir::Register::Virtual(_) => false,
                        mir::Register::Physical(reg2) => {
                            ctx.target.register_info().overlaps(reg1, reg2)
                        }
                    })
                    .then_some(*reg1)
            })
            .collect();
        let regs_stack_slots: Vec<FrameIdx> = used_callee_saved_regs
            .iter()
            .map(|_| func.frame_info.create_stack_object(8))
            .collect();
        let stack_frame_size = func
            .frame_info
            .objects_iter()
            .map(|object| object.size)
            .sum::<usize>()
            .next_multiple_of(16);
        let prolog_bb_idx = func.create_block("prolog".into());
        let old_head_bb_idx = func.block_head.unwrap();

        func.blocks[prolog_bb_idx]
            .successors
            .insert(old_head_bb_idx);
        func.block_cursor_mut().insert_after(prolog_bb_idx);

        {
            let push_instr_idx = func
                .create_instr()
                .with_opcode(Opcode::Push64r.into())
                .add_use(mir::Register::Physical(Register::Rbp.into()))
                .idx();
            let mov_instr_idx = func
                .create_instr()
                .with_opcode(Opcode::Mov64rr.into())
                .add_def(mir::Register::Physical(Register::Rbp.into()))
                .add_use(mir::Register::Physical(Register::Rsp.into()))
                .idx();
            let mut cursor = func.instr_cursor_mut(prolog_bb_idx);

            cursor.insert_after(push_instr_idx);
            cursor.insert_after(mov_instr_idx);
        }

        if stack_frame_size > 0 {
            let instr_idx = func
                .create_instr()
                .with_opcode(Opcode::Sub64ri32.into())
                .add_use(mir::Register::Physical(Register::Rsp.into()))
                .add_operand(Operand::Immediate(stack_frame_size as u64))
                .idx();

            func.instr_cursor_mut(prolog_bb_idx)
                .at_tail()
                .insert_after(instr_idx);
        }

        for (reg, frame_idx) in used_callee_saved_regs.iter().zip(&regs_stack_slots) {
            let instr_idx = func
                .create_instr()
                .with_opcode(Opcode::Mov64mr.into())
                .add_addr_mode(AddressMode {
                    base: Base::Frame(*frame_idx),
                    index: None,
                    scale: 1,
                    displacement: None,
                })
                .add_use(mir::Register::Physical(*reg))
                .idx();

            func.instr_cursor_mut(prolog_bb_idx)
                .at_tail()
                .insert_after(instr_idx);
        }

        let instr_idx = func
            .create_instr()
            .with_opcode(Opcode::Jmp.into())
            .add_operand(old_head_bb_idx.into())
            .idx();

        func.instr_cursor_mut(prolog_bb_idx)
            .at_tail()
            .insert_after(instr_idx);

        let mut bb_cursor = func.block_cursor_mut();

        while let Some(bb_idx) = bb_cursor.move_next() {
            if let Some(tail) = bb_cursor.func.blocks[bb_idx].instruction_tail
                && bb_cursor.func.instructions[tail].opcode == Opcode::Ret.into()
            {
                let leave_instr_idx = bb_cursor
                    .func
                    .create_instr()
                    .with_opcode(Opcode::Leave.into())
                    .idx();
                let instr_indices: Vec<InstructionIdx> = used_callee_saved_regs
                    .iter()
                    .zip(&regs_stack_slots)
                    .map(|(reg, frame_idx)| {
                        bb_cursor
                            .func
                            .create_instr()
                            .with_opcode(Opcode::Mov64rm.into())
                            .add_use(mir::Register::Physical(*reg))
                            .add_addr_mode(AddressMode {
                                base: Base::Frame(*frame_idx),
                                index: None,
                                scale: 1,
                                displacement: None,
                            })
                            .idx()
                    })
                    .collect();
                let mut cursor = bb_cursor
                    .func
                    .instr_cursor_mut(bb_cursor.idx().unwrap())
                    .at_tail();

                cursor.move_prev();

                for instr_idx in instr_indices {
                    cursor.insert_after(instr_idx);
                }

                cursor.insert_after(leave_instr_idx);
            }
        }
    }
}
