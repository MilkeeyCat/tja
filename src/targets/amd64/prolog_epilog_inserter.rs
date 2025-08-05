use super::Opcode;
use crate::{
    mir::{
        self, BasicBlock, BasicBlockPatch, Function, InstrBuilder, Instruction, Operand,
        PhysicalRegister, StackFrameIdx, function::FunctionPatch,
    },
    pass::{Context, Pass},
    targets::{
        Abi, RegisterInfo, Target,
        amd64::{
            Register,
            address_mode::{AddressMode, Base},
        },
    },
};
use std::collections::HashSet;

#[derive(Default)]
pub struct PrologEpilogInserter;

impl<'a, T: Target> Pass<'a, Function, T> for PrologEpilogInserter {
    fn run(&self, func: &mut Function, ctx: &mut Context<'a, T>) {
        let stack_frame_size = func
            .stack_slots
            .values()
            .sum::<usize>()
            .next_multiple_of(16);
        let regs = func.registers();
        let used_callee_saved_regs: Vec<PhysicalRegister> = ctx
            .target
            .abi()
            .callee_saved_regs()
            .iter()
            .filter(|r| **r != Register::Rbp as PhysicalRegister)
            .filter_map(|r1| {
                regs.iter()
                    .any(|r| match r {
                        mir::Register::Virtual(_) => false,
                        mir::Register::Physical(r2) => ctx.target.register_info().overlaps(r1, r2),
                    })
                    .then_some(*r1)
            })
            .collect();
        let mut regs_stack_slots: Vec<StackFrameIdx> =
            Vec::with_capacity(used_callee_saved_regs.len());

        let mut bb = BasicBlock {
            name: "prolog".into(),
            instructions: vec![
                InstrBuilder::new(Opcode::Push64r as mir::Opcode)
                    .add_use(mir::Register::Physical(Register::Rbp as PhysicalRegister))
                    .into(),
            ],
            successors: HashSet::from([1]),
        };

        if stack_frame_size > 0 {
            bb.instructions.extend([
                InstrBuilder::new(Opcode::Mov64rr as mir::Opcode)
                    .add_def(mir::Register::Physical(
                        Register::Rbp as mir::PhysicalRegister,
                    ))
                    .add_use(mir::Register::Physical(
                        Register::Rsp as mir::PhysicalRegister,
                    ))
                    .into(),
                InstrBuilder::new(Opcode::Sub64ri as mir::Opcode)
                    .add_use(mir::Register::Physical(
                        Register::Rsp as mir::PhysicalRegister,
                    ))
                    .add_operand(Operand::Immediate(stack_frame_size as u64))
                    .into(),
            ]);
        }

        for r in &used_callee_saved_regs {
            let idx = func.next_stack_frame_idx;

            regs_stack_slots.push(idx);
            func.next_stack_frame_idx += 1;
            func.stack_slots.insert(idx, 8);

            bb.instructions.push(
                InstrBuilder::new(Opcode::Mov64mr as mir::Opcode)
                    .add_addr_mode(AddressMode {
                        base: Base::Frame(idx),
                        index: None,
                        scale: 1,
                        displacement: None,
                    })
                    .add_use(mir::Register::Physical(*r))
                    .into(),
            );
        }

        bb.instructions.push(
            InstrBuilder::new(Opcode::Jmp as mir::Opcode)
                .add_operand(Operand::Block(1))
                .into(),
        );

        let mut patch = FunctionPatch::new();

        patch.add_basic_block(0, bb);
        patch.apply(func);

        for bb in &mut func.blocks {
            let terminator = bb.instructions.last().unwrap();

            if terminator.opcode == Opcode::Ret as mir::Opcode {
                let mut patch = BasicBlockPatch::new();

                patch.add_instruction(
                    bb.instructions.len() - 1,
                    Instruction::new(Opcode::Leave as mir::Opcode),
                );

                for (r, frame_idx) in used_callee_saved_regs.iter().zip(&regs_stack_slots) {
                    patch.add_instruction(
                        bb.instructions.len() - 1,
                        InstrBuilder::new(Opcode::Mov64rm as mir::Opcode)
                            .add_use(mir::Register::Physical(*r))
                            .add_addr_mode(AddressMode {
                                base: Base::Frame(*frame_idx),
                                index: None,
                                scale: 1,
                                displacement: None,
                            })
                            .into(),
                    );
                }

                patch.apply(bb);
            }
        }
    }
}
