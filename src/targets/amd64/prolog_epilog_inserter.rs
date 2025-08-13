use super::Opcode;
use crate::{
    mir::{
        self, BasicBlock, BasicBlockPatch, BlockIdx, FrameIdx, Function, InstrBuilder, Instruction,
        InstructionIdx, Operand, PhysicalRegister, function::FunctionPatch,
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
        let mut bb = BasicBlock {
            name: "prolog".into(),
            instructions: vec![
                InstrBuilder::new(Opcode::Push64r.into())
                    .add_use(mir::Register::Physical(Register::Rbp.into()))
                    .into(),
                InstrBuilder::new(Opcode::Mov64rr.into())
                    .add_def(mir::Register::Physical(Register::Rbp.into()))
                    .add_use(mir::Register::Physical(Register::Rsp.into()))
                    .into(),
            ],
            successors: HashSet::from([BlockIdx(1)]),
        };

        if stack_frame_size > 0 {
            bb.instructions.push(
                InstrBuilder::new(Opcode::Sub64ri.into())
                    .add_use(mir::Register::Physical(Register::Rsp.into()))
                    .add_operand(Operand::Immediate(stack_frame_size as u64))
                    .into(),
            );
        }

        for (reg, frame_idx) in used_callee_saved_regs.iter().zip(&regs_stack_slots) {
            bb.instructions.push(
                InstrBuilder::new(Opcode::Mov64mr.into())
                    .add_addr_mode(AddressMode {
                        base: Base::Frame(*frame_idx),
                        index: None,
                        scale: 1,
                        displacement: None,
                    })
                    .add_use(mir::Register::Physical(*reg))
                    .into(),
            );
        }

        bb.instructions.push(
            InstrBuilder::new(Opcode::Jmp.into())
                .add_operand(BlockIdx(1).into())
                .into(),
        );

        let mut patch = FunctionPatch::new();

        patch.add_basic_block(BlockIdx(0), bb);
        patch.apply(func);

        for bb in &mut func.blocks {
            let terminator = bb.instructions.last().unwrap();

            if terminator.opcode == Opcode::Ret.into() {
                let mut patch = BasicBlockPatch::new();

                patch.add_instruction(
                    InstructionIdx(bb.instructions.len() - 1),
                    Instruction::new(Opcode::Leave.into()),
                );

                for (reg, frame_idx) in used_callee_saved_regs.iter().zip(&regs_stack_slots) {
                    patch.add_instruction(
                        InstructionIdx(bb.instructions.len() - 1),
                        InstrBuilder::new(Opcode::Mov64rm.into())
                            .add_use(mir::Register::Physical(*reg))
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
