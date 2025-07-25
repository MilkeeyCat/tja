use super::Opcode;
use crate::{
    mir::{
        self, BasicBlock, BasicBlockPatch, Function, Instruction, Operand, RegisterRole,
        function::FunctionPatch,
    },
    pass::{Context, Pass},
    targets::{Target, amd64::Register},
};
use std::collections::HashSet;

#[derive(Default)]
pub struct PrologEpilogInserter;

impl<'a, T: Target> Pass<'a, Function, T> for PrologEpilogInserter {
    fn run(&self, func: &mut Function, _ctx: &mut Context<'a, T>) {
        let stack_frame_size = func
            .stack_slots
            .values()
            .sum::<usize>()
            .next_multiple_of(16);

        let mut bb = BasicBlock {
            name: "prologue".into(),
            instructions: vec![
                Instruction::new(
                    Opcode::Push64r as mir::Opcode,
                    vec![Operand::Register(
                        mir::Register::Physical(Register::Rbp as mir::PhysicalRegister),
                        RegisterRole::Use,
                    )],
                ),
                Instruction::new(
                    Opcode::Mov64rr as mir::Opcode,
                    vec![
                        Operand::Register(
                            mir::Register::Physical(Register::Rbp as mir::PhysicalRegister),
                            RegisterRole::Use,
                        ),
                        Operand::Register(
                            mir::Register::Physical(Register::Rsp as mir::PhysicalRegister),
                            RegisterRole::Use,
                        ),
                    ],
                ),
            ],
            successors: HashSet::from([1]),
        };

        if stack_frame_size > 0 {
            bb.instructions.push(Instruction::new(
                Opcode::Sub64ri as mir::Opcode,
                vec![
                    Operand::Register(
                        mir::Register::Physical(Register::Rsp as mir::PhysicalRegister),
                        RegisterRole::Use,
                    ),
                    Operand::Immediate(stack_frame_size as u64),
                ],
            ));
        }

        bb.instructions.push(Instruction::new(
            Opcode::Jmp as mir::Opcode,
            vec![Operand::Block(1)],
        ));

        let mut patch = FunctionPatch::new();

        patch.add_basic_block(0, bb);
        patch.apply(func);

        for bb in &mut func.blocks {
            let terminator = bb.instructions.last().unwrap();

            if terminator.opcode == Opcode::Ret as mir::Opcode {
                let mut patch = BasicBlockPatch::new();

                patch.add_instruction(
                    bb.instructions.len() - 1,
                    Instruction::new(Opcode::Leave as mir::Opcode, vec![]),
                );

                patch.apply(bb);
            }
        }
    }
}
