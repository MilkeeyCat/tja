use super::Opcode;
use crate::{
    mir::{self, BasicBlock, BasicBlockPatch, Function, Instruction, Operand, RegisterRole},
    pass::{Context, Pass},
    targets::{Target, amd64::Register},
};

#[derive(Default)]
pub struct PrologueEpilogueInsterter;

impl<'a, T: Target> Pass<'a, Function, T> for PrologueEpilogueInsterter {
    fn run(&self, func: &mut Function, _ctx: &mut Context<'a, T>) {
        let stack_frame_size = func
            .stack_slots
            .values()
            .sum::<usize>()
            .next_multiple_of(16);

        func.blocks.insert(
            0,
            BasicBlock {
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
            },
        );

        if stack_frame_size > 0 {
            func.blocks[0].instructions.push(Instruction::new(
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

        func.blocks[0].instructions.push(Instruction::new(
            Opcode::Jmp as mir::Opcode,
            vec![Operand::Block(1)],
        ));

        for bb in func.blocks.iter_mut().skip(1) {
            for instr in &mut bb.instructions {
                for operand in &mut instr.operands {
                    if let Operand::Block(idx) = operand {
                        *idx += 1;
                    }
                }
            }
        }

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
