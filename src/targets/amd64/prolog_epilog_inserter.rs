use super::Opcode;
use crate::{
    mir::{
        self, BasicBlock, BasicBlockPatch, Function, Instruction, Operand, PhysicalRegister,
        RegisterRole, function::FunctionPatch,
    },
    pass::{Context, Pass},
    targets::{Abi, RegisterInfo, Target, amd64::Register},
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
            .filter_map(|r1| {
                regs.iter()
                    .any(|r| match r {
                        mir::Register::Virtual(_) => false,
                        mir::Register::Physical(r2) => ctx.target.register_info().overlaps(r1, r2),
                    })
                    .then_some(*r1)
            })
            .collect();

        let mut bb = BasicBlock {
            name: "prolog".into(),
            instructions: Vec::new(),
            successors: HashSet::from([1]),
        };

        for r in &used_callee_saved_regs {
            bb.instructions.push(Instruction::new(
                Opcode::Push64r as mir::Opcode,
                vec![Operand::Register(
                    mir::Register::Physical(*r),
                    RegisterRole::Use,
                )],
            ));
        }
        if stack_frame_size > 0 {
            bb.instructions.extend([
                Instruction::new(
                    Opcode::Mov64rr as mir::Opcode,
                    vec![
                        Operand::Register(
                            mir::Register::Physical(Register::Rbp as mir::PhysicalRegister),
                            RegisterRole::Def,
                        ),
                        Operand::Register(
                            mir::Register::Physical(Register::Rsp as mir::PhysicalRegister),
                            RegisterRole::Use,
                        ),
                    ],
                ),
                Instruction::new(
                    Opcode::Sub64ri as mir::Opcode,
                    vec![
                        Operand::Register(
                            mir::Register::Physical(Register::Rsp as mir::PhysicalRegister),
                            RegisterRole::Use,
                        ),
                        Operand::Immediate(stack_frame_size as u64),
                    ],
                ),
            ]);
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

                for r in &used_callee_saved_regs {
                    patch.add_instruction(
                        bb.instructions.len() - 1,
                        Instruction::new(
                            Opcode::Pop64r as mir::Opcode,
                            vec![Operand::Register(
                                mir::Register::Physical(*r),
                                RegisterRole::Def,
                            )],
                        ),
                    );
                }

                if stack_frame_size > 0 {
                    patch.add_instruction(
                        bb.instructions.len() - 1,
                        Instruction::new(
                            Opcode::Add64ri as mir::Opcode,
                            vec![
                                Operand::Register(
                                    mir::Register::Physical(Register::Rsp as mir::PhysicalRegister),
                                    RegisterRole::Use,
                                ),
                                Operand::Immediate(stack_frame_size as u64),
                            ],
                        ),
                    );
                }

                patch.apply(bb);
            }
        }
    }
}
