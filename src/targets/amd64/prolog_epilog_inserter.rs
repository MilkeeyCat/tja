use super::Opcode;
use crate::{
    mir::{
        self, BasicBlock, BasicBlockPatch, Function, Instruction, Operand, PhysicalRegister,
        RegisterRole, StackFrameIdx, function::FunctionPatch,
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
            instructions: vec![Instruction::new(
                Opcode::Push64r as mir::Opcode,
                vec![Operand::Register(
                    mir::Register::Physical(Register::Rbp as PhysicalRegister),
                    RegisterRole::Use,
                )],
            )],
            successors: HashSet::from([1]),
        };

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

        for r in &used_callee_saved_regs {
            let idx = func.next_stack_frame_idx;

            regs_stack_slots.push(idx);
            func.next_stack_frame_idx += 1;
            func.stack_slots.insert(idx, 8);

            let mut operands = vec![Operand::Register(
                mir::Register::Physical(*r),
                RegisterRole::Use,
            )];
            let address_mode = AddressMode {
                base: Base::Frame(idx),
                index: None,
                scale: 1,
                displacement: None,
            };

            address_mode.write(&mut operands, 0);

            bb.instructions
                .push(Instruction::new(Opcode::Mov64mr as mir::Opcode, operands));
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

                for (r, frame_idx) in used_callee_saved_regs.iter().zip(&regs_stack_slots) {
                    let mut operands = vec![Operand::Register(
                        mir::Register::Physical(*r),
                        RegisterRole::Use,
                    )];
                    let address_mode = AddressMode {
                        base: Base::Frame(*frame_idx),
                        index: None,
                        scale: 1,
                        displacement: None,
                    };

                    address_mode.write(&mut operands, 1);
                    patch.add_instruction(
                        bb.instructions.len() - 1,
                        Instruction::new(Opcode::Mov64rm as mir::Opcode, operands),
                    );
                }

                patch.apply(bb);
            }
        }
    }
}
