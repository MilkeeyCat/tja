use crate::{
    mir::{BasicBlockPatch, Function, Instruction, InstructionIdx, Operand, RegisterRole},
    pass::{Context, Pass},
    targets::Target,
};

#[derive(Default)]
pub struct TwoAddressForm;

impl<'a, T: Target> Pass<'a, Function, T> for TwoAddressForm {
    fn run(&self, func: &mut Function, _ctx: &mut Context<'a, T>) {
        for bb in &mut func.blocks {
            let mut patch = BasicBlockPatch::new();

            for (idx, instr) in &mut bb.instructions.iter_mut().enumerate() {
                if let Some((lhs, rhs)) = std::mem::take(&mut instr.tied_operands) {
                    patch.add_instruction(
                        InstructionIdx(idx),
                        Instruction::copy(
                            instr.operands[*lhs].clone().try_into().unwrap(),
                            instr.operands[*rhs].clone(),
                        ),
                    );
                    instr.operands.remove(*rhs);

                    let reg = match instr.operands[*lhs].clone() {
                        Operand::Register(r, RegisterRole::Def) => r,
                        _ => unreachable!(),
                    };
                    instr.implicit_uses.insert(reg);
                }
            }

            patch.apply(bb);
        }
    }
}
