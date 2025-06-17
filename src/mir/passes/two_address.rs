use crate::mir::{BasicBlockPatch, Function, GenericOpcode, Instruction, Opcode};

pub fn two_address(func: &mut Function) {
    for bb in &mut func.blocks {
        let mut patch = BasicBlockPatch::new();

        for (idx, instr) in &mut bb.instructions.iter_mut().enumerate() {
            if let Some((lhs, rhs)) = std::mem::take(&mut instr.tied_operands) {
                patch.add_instruction(
                    idx,
                    Instruction::new(
                        GenericOpcode::Copy as Opcode,
                        vec![instr.operands[lhs].clone(), instr.operands[rhs].clone()],
                    ),
                );
                instr.operands.remove(rhs);
            }
        }

        patch.apply(bb);
    }
}
