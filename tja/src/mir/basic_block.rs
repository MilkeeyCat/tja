use crate::mir::{Instruction, InstructionId, Register};
use slotmap::new_key_type;

new_key_type! {
    pub(super) struct BlockId;
}

pub(super) struct Block<I: Instruction> {
    // FIXME: wat to do here???
    params: Vec<Register<I::PhysicalRegister>>,
    first_instr: Option<InstructionId>,
    last_instr: Option<InstructionId>,
}
