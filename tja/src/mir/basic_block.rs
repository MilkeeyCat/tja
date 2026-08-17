use crate::mir::{InstructionId, VregIdx};
use slotmap::new_key_type;

new_key_type! {
    pub(super) struct BlockId;
}

pub(super) struct Block {
    params: Vec<VregIdx>,
    pub(super) first_instr: Option<InstructionId>,
    pub(super) last_instr: Option<InstructionId>,
}
