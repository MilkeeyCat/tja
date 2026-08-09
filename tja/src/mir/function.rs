use crate::mir::{Block, BlockId, Instruction, InstructionId, Target};
use slotmap::SlotMap;

struct InstructionNode<I: Instruction> {
    instr: I,
    prev: Option<InstructionId>,
    next: Option<InstructionId>,
}

struct BlockNode {
    block: Block,
    prev: Option<BlockId>,
    next: Option<BlockId>,
}

pub(super) struct Function<T: Target> {
    instrs: SlotMap<InstructionId, InstructionNode<T::TargetInstr>>,
    blocks: SlotMap<BlockId, BlockNode>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}
