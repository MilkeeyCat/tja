use crate::mir::{Block, BlockId, Instruction, InstructionId};
use slotmap::SlotMap;

struct InstructionNode<I: Instruction> {
    instr: I,
    prev: Option<InstructionId>,
    next: Option<InstructionId>,
}

struct BlockNode<I: Instruction> {
    block: Block<I>,
    prev: Option<BlockId>,
    next: Option<BlockId>,
}

pub(super) struct Function<I: Instruction> {
    instrs: SlotMap<InstructionId, InstructionNode<I>>,
    blocks: SlotMap<BlockId, BlockNode<I>>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}
