pub mod basic_block;
pub mod function;
pub mod instruction;

pub use basic_block::{
    BasicBlock, BlockIdx, Cursor as BasicBlockCursor, CursorMut as BasicBlockCursorMut,
};
pub use function::{FrameIdx, Function, GenericRegister, VregIdx};
pub use instruction::{
    Cursor as InstructionCursor, CursorMut as InstructionCursorMut, GenericInstruction,
    Instruction, InstructionIdx,
};
