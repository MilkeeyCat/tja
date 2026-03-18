pub mod basic_block;
pub mod function;
pub mod instruction;

use crate::{FunctionIdx, Global, GlobalIdx};
pub use basic_block::{
    BasicBlock, BlockIdx, Cursor as BasicBlockCursor, CursorMut as BasicBlockCursorMut,
};
pub use function::{FrameIdx, Function, GenericRegister, VregIdx};
use index_vec::IndexVec;
pub use instruction::{
    Cursor as InstructionCursor, CursorMut as InstructionCursorMut, GenericInstruction,
    Instruction, InstructionIdx,
};

pub struct Module<I: Instruction> {
    pub name: String,
    pub globals: IndexVec<GlobalIdx, Global>,
    pub functions: IndexVec<FunctionIdx, Function<I>>,
}
