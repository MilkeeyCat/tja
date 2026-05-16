mod basic_block;
mod function;
mod instruction;
pub mod module;
mod ty;

pub use basic_block::{Block, BlockId, Builder as BlockBuilder};
pub use function::{Builder as FunctionBuilder, Function, FunctionIdx, Signature};
pub use instruction::{Instruction, InstructionId, Terminator};
pub use module::{Constant, Global, GlobalIdx, Immediate, Module};
pub use ty::{Storage as TyStorage, Ty, TyIdx};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Param {
        ty: TyIdx,
        block: BlockId,
        idx: usize,
    },
    Instr {
        ty: TyIdx,
        instr: InstructionId,
        result_idx: usize,
    },
}

impl Value {
    pub fn ty(&self) -> TyIdx {
        match self {
            Self::Param { ty, .. } => *ty,
            Self::Instr { ty, .. } => *ty,
        }
    }
}
