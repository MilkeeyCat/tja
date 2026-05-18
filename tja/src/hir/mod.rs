mod basic_block;
mod function;
mod instruction;
pub mod module;
mod ty;

pub use basic_block::{Block, BlockId, Builder as BlockBuilder};
pub use function::{Builder as FunctionBuilder, Function, FunctionIdx, Signature};
pub use instruction::{Instruction, InstructionId, Terminator};
pub use module::{Constant, Global, GlobalIdx, Immediate, Module};
use std::{collections::BTreeMap, fmt::Display};
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

    pub(super) fn display<'a>(
        &'a self,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    ) -> DisplayValue<'a> {
        DisplayValue {
            instr_to_idx,
            value: self,
        }
    }
}

pub struct DisplayValue<'a> {
    instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    value: &'a Value,
}

impl Display for DisplayValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Value::Param { idx, .. } => write!(f, "p{}", idx),
            Value::Instr {
                instr, result_idx, ..
            } => write!(f, "v{}_{}", self.instr_to_idx[instr], result_idx),
        }
    }
}
