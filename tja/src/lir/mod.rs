mod basic_block;
mod function;
mod instruction;
pub(crate) mod module;
pub(crate) mod signature;
mod ty;

pub(crate) use basic_block::Builder as BlockBuilder;
use basic_block::{Block, BlockId};
use function::Function;
use instruction::{Instruction, InstructionId, Terminator};
pub(crate) use module::{
    Builder as ModuleBuilder, FunctionDeclaration, GlobalVariable, GlobalVariableDeclaration,
    GlobalVariableValue, Module,
};
pub(crate) use ty::Ty;

use crate::{FunctionIdx, GlobalVariableIdx};

#[allow(private_interfaces)]
#[derive(Clone, Copy)]
pub(crate) enum Value {
    Param {
        ty: Ty,
        block: BlockId,
        idx: usize,
    },
    Instr {
        ty: Ty,
        instr: InstructionId,
        result_idx: usize,
    },
}

impl Value {
    pub(crate) fn ty(&self) -> Ty {
        match self {
            Self::Param { ty, .. } => *ty,
            Self::Instr { ty, .. } => *ty,
        }
    }
}

pub(crate) enum GlobalValueIdx {
    GlobalValue(GlobalVariableIdx),
    Function(FunctionIdx),
}
