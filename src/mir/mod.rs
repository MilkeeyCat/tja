pub mod basic_block;
pub mod function;
mod instruction;
mod opcode;
pub mod pass;
pub mod passes;

use crate::{FunctionIdx, Global, GlobalIdx, hir};
pub use basic_block::{BasicBlock, BasicBlockPatch};
pub use function::{FrameIdx, Function, RegisterClass, VregIdx};
pub use instruction::{Builder as InstrBuilder, Instruction, InstructionIdx};
pub use opcode::{GenericOpcode, Opcode};

pub type PhysicalRegister = usize;
pub type BlockIdx = hir::BlockIdx;
pub type OperandIdx = usize;

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub enum RegisterRole {
    Def,
    Use,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Register {
    Virtual(VregIdx),
    Physical(PhysicalRegister),
}

impl TryFrom<Operand> for Register {
    type Error = ();

    fn try_from(value: Operand) -> Result<Self, Self::Error> {
        match value {
            Operand::Register(reg, _) => Ok(reg),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Register(Register, RegisterRole),
    Frame(FrameIdx),
    Global(GlobalIdx),
    Function(FunctionIdx),
    Block(BlockIdx),
    Immediate(u64),
}

impl Operand {
    pub fn def(r: Register) -> Self {
        Self::Register(r, RegisterRole::Def)
    }

    // Can't use `use` -.-
    pub fn not_def(r: Register) -> Self {
        Self::Register(r, RegisterRole::Use)
    }

    pub fn get_vreg_idx(&self) -> Option<&VregIdx> {
        if let Self::Register(Register::Virtual(idx), _) = self {
            Some(idx)
        } else {
            None
        }
    }
}
