pub mod basic_block;
pub mod function;
mod instruction;
mod opcode;
pub mod pass;
pub mod passes;

use crate::hir::{self, FunctionIdx};
pub use basic_block::{BasicBlock, BasicBlockPatch};
pub use function::Function;
pub use instruction::{Builder as InstrBuilder, Instruction, InstructionIdx};
pub use opcode::{GenericOpcode, Opcode};

pub type VregIdx = usize;
pub type StackFrameIdx = usize;
pub type RegisterClass = usize;
pub type PhysicalRegister = usize;
pub type BlockIdx = hir::BlockIdx;
pub type OperandIdx = usize;

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub globals: Vec<hir::Global>,
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
            Operand::Register(r, _) => Ok(r),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Register(Register, RegisterRole),
    Frame(StackFrameIdx),
    Global(hir::GlobalIdx),
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
