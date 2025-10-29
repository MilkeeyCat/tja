pub mod basic_block;
pub mod function;
mod instruction;
mod opcode;
pub mod pass;
pub mod passes;

use crate::{FunctionIdx, Global, GlobalIdx, macros::usize_wrapper};
pub use basic_block::{BasicBlock, BasicBlockPatch};
use derive_more::From;
pub use function::{Cursor as FunctionCursor, FrameIdx, Function, RegisterClass, VregIdx};
use index_vec::{IndexVec, define_index_type};
pub use instruction::{Builder as InstrBuilder, Instruction, InstructionIdx};
pub use opcode::{GenericOpcode, Opcode};

usize_wrapper! {PhysicalRegister}

define_index_type! {
    pub struct OperandIdx = usize;
}

define_index_type! {
    pub struct BlockIdx = usize;
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub globals: IndexVec<GlobalIdx, Global>,
    pub functions: IndexVec<FunctionIdx, Function>,
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

#[derive(Debug, Clone, From)]
pub enum Operand {
    #[from(ignore)]
    Register(Register, RegisterRole),
    Frame(FrameIdx),
    Global(GlobalIdx),
    Function(FunctionIdx),
    Block(BlockIdx),
    #[from(ignore)]
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
