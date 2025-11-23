pub mod basic_block;
pub mod function;
mod instruction;
mod opcode;
pub mod pass;
pub mod passes;
pub mod pattern_match;

use crate::{FunctionIdx, Global, GlobalIdx, macros::usize_wrapper};
pub use basic_block::{
    BasicBlock, BlockIdx, Cursor as BasicBlockCursor, CursorMut as BasicBlockCursorMut,
};
use derive_more::From;
pub use function::{FrameIdx, Function, RegisterClass, VregIdx};
use index_vec::{IndexVec, define_index_type};
pub use instruction::{
    Builder as InstrBuilder, Cursor as InstructionCursor, CursorMut as InstructionCursorMut,
    Instruction, InstructionIdx, ManualBuilder as InstructionBuilder,
};
pub use opcode::{GenericOpcode, Opcode};

usize_wrapper! {PhysicalRegister}

define_index_type! {
    pub struct OperandIdx = usize;
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub globals: IndexVec<GlobalIdx, Global>,
    pub functions: IndexVec<FunctionIdx, Function>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegisterRole {
    Def,
    Use,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Register {
    Virtual(VregIdx),
    Physical(PhysicalRegister),
}

impl Register {
    pub fn expect_virtual(&self) -> &VregIdx {
        match self {
            Self::Virtual(idx) => idx,
            _ => unreachable!("expected virtual register"),
        }
    }
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

    pub fn expect_register(&self) -> (&Register, &RegisterRole) {
        match self {
            Self::Register(reg, role) => (reg, role),
            _ => unreachable!("expected register"),
        }
    }

    pub fn expect_frame_idx(&self) -> &FrameIdx {
        match self {
            Self::Frame(idx) => idx,
            _ => unreachable!("expected frame index"),
        }
    }
}

pub trait OperandInfo: IntoIterator<Item = Operand> {
    /// Number of [Operand]s the operand consists of.
    ///
    /// Targets can define custom operands like and address operand for example,
    /// which can take multiple mir operands in [Instruction::operands].
    const LEN: usize;
}
