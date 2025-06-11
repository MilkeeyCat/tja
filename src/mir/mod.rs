mod function;
pub mod interference_graph;
mod opcode;

use crate::hir::{self, FunctionIdx};
pub use function::Function;
pub use opcode::{GenericOpcode, Opcode};
use std::collections::HashSet;

pub type VregIdx = usize;
pub type StackFrameIdx = usize;
pub type RegisterClass = usize;
pub type PhysicalRegister = usize;
pub type BlockIdx = hir::BlockIdx;

#[derive(Debug)]
pub struct Mir<'hir>(pub Vec<Module<'hir>>);

#[derive(Debug)]
pub struct Module<'hir> {
    pub name: &'hir str,
    pub globals: &'hir [hir::Global],
    pub functions: Vec<Function<'hir>>,
}

#[derive(Debug)]
pub struct BasicBlock<'hir> {
    pub name: &'hir str,
    pub instructions: Vec<Instruction>,
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

#[derive(Debug, Clone)]
pub enum Operand {
    Register(Register, RegisterRole),
    Frame(StackFrameIdx),
    Global(hir::GlobalIdx),
    Function(FunctionIdx),
    Block(BlockIdx),
    Immediate(u64),
}

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: Vec<Operand>,
}

impl Instruction {
    fn defs(&self) -> HashSet<Register> {
        self.operands
            .iter()
            .filter_map(|operand| match operand {
                Operand::Register(r, RegisterRole::Def) => Some(r.clone()),
                _ => None,
            })
            .collect()
    }

    fn uses(&self) -> HashSet<Register> {
        self.operands
            .iter()
            .filter_map(|operand| match operand {
                Operand::Register(r, RegisterRole::Use) => Some(r.clone()),
                _ => None,
            })
            .collect()
    }

    fn next(&self) -> HashSet<BlockIdx> {
        self.operands
            .iter()
            .filter_map(|operand| match operand {
                Operand::Block(idx) => Some(*idx),
                _ => None,
            })
            .collect()
    }
}
