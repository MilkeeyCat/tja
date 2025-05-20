mod function;

use crate::hir::{self, FunctionIdx};
pub use function::Function;
use std::collections::HashSet;

pub type VregIdx = usize;
pub type StackFrameIdx = usize;
pub type RegisterClass = usize;
pub type Register = usize;
pub type Opcode = usize;
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
pub enum VregRole {
    Def,
    Use,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Vreg(VregIdx, VregRole),
    Reg(Register),
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
    fn defs(&self) -> HashSet<VregIdx> {
        self.operands
            .iter()
            .filter_map(|operand| match operand {
                Operand::Vreg(idx, VregRole::Def) => Some(*idx),
                _ => None,
            })
            .collect()
    }

    fn uses(&self) -> HashSet<VregIdx> {
        self.operands
            .iter()
            .filter_map(|operand| match operand {
                Operand::Vreg(idx, VregRole::Use) => Some(*idx),
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
