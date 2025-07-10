pub mod function;
pub mod interference_graph;
mod opcode;
pub mod pass;
pub mod passes;

use crate::hir::{self, FunctionIdx};
pub use function::Function;
pub use opcode::{GenericOpcode, Opcode};
use std::collections::HashSet;

pub type VregIdx = usize;
pub type StackFrameIdx = usize;
pub type RegisterClass = usize;
pub type PhysicalRegister = usize;
pub type BlockIdx = hir::BlockIdx;
pub type InstructionIdx = hir::InstructionIdx;
pub type OperandIdx = usize;

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub globals: Vec<hir::Global>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub successors: HashSet<BlockIdx>,
}

pub struct BasicBlockPatch {
    new_instructions: Vec<(InstructionIdx, Instruction)>,
}

impl BasicBlockPatch {
    pub fn new() -> Self {
        Self {
            new_instructions: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, instr_idx: InstructionIdx, instr: Instruction) {
        self.new_instructions.push((instr_idx, instr));
    }

    pub fn apply(self, bb: &mut BasicBlock) {
        let mut new_instructions = self.new_instructions;

        new_instructions.sort_by_key(|(instr_idx, _)| (*instr_idx));

        for (instr_idx, instruction) in new_instructions.into_iter().rev() {
            bb.instructions.insert(instr_idx, instruction);
        }
    }
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

impl Operand {
    pub fn get_vreg_idx(&self) -> Option<&VregIdx> {
        if let Self::Register(Register::Virtual(idx), _) = self {
            Some(idx)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: Vec<Operand>,
    pub tied_operands: Option<(OperandIdx, OperandIdx)>,
}

impl Instruction {
    pub fn new(opcode: Opcode, operands: Vec<Operand>) -> Self {
        Self {
            opcode,
            operands,
            tied_operands: None,
        }
    }

    pub fn is_copy(&self) -> bool {
        self.opcode == GenericOpcode::Copy as Opcode
    }

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
