use crate::{
    GlobalIdx,
    dataflow::DefsUses,
    hir::{self, BlockIdx},
    mir::{FrameIdx, GenericOpcode, Opcode, Operand, OperandIdx, Register, RegisterRole},
};
use std::collections::HashSet;

pub type InstructionIdx = hir::InstructionIdx;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: Vec<Operand>,
    pub tied_operands: Option<(OperandIdx, OperandIdx)>,
    pub implicit_defs: HashSet<Register>,
    pub implicit_uses: HashSet<Register>,
}

impl Instruction {
    pub fn new(opcode: Opcode) -> Self {
        Self {
            opcode,
            operands: Vec::new(),
            tied_operands: None,
            implicit_defs: HashSet::new(),
            implicit_uses: HashSet::new(),
        }
    }

    pub fn is_copy(&self) -> bool {
        self.opcode == GenericOpcode::Copy as Opcode
    }

    pub fn defs_uses(&self) -> DefsUses {
        DefsUses {
            defs: self
                .operands
                .iter()
                .filter_map(|operand| match operand {
                    Operand::Register(r, RegisterRole::Def) => Some(r.clone()),
                    _ => None,
                })
                .chain(self.implicit_defs.iter().cloned())
                .collect(),
            uses: self
                .operands
                .iter()
                .filter_map(|operand| match operand {
                    Operand::Register(r, RegisterRole::Use) => Some(r.clone()),
                    _ => None,
                })
                .chain(self.implicit_uses.iter().cloned())
                .collect(),
        }
    }

    pub fn add_def(&mut self, r: Register) -> &mut Self {
        self.operands.push(Operand::Register(r, RegisterRole::Def));

        self
    }

    pub fn add_use(&mut self, r: Register) -> &mut Self {
        self.operands.push(Operand::Register(r, RegisterRole::Use));

        self
    }

    pub fn add_operand(&mut self, operand: Operand) -> &mut Self {
        self.operands.push(operand);

        self
    }

    pub fn binary(opcode: Opcode, out: Register, lhs: Operand, rhs: Operand) -> Self {
        Builder::new(opcode)
            .add_def(out)
            .add_operand(lhs)
            .add_operand(rhs)
            .into()
    }

    pub fn frame_idx(lhs: Register, rhs: FrameIdx) -> Self {
        Builder::new(GenericOpcode::FrameIndex as Opcode)
            .add_def(lhs)
            .add_operand(Operand::Frame(rhs))
            .into()
    }

    pub fn ptr_add(out: Register, lhs: Operand, rhs: Operand) -> Self {
        assert!(matches!(
            rhs,
            Operand::Register(_, _) | Operand::Immediate(_)
        ));

        Builder::new(GenericOpcode::PtrAdd as Opcode)
            .add_def(out)
            .add_operand(lhs)
            .add_operand(rhs)
            .into()
    }

    pub fn load(lhs: Register, rhs: Operand) -> Self {
        Builder::new(GenericOpcode::Load as Opcode)
            .add_def(lhs)
            .add_operand(rhs)
            .into()
    }

    pub fn store(lhs: Register, rhs: Operand) -> Self {
        Builder::new(GenericOpcode::Store as Opcode)
            .add_use(lhs)
            .add_operand(rhs)
            .into()
    }

    pub fn br(idx: BlockIdx) -> Self {
        Builder::new(GenericOpcode::Br as Opcode)
            .add_operand(Operand::Block(idx))
            .into()
    }

    pub fn global_value(lhs: Register, rhs: GlobalIdx) -> Self {
        Builder::new(GenericOpcode::GlobalValue as Opcode)
            .add_def(lhs)
            .add_operand(Operand::Global(rhs))
            .into()
    }

    pub fn copy(lhs: Register, rhs: Operand) -> Self {
        Builder::new(GenericOpcode::Copy as Opcode)
            .add_def(lhs)
            .add_operand(rhs)
            .into()
    }
}

pub struct Builder(pub Instruction);

impl Builder {
    pub fn new(opcode: Opcode) -> Self {
        Self(Instruction::new(opcode))
    }

    pub fn add_def(mut self, r: Register) -> Self {
        self.0.add_def(r);

        self
    }

    pub fn add_use(mut self, r: Register) -> Self {
        self.0.add_use(r);

        self
    }

    pub fn add_operand(mut self, operand: Operand) -> Self {
        self.0.add_operand(operand);

        self
    }
}

impl From<Builder> for Instruction {
    fn from(value: Builder) -> Self {
        value.0
    }
}
