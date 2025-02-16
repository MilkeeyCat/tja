use super::{BlockId, Instruction, Place, RegisterId, Terminator};
use std::collections::HashSet;

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn defs(&self) -> HashSet<RegisterId> {
        self.instructions
            .iter()
            .filter_map(|ins| match ins {
                Instruction::Binary {
                    place: Place::Register(r),
                    ..
                } => Some(*r),
                Instruction::Copy { place, .. } => {
                    if let Place::Register(r) = place {
                        Some(*r)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect()
    }

    pub fn uses(&self) -> HashSet<RegisterId> {
        let mut uses: HashSet<_> = self
            .instructions
            .iter()
            .filter_map(|ins| match ins {
                Instruction::Binary { lhs, rhs, .. } => {
                    Some(vec![lhs.register_id(), rhs.register_id()])
                }
                Instruction::Copy { operand, .. } => Some(vec![operand.register_id()]),
            })
            .flatten()
            .flatten()
            .collect();

        match &self.terminator {
            Terminator::Return(Some(operand)) => {
                if let Some(id) = operand.register_id() {
                    uses.insert(id);
                }
            }
            _ => (),
        };

        &uses - &self.defs()
    }

    pub fn successors(&self) -> HashSet<BlockId> {
        match self.terminator {
            Terminator::Goto(block_id) => HashSet::from([block_id]),
            Terminator::Return(_) => HashSet::new(),
        }
    }
}
