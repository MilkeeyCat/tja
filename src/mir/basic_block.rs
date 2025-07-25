use crate::{
    dataflow::{DefsUses, Liveness},
    mir::{BlockIdx, Instruction, InstructionIdx, Register},
};
use std::collections::HashSet;

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub successors: HashSet<BlockIdx>,
}

impl BasicBlock {
    pub fn defs_uses(&self) -> DefsUses {
        let mut defs_uses = DefsUses::default();

        for instr in &self.instructions {
            let instr_defs_uses = instr.defs_uses();

            defs_uses.defs.extend(instr_defs_uses.defs);
            defs_uses.uses.extend(instr_defs_uses.uses);
        }

        defs_uses.uses = &defs_uses.uses - &defs_uses.defs;

        defs_uses
    }

    pub fn liveness(&self, mut succ_live_in: HashSet<Register>) -> Vec<Liveness> {
        let defs_uses: Vec<_> = self
            .instructions
            .iter()
            .map(|instr| instr.defs_uses())
            .collect();
        let mut liveness = vec![Liveness::default(); defs_uses.len()];

        loop {
            let mut done = true;

            for (i, defs_uses) in defs_uses.iter().enumerate().rev() {
                // out[v] = ∪ in[w] where w ∈ succ(v)
                let live_out = succ_live_in.clone();
                // in[v] = use(v) ∪ (out[v] - def(v))
                let live_in = defs_uses
                    .uses
                    .union(&(&liveness[i].outs - &defs_uses.defs))
                    .cloned()
                    .collect();

                if &liveness[i].ins != &live_in || &liveness[i].outs != &live_out {
                    done &= false;
                }

                succ_live_in = live_out.clone();
                liveness[i].ins = live_in;
                liveness[i].outs = live_out;
            }

            if done {
                break;
            }
        }

        liveness
    }
}

pub struct BasicBlockPatch {
    new_instructions: Vec<(InstructionIdx, Instruction)>,
    deleted_instructions: Vec<InstructionIdx>,
}

impl BasicBlockPatch {
    pub fn new() -> Self {
        Self {
            new_instructions: Vec::new(),
            deleted_instructions: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, instr_idx: InstructionIdx, instr: Instruction) {
        self.new_instructions.push((instr_idx, instr));
    }

    pub fn delete_instruction(&mut self, instr_idx: InstructionIdx) {
        self.deleted_instructions.push(instr_idx);
    }

    pub fn apply(self, bb: &mut BasicBlock) {
        let mut new_instructions = self.new_instructions;
        let mut deleted_instructions = self.deleted_instructions;

        let mut instr_indices: Vec<InstructionIdx> = deleted_instructions
            .clone()
            .into_iter()
            .chain(new_instructions.iter().map(|(instr_idx, _)| *instr_idx))
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();

        instr_indices.sort_by_key(|instr_idx| *instr_idx);

        for instr_idx in instr_indices.into_iter().rev() {
            for _ in deleted_instructions.extract_if(.., |idx| *idx == instr_idx) {
                bb.instructions.remove(instr_idx);
            }

            for (instr_idx, instr) in new_instructions.extract_if(.., |(idx, _)| *idx == instr_idx)
            {
                bb.instructions.insert(instr_idx, instr);
            }
        }
    }
}
