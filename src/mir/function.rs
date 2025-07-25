use super::{
    Operand, Register, RegisterClass, StackFrameIdx, VregIdx,
    interference_graph::{InterferenceGraph, NodeId},
};
use crate::{
    dataflow::Liveness,
    hir::ty::TyIdx,
    mir::{BasicBlock, BlockIdx},
};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub next_vreg_idx: VregIdx,
    pub vreg_classes: HashMap<VregIdx, RegisterClass>,
    pub vreg_types: HashMap<VregIdx, TyIdx>,
    pub next_stack_frame_idx: StackFrameIdx,
    pub stack_slots: HashMap<StackFrameIdx, usize>,
    pub blocks: Vec<BasicBlock>,
}

impl Function {
    pub fn liveness(&self) -> Vec<Liveness> {
        let defs_uses: Vec<_> = self.blocks.iter().map(|bb| bb.defs_uses()).collect();
        let mut liveness = vec![Liveness::default(); defs_uses.len()];

        loop {
            let mut done = true;

            // Some dude said that in reverse is better
            for (i, (defs_uses, block)) in defs_uses.iter().zip(&self.blocks).enumerate().rev() {
                // out[v] = ∪ in[w] where w ∈ succ(v)
                let live_out = block
                    .successors
                    .iter()
                    .map(|id| liveness[*id].outs.clone())
                    .reduce(|acc, el| acc.union(&el).cloned().collect())
                    .unwrap_or_default();
                // in[v] = use(v) ∪ (out[v] - def(v))
                let live_in = defs_uses
                    .uses
                    .union(&(&liveness[i].outs - &defs_uses.defs))
                    .cloned()
                    .collect();

                if &liveness[i].ins != &live_in || &liveness[i].outs != &live_out {
                    done &= false;
                }

                liveness[i].ins = live_in;
                liveness[i].outs = live_out;
            }

            if done {
                break;
            }
        }

        liveness
    }

    pub fn interference(&self) -> (InterferenceGraph, Vec<NodeId>) {
        let mut graph = InterferenceGraph::new();
        let r_to_node_idx: HashMap<Register, NodeId> = self
            .registers()
            .into_iter()
            .map(|r| (r.clone(), graph.add_node(r)))
            .collect();
        let node_ids: Vec<NodeId> = self
            .registers()
            .into_iter()
            .filter_map(|r| match r {
                Register::Virtual(_) => Some(r_to_node_idx[&r]),
                Register::Physical(_) => None,
            })
            .collect();

        for (bb, bb_liveness) in self.blocks.iter().zip(self.liveness()) {
            for (instr_liveness, instr) in bb
                .liveness(bb_liveness.outs)
                .into_iter()
                .zip(&bb.instructions)
            {
                for def in instr.defs_uses().defs {
                    for out in &instr_liveness.outs {
                        let a = r_to_node_idx[out];
                        let b = r_to_node_idx[&def];

                        if a != b {
                            graph.add_edge(a, b);
                        }
                    }
                }
            }
        }

        (graph, node_ids)
    }

    pub fn registers(&self) -> Vec<Register> {
        let mut registers = Vec::new();

        for bb in &self.blocks {
            for instr in &bb.instructions {
                for operand in &instr.operands {
                    if let Operand::Register(r, _) = operand {
                        if !registers.contains(r) {
                            registers.push(r.clone());
                        }
                    }
                }
            }
        }

        registers
    }
}

pub struct FunctionPatch {
    new_basic_blocks: Vec<(BlockIdx, BasicBlock)>,
}

impl FunctionPatch {
    pub fn new() -> Self {
        Self {
            new_basic_blocks: Vec::new(),
        }
    }

    pub fn add_basic_block(&mut self, bb_idx: BlockIdx, bb: BasicBlock) {
        self.new_basic_blocks.push((bb_idx, bb));
    }

    pub fn apply(self, func: &mut Function) {
        let mut new_basic_blocks = self.new_basic_blocks;

        new_basic_blocks.sort_by_key(|(instr_idx, _)| (*instr_idx));

        for (bb_idx, bb) in new_basic_blocks.into_iter().rev() {
            for bb in &mut func.blocks[bb_idx..] {
                for instr in &mut bb.instructions {
                    for operand in &mut instr.operands {
                        if let Operand::Block(idx) = operand {
                            if *idx >= bb_idx {
                                *idx += 1;
                            }
                        }
                    }
                }

                bb.successors = std::mem::take(&mut bb.successors)
                    .into_iter()
                    .map(|idx| idx + 1)
                    .collect();
            }
            func.blocks.insert(bb_idx, bb);
        }
    }
}
