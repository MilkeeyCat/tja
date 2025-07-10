use super::{
    BasicBlock, Operand, Register, RegisterClass, StackFrameIdx, VregIdx,
    interference_graph::{InterferenceGraph, NodeId},
};
use crate::{hir::ty::TyIdx, mir::BlockIdx};
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct DefUseBlock {
    defs: HashSet<Register>,
    uses: HashSet<Register>,
    next: HashSet<usize>,
}

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
    fn defs_uses(&self) -> Vec<DefUseBlock> {
        let mut basic_block_to_def_use_block = HashMap::new();
        let mut blocks = Vec::new();

        for (i, block) in self.blocks.iter().enumerate() {
            let last_blocks_len = blocks.len();

            for instr in &block.instructions {
                blocks.push(DefUseBlock {
                    defs: instr.defs(),
                    uses: instr.uses(),
                    next: instr.next(),
                });
            }

            if last_blocks_len == blocks.len() {
                blocks.push(DefUseBlock {
                    defs: HashSet::new(),
                    uses: HashSet::new(),
                    next: HashSet::new(),
                });
            }

            basic_block_to_def_use_block.insert(i, last_blocks_len);
        }

        let len = blocks.len();
        for (i, block) in blocks.iter_mut().enumerate() {
            if i != len - 1 {
                let mut next: HashSet<_> = block
                    .next
                    .iter()
                    .map(|idx| basic_block_to_def_use_block[idx])
                    .collect();

                if next.is_empty() {
                    next.insert(i + 1);
                }

                block.next = next;
            }
        }

        blocks
    }

    fn liveness(&self) -> Vec<(HashSet<Register>, HashSet<Register>)> {
        let defs_uses = self.defs_uses();
        let mut liveness: Vec<_> = defs_uses
            .iter()
            .map(|_| (HashSet::new(), HashSet::new()))
            .collect();

        loop {
            let mut done = true;

            // Some dude said that in reverse is better
            for (id, block) in defs_uses.iter().enumerate().rev() {
                // out[v] = ∪ in[w] where w ∈ succ(v)
                let live_out = block
                    .next
                    .iter()
                    .map(|id| liveness[*id].0.clone())
                    .reduce(|acc, el| acc.union(&el).cloned().collect())
                    .unwrap_or_default();
                // in[v] = use(v) ∪ (out[v] - def(v))
                let live_in = block
                    .uses
                    .union(&(&liveness[id].1 - &block.defs))
                    .cloned()
                    .collect();

                if &liveness[id].0 != &live_in || &liveness[id].1 != &live_out {
                    done &= false;
                }

                liveness[id] = (live_in, live_out);
            }

            if done {
                break;
            }
        }

        liveness
    }

    pub fn interference(&self) -> (InterferenceGraph, Vec<NodeId>) {
        let mut graph = InterferenceGraph::new();
        let liveness = self.liveness();
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

        for (i, block) in self.defs_uses().into_iter().enumerate() {
            for def in block.defs {
                for out in &liveness[i].1 {
                    let a = r_to_node_idx[out];
                    let b = r_to_node_idx[&def];

                    if a != b {
                        graph.add_edge(a, b);
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
