use super::{
    BasicBlock, Operand, PhysicalRegister, Register, RegisterClass, RegisterRole, StackFrameIdx,
    VregIdx,
    interference_graph::{InterferenceGraph, NodeId},
};
use crate::hir::ty::TyIdx;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Virtual(VregIdx, usize),
    Physical(PhysicalRegister),
}

#[derive(Debug)]
struct DefUseBlock {
    defs: HashSet<Value>,
    uses: HashSet<Value>,
    next: HashSet<usize>,
}

#[derive(Debug)]
pub struct Function<'hir> {
    pub name: &'hir str,
    pub next_vreg_idx: VregIdx,
    pub vreg_classes: HashMap<VregIdx, RegisterClass>,
    pub vreg_types: HashMap<VregIdx, TyIdx>,
    pub next_stack_frame_idx: StackFrameIdx,
    pub stack_slots: HashMap<StackFrameIdx, usize>,
    pub precolored_vregs: HashMap<VregIdx, PhysicalRegister>,
    pub blocks: Vec<BasicBlock<'hir>>,
}

impl Function<'_> {
    fn defs_uses(&self) -> Vec<DefUseBlock> {
        let mut basic_block_to_def_use_block = HashMap::new();
        let mut blocks = Vec::new();
        let mut vregs_occurences = HashMap::new();

        for (i, block) in self.blocks.iter().enumerate() {
            let last_blocks_len = blocks.len();

            for instr in &block.instructions {
                blocks.push(DefUseBlock {
                    defs: instr
                        .defs()
                        .into_iter()
                        .map(|r| match r {
                            Register::Virtual(idx) => {
                                let def_idx = vregs_occurences
                                    .entry(idx)
                                    .and_modify(|def_idx| *def_idx += 1)
                                    .or_default();

                                Value::Virtual(idx, *def_idx)
                            }
                            Register::Physical(r) => Value::Physical(r),
                        })
                        .collect(),
                    uses: instr
                        .uses()
                        .into_iter()
                        .map(|r| match r {
                            Register::Virtual(idx) => Value::Virtual(idx, vregs_occurences[&idx]),
                            Register::Physical(r) => Value::Physical(r),
                        })
                        .collect(),
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

            basic_block_to_def_use_block.insert(i, blocks.len() - 1);
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

    fn liveness(&self) -> Vec<(HashSet<Value>, HashSet<Value>)> {
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
        let value_to_node_idx: HashMap<Value, NodeId> = self
            .values()
            .into_iter()
            .map(|value| (value.clone(), graph.add_node(value)))
            .collect();
        let node_ids: Vec<NodeId> = self
            .values()
            .into_iter()
            .filter_map(|value| match value {
                Value::Virtual(_, _) => Some(value_to_node_idx[&value]),
                Value::Physical(_) => None,
            })
            .collect();

        for (i, block) in self.defs_uses().into_iter().enumerate() {
            for def in block.defs {
                for out in &liveness[i].1 {
                    let a = value_to_node_idx[out];
                    let b = value_to_node_idx[&def];

                    if a != b {
                        graph.add_edge(a, b);
                    }
                }
            }
        }

        (graph, node_ids)
    }

    pub fn values(&self) -> Vec<Value> {
        let mut values = Vec::new();
        let mut vregs_occurences = HashMap::new();

        for bb in &self.blocks {
            for instr in &bb.instructions {
                for operand in &instr.operands {
                    if let Operand::Register(r, role) = operand {
                        let value = match r {
                            Register::Virtual(idx) => {
                                let def_idx = vregs_occurences
                                    .entry(idx)
                                    .and_modify(|def_idx| {
                                        if let RegisterRole::Def = role {
                                            *def_idx += 1
                                        }
                                    })
                                    .or_default();

                                Value::Virtual(*idx, *def_idx)
                            }
                            Register::Physical(r) => Value::Physical(*r),
                        };

                        if !values.contains(&value) {
                            values.push(value);
                        }
                    }
                }
            }
        }

        values
    }
}
