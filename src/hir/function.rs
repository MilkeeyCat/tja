use super::{BasicBlock, BlockIdx, Branch, LocalIdx, Terminator};
use crate::ty::TyIdx;
use index_vec::IndexVec;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct DefUseBlock {
    defs: HashSet<LocalIdx>,
    uses: HashSet<LocalIdx>,
    next: HashSet<usize>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub ret_ty: TyIdx,
    pub params_count: usize,
    pub blocks: IndexVec<BlockIdx, BasicBlock>,
    pub locals: IndexVec<LocalIdx, TyIdx>,
}

impl Function {
    pub fn param(&self, idx: usize) -> TyIdx {
        assert!(idx <= self.params_count, "invalid param index");
        self.locals[idx]
    }

    pub fn create_block(&mut self, name: Option<String>) -> BlockIdx {
        self.blocks.push(BasicBlock::new(
            name.unwrap_or_else(|| self.blocks.len().to_string()),
        ))
    }

    fn defs_uses(&self) -> Vec<DefUseBlock> {
        let mut basic_block_to_def_use_block = HashMap::new();
        let mut blocks = Vec::new();

        for (idx, block) in self.blocks.iter().enumerate() {
            let last_blocks_len = blocks.len();

            for instruction in &block.instructions {
                blocks.push(DefUseBlock {
                    defs: instruction
                        .def()
                        .map(|def| HashSet::from([def]))
                        .unwrap_or_default(),
                    uses: instruction.uses(),
                    next: HashSet::from([blocks.len() + 1]),
                });
            }

            let uses = block.terminator.uses();
            if !uses.is_empty() {
                blocks.push(DefUseBlock {
                    defs: HashSet::new(),
                    uses,
                    next: HashSet::new(),
                });
            }

            if last_blocks_len == blocks.len() {
                blocks.push(DefUseBlock {
                    defs: HashSet::new(),
                    uses: HashSet::new(),
                    next: HashSet::new(),
                });
            }

            if let Some(block) = blocks.last_mut() {
                block.next.clear();
            }

            basic_block_to_def_use_block.insert(BlockIdx::new(idx), blocks.len() - 1);
        }

        for (idx, block) in self.blocks.iter().enumerate() {
            match &block.terminator {
                Terminator::Br(branch) => match branch {
                    Branch::Conditional {
                        iftrue, iffalse, ..
                    } => {
                        blocks[basic_block_to_def_use_block[&BlockIdx::new(idx)]].next =
                            HashSet::from([
                                basic_block_to_def_use_block[&iftrue],
                                basic_block_to_def_use_block[&iffalse],
                            ]);
                    }
                    Branch::Unconditional { block_idx } => {
                        blocks[basic_block_to_def_use_block[&BlockIdx::new(idx)]].next =
                            HashSet::from([basic_block_to_def_use_block[&block_idx]]);
                    }
                },
                Terminator::Return(_) => (),
            };
        }

        blocks
    }

    fn liveness(&self) -> Vec<(HashSet<LocalIdx>, HashSet<LocalIdx>)> {
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

    pub fn interference(&self) -> HashSet<(LocalIdx, LocalIdx)> {
        let mut edges = HashSet::new();
        let liveness = self.liveness();

        for (i, block) in self.defs_uses().into_iter().enumerate() {
            for def in block.defs {
                for out in &liveness[i].1 {
                    if !edges.contains(&(*out, def)) && *out != def {
                        edges.insert((def, *out));
                    }
                }
            }
        }

        edges
    }

    pub fn is_declaration(&self) -> bool {
        self.blocks.is_empty()
    }
}
