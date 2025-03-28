use super::{BasicBlock, BlockIdx, LocalIdx, Terminator, Wrapper, basic_block, ty::TyIdx};
use std::collections::{HashMap, HashSet};

pub type FunctionIdx = usize;

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
    pub blocks: Vec<BasicBlock>,
    pub locals: Vec<TyIdx>,
}

impl Function {
    pub fn param(&self, idx: usize) -> TyIdx {
        assert!(idx <= self.params_count, "invalid param index");
        self.locals[idx]
    }

    pub fn create_block(&mut self, name: String, terminator: Terminator) -> BlockIdx {
        let idx = self.blocks.len();
        self.blocks.push(BasicBlock {
            name,
            instructions: Vec::new(),
            terminator,
        });

        idx
    }

    fn defs_uses(&self) -> Vec<DefUseBlock> {
        let mut basic_block_to_def_use_block = HashMap::new();
        let mut blocks = Vec::new();

        for (i, block) in self.blocks.iter().enumerate() {
            for instruction in &block.instructions {
                blocks.push(DefUseBlock {
                    defs: HashSet::from(instruction.def().map(|def| [def]).unwrap_or_default()),
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

            if let Some(block) = blocks.last_mut() {
                block.next.clear();
            }

            basic_block_to_def_use_block.insert(i, blocks.len() - 1);
        }

        for (i, block) in self.blocks.iter().enumerate() {
            match block.terminator {
                Terminator::Goto(block_id) => {
                    blocks[basic_block_to_def_use_block[&i]].next =
                        HashSet::from([basic_block_to_def_use_block[&block_id]]);
                }
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
}

impl Wrapper<'_, &mut Function> {
    pub fn get_block(&mut self, idx: BlockIdx) -> basic_block::Wrapper {
        basic_block::Wrapper {
            ty_storage: self.ty_storage,
            fn_locals: &mut self.inner.locals,
            block: &mut self.inner.blocks[idx],
        }
    }
}
