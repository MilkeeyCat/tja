use super::{BasicBlock, Register, RegisterId, Ty};
use std::collections::HashSet;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<RegisterId>,
    pub ret_ty: Ty,
    pub blocks: Vec<BasicBlock>,
    pub registers: Vec<Register>,
}

impl Function {
    pub fn liveness(&self) -> Vec<(HashSet<RegisterId>, HashSet<RegisterId>)> {
        let mut liveness: Vec<_> = self
            .blocks
            .iter()
            .map(|_| (HashSet::new(), HashSet::new()))
            .collect();

        loop {
            let mut done = true;

            // Some dude said that in reverse is better
            for (id, block) in self.blocks.iter().enumerate().rev() {
                // out[v] = ∪ in[w] where w ∈ succ(v)
                let live_out = block
                    .successors()
                    .into_iter()
                    .map(|id| liveness[id].0.clone())
                    .reduce(|acc, el| acc.union(&el).cloned().collect())
                    .unwrap_or_default();
                // in[v] = use(v) ∪ (out[v] - def(v))
                let live_in = block
                    .uses()
                    .union(&(&liveness[id].1 - &block.defs()))
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

    pub fn interference(&self) -> HashSet<(RegisterId, RegisterId)> {
        let mut edges = HashSet::new();
        let liveness = self.liveness();

        for (i, block) in self.blocks.iter().enumerate() {
            for def in block.defs() {
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
