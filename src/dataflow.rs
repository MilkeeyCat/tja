use crate::{datastructures::vecset::VecSet, mir::Register};

#[derive(Default, Debug)]
pub struct DefsUses {
    pub defs: VecSet<Register>,
    pub uses: VecSet<Register>,
}

#[derive(Default, Clone, Debug)]
pub struct Liveness {
    pub ins: VecSet<Register>,
    pub outs: VecSet<Register>,
}
