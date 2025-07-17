use crate::mir::Register;
use std::collections::HashSet;

#[derive(Default, Debug)]
pub struct DefsUses {
    pub defs: HashSet<Register>,
    pub uses: HashSet<Register>,
}

#[derive(Default, Clone, Debug)]
pub struct Liveness {
    pub ins: HashSet<Register>,
    pub outs: HashSet<Register>,
}
