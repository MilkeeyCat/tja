use crate::mir::Register;
use indexmap::IndexSet;

#[derive(Default, Debug)]
pub struct DefsUses {
    pub defs: IndexSet<Register>,
    pub uses: IndexSet<Register>,
}

#[derive(Default, Clone, Debug)]
pub struct Liveness {
    pub ins: IndexSet<Register>,
    pub outs: IndexSet<Register>,
}
