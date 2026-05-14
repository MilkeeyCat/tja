use crate::hir::{Constant, Value};
use slotmap::new_key_type;
use smallvec::SmallVec;

new_key_type! {
    pub struct InstructionId;
}

pub enum Instruction {
    Const { const_: Constant },
}

pub enum Terminator {
    Return(SmallVec<[Value; 1]>),
}
