use crate::{
    Immediate,
    lir::{GlobalValueIdx, Value},
};
use slotmap::new_key_type;
use smallvec::SmallVec;

new_key_type! {
    pub(super) struct InstructionId;
}

pub(super) enum Instruction {
    Iconst { imm: Immediate },
    Load { ptr: Value },
    Store { ptr: Value, value: Value },
    Shl { value: Value, bits: Value },
    Lshr { value: Value, bits: Value },
    GlobalValuePtr { value: GlobalValueIdx },
}

pub(super) enum Terminator {
    Return(SmallVec<[Value; 1]>),
}
