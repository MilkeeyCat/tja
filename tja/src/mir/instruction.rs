use slotmap::new_key_type;

new_key_type! {
    pub(super) struct InstructionId;
}

pub(crate) trait Instruction {}
