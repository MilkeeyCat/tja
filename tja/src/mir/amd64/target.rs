use crate::{
    generic_ir::target_instrs::Amd64Instruction,
    mir::{
        Abi, InternalTarget,
        amd64::{Instruction, SysvAbi},
    },
};

pub struct Target {
    abi: SysvAbi,
}

impl Target {
    pub fn new() -> Self {
        Self {
            abi: SysvAbi::new(),
        }
    }
}

impl InternalTarget for Target {
    type GenericInstr = Amd64Instruction;
    type TargetInstr = Instruction;

    fn abi(&self) -> &dyn Abi<TargetInstruction = Self::GenericInstr> {
        &self.abi
    }
}
