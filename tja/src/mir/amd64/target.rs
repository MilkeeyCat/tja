use crate::{
    generic_ir::target_instrs::Amd64Instruction,
    mir::{
        self, Abi,
        amd64::{self, SysvAbi},
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

#[allow(private_interfaces)]
impl mir::Target for Target {
    type TargetInstruction = Amd64Instruction;
    type Instr = amd64::Instruction;

    fn abi(&self) -> &dyn Abi<TargetInstruction = Self::TargetInstruction> {
        &self.abi
    }
}
