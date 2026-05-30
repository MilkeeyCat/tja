use crate::mir::{self, Abi, amd64::SysvAbi};

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
    fn abi(&self) -> &dyn Abi {
        &self.abi
    }
}
