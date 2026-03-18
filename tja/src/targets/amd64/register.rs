use super::RegisterClass;
use crate::{
    mir::{Function, GenericRegister, Instruction},
    targets,
};

include!(concat!(env!("OUT_DIR"), "/amd64/register.rs"));

impl targets::Register for Register {
    type RegisterClass = RegisterClass;

    fn class<
        I: Instruction<Register = impl targets::Register<RegisterClass = Self::RegisterClass>>,
    >(
        &self,
        _func: &Function<I>,
    ) -> Option<Self::RegisterClass> {
        todo!()
    }
}

impl From<Register> for GenericRegister<Register> {
    fn from(reg: Register) -> Self {
        Self::Physical(reg)
    }
}
