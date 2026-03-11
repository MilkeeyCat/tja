mod abi;
mod calling_convention;

use crate::mir::{self, GenericInstruction, GenericRegister};
pub use generated::{Register, RegisterClass};

mod generated {
    include!(concat!(env!("OUT_DIR"), "/amd64/register.rs"));

    include!(concat!(env!("OUT_DIR"), "/amd64/register_class.rs"));
}

impl super::RegisterClass for RegisterClass {}

impl super::Register for Register {
    type RegisterClass = RegisterClass;

    fn class<
        I: super::Instruction<Register = impl super::Register<RegisterClass = Self::RegisterClass>>,
    >(
        &self,
        _func: &mir::Function<I>,
    ) -> Option<Self::RegisterClass> {
        todo!()
    }
}

pub enum Instruction<R: super::Register> {
    Foo,
    Bar(R),
}

impl<R: super::Register> mir::Instruction for Instruction<R> {
    type Register = R;
}

pub struct Target {
    default_cc: calling_convention::SysV,
}

impl Target {
    pub fn new() -> Self {
        Self {
            default_cc: calling_convention::SysV,
        }
    }
}

impl super::Target for Target {
    type Abi = abi::SysV;
    type CallingConventionInstruction = GenericInstruction<Instruction<GenericRegister<Register>>>;

    fn get_calling_convention(&self) -> &dyn super::CallingConvention<Target = Self> {
        &self.default_cc
    }
}
