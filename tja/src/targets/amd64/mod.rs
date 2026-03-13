mod abi;
mod calling_convention;
pub mod instruction;
mod operands;

use crate::mir::{self, GenericInstruction, GenericRegister};
use derive_more::Display;
pub use generated::{Register, RegisterClass};
pub use instruction::Instruction;
pub use operands::{AddressMode, Base, Memory, ReadWrite};

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

impl From<Register> for GenericRegister<Register> {
    fn from(reg: Register) -> Self {
        Self::Physical(reg)
    }
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

#[derive(Debug, Display)]
pub enum ConditionCode {
    #[display("a")]
    Above,
    #[display("ae")]
    AboveEqual,
    #[display("b")]
    Below,
    #[display("be")]
    BelowEqual,
    #[display("e")]
    Equal,
    #[display("ne")]
    NotEqual,
    #[display("g")]
    Greater,
    #[display("ge")]
    GreaterEqual,
    #[display("l")]
    Less,
    #[display("le")]
    LessEqual,
}
