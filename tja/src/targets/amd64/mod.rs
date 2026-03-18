mod abi;
mod calling_convention;
pub mod instruction;
mod operands;
mod register;
mod register_class;

use crate::mir::{GenericInstruction, GenericRegister};
use derive_more::Display;
pub use instruction::Instruction;
pub use operands::{AddressMode, Base, Memory, ReadWrite};
pub use register::Register;
pub use register_class::RegisterClass;

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
    type GenericInstruction = GenericInstruction<Instruction<GenericRegister<Register>>>;

    fn get_calling_convention(&self) -> &dyn super::CallingConvention<Target = Self> {
        &self.default_cc
    }
}

/// The terms "above" and "below" are associated with the CF flag and refer to
/// the relationship between two unsigned integer values. The terms "greater" and
/// "less" are associated with the SF and OF flags and refer to the relationship
/// between two signed integer values.
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
