mod amd64;
mod default;

pub use amd64::Instruction as Amd64Instruction;
pub use default::Instruction as DefaultInstruction;

pub(crate) trait InstrName {
    fn name(&self) -> &'static str;
}
