use crate::mir::{
    self,
    amd64::{Register, Target},
};

pub enum Instruction {}

impl mir::Instruction for Instruction {
    type Target = Target;
    type PhysicalRegister = Register;
}
