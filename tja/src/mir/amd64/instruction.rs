use crate::mir::{
    self, BlockId,
    amd64::{self, EffectiveAddress, Immediate, Memory},
};

type Register = mir::Register<amd64::Register>;

include!(concat!(env!("OUT_DIR"), "/amd64/instruction.rs"));

pub(crate) enum Instruction {
    Target(TargetInstruction),
}

impl mir::Instruction for Instruction {}
