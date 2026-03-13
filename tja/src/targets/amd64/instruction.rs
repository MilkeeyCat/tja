use super::{AddressMode, Memory, ReadWrite};
use crate::{
    mir::{self, BlockIdx},
    targets::Register,
};

include!(concat!(env!("OUT_DIR"), "/amd64/instruction.rs"));

impl<R: Register> mir::Instruction for Instruction<R> {
    type Register = R;
}
