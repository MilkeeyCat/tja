use crate::{
    mir,
    targets::{self, amd64::Register},
};
use derive_more::From;
pub use generated::*;

mod generated {
    use crate::{
        mir::BlockIdx,
        targets::{
            Register,
            amd64::{AddressMode, Memory, ReadWrite},
        },
    };

    include!(concat!(env!("OUT_DIR"), "/amd64/instruction.rs"));
}

#[derive(Debug, From)]
pub enum Instruction<R: targets::Register> {
    Call(Call<R>),
    Target(generated::Instruction<R>),
}

impl<R: targets::Register> mir::Instruction for Instruction<R> {
    type Register = R;
}

#[derive(Debug)]
pub struct Call<R: targets::Register> {
    target: R,
    clobbers: Vec<Register>,
}
