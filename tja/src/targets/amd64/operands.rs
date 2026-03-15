use crate::{FunctionIdx, GlobalIdx, mir::FrameIdx, targets::Register};
use derive_more::From;

#[derive(Debug)]
pub struct ReadWrite<R: Register> {
    read: R,
    write: R,
}

impl<R: Register> ReadWrite<R> {
    pub fn new(read: R, write: R) -> Self {
        Self { read, write }
    }
}

#[derive(Clone, Debug, From)]
pub enum Base<R: Register> {
    Register(R),
    Frame(FrameIdx),
    Function(FunctionIdx),
    Global(GlobalIdx),
}

#[derive(Clone, Debug)]
pub struct AddressMode<R: Register> {
    pub base: Base<R>,
    pub index: Option<R>,
    pub scale: usize,
    pub displacement: Option<isize>,
}

#[derive(Debug)]
pub struct Memory<R: Register>(AddressMode<R>);
