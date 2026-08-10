use crate::{
    FunctionIdx, GlobalVariableIdx,
    mir::{self, amd64::Register},
};
use derive_more::{Display, From};

#[derive(Debug, Clone, Copy, From, Display)]
#[from(u8, i8, u16, i16, u32, i32, i64)]
pub(super) struct Immediate(i64);

#[derive(Debug)]
pub(super) struct Memory(EffectiveAddress);

#[derive(Debug)]
pub(super) enum Base {
    Register(mir::Register<Register>),
    Function(FunctionIdx),
    Global(GlobalVariableIdx),
}

#[derive(Debug)]
pub(super) enum Scale {
    One,
    Two,
    Four,
    Eight,
}

#[derive(Debug)]
pub(super) struct EffectiveAddress {
    pub(super) base: Base,
    pub(super) index: Option<mir::Register<Register>>,
    pub(super) scale: Scale,
    pub(super) displacement: Option<isize>,
}
