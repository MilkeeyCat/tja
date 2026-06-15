mod generic_ir;
pub mod mir;

pub use generic_ir::hir;
pub(crate) use generic_ir::lir;

use crate::{
    hir::{Module, TyStorage, lower},
    mir::Target,
};
use derive_more::{Display, From};
use index_vec::define_index_type;

define_index_type! {
    pub struct GlobalVariableIdx = usize;
}

define_index_type! {
    pub struct FunctionIdx = usize;
}

#[derive(From, Clone, Copy, Display)]
#[from(u8, i8, u16, i16, u32, i32, i64)]
pub struct Immediate(i64);

pub fn compile(module: Module, ty_storage: &TyStorage, target: &dyn Target) {
    lower(module, ty_storage, target);
}
