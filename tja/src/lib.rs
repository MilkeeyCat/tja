mod generic_ir;
pub mod mir;

pub use generic_ir::hir;
pub(crate) use generic_ir::lir;
pub use generic_ir::target_instrs::*;

use crate::{
    hir::{Module, TyStorage},
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

pub fn compile<T: Target, M: Into<Module<T::TargetInstruction>>>(
    module: M,
    ty_storage: &TyStorage,
    target: &T,
) {
    let module = hir::lower(module.into(), ty_storage, target);

    lir::lower(target, module);
}
