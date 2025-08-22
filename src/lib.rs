pub mod codegen;
mod dataflow;
mod datastructures;
pub mod hir;
#[macro_use]
pub(crate) mod macros;
pub mod mir;
pub mod pass;
pub mod targets;
pub mod ty;

use crate::ty::TyIdx;
use index_vec::define_index_type;

define_index_type! {
    pub struct GlobalIdx = usize;
}

define_index_type! {
    pub struct FunctionIdx = usize;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    Global(GlobalIdx),
    Function(FunctionIdx),
    Int(u64),
    Aggregate(Vec<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Global {
    pub name: String,
    pub ty: TyIdx,
    pub value: Option<Const>,
}
