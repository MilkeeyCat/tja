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
use macros::usize_wrapper;

usize_wrapper! {GlobalIdx}
usize_wrapper! {FunctionIdx}

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
