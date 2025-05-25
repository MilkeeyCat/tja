pub mod amd64;

use crate::mir::{Register, RegisterClass};

pub trait RegisterInfo {
    fn get_registers_by_class(&self, class: &RegisterClass) -> &[Register];
    fn overlaps(&self, a: &Register, b: &Register) -> bool;
    fn get_name(&self, r: &Register) -> &'static str;
}

pub trait Target {
    fn abi(&self) -> &dyn Abi;
    fn register_info(&self) -> &dyn RegisterInfo;
}

//use super::calling_convention::CallingConvention;
use crate::hir::ty::{Storage, TyIdx};

pub trait Abi {
    fn field_offset(&self, storage: &Storage, fields: &[TyIdx], i: usize) -> usize;
    fn ty_size(&self, storage: &Storage, ty: TyIdx) -> usize;
    fn alignment(&self, storage: &Storage, ty: TyIdx) -> usize;
    //fn calling_convention(&self) -> &dyn CallingConvention;
}
