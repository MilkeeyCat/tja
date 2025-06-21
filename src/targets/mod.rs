pub mod amd64;

//use super::calling_convention::CallingConvention;
use crate::hir::ty::{Storage, TyIdx};
use crate::mir::{PhysicalRegister, RegisterClass};

pub trait RegisterInfo {
    fn get_registers_by_class(&self, class: &RegisterClass) -> &[PhysicalRegister];
    fn overlaps(&self, a: &PhysicalRegister, b: &PhysicalRegister) -> bool;
    fn get_name(&self, r: &PhysicalRegister) -> &'static str;
}

pub trait Target {
    type Abi: Abi;
    type RegisterInfo: RegisterInfo;

    fn abi(&self) -> &Self::Abi;
    fn register_info(&self) -> &Self::RegisterInfo;
}

pub trait Abi {
    fn field_offset(&self, storage: &Storage, fields: &[TyIdx], i: usize) -> usize;
    fn ty_size(&self, storage: &Storage, ty: TyIdx) -> usize;
    fn alignment(&self, storage: &Storage, ty: TyIdx) -> usize;
    //fn calling_convention(&self) -> &dyn CallingConvention;
}
