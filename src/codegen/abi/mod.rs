pub mod sysv_amd64;

use super::calling_convention::CallingConvention;
use crate::hir::ty::{Storage, TyIdx};

pub trait Abi {
    fn field_offset(&self, storage: &Storage, fields: &[TyIdx], i: usize) -> usize;
    fn ty_size(&self, storage: &Storage, ty: TyIdx) -> usize;
    fn alignment(&self, storage: &Storage, ty: TyIdx) -> usize;
    fn calling_convention(&self) -> &dyn CallingConvention;
}
