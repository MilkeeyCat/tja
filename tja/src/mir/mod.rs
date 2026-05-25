pub mod amd64;

use crate::{
    hir::{self, TyStorage},
    lir,
};

#[allow(private_interfaces)]
pub trait Target {
    fn abi(&self) -> &dyn Abi;
}

pub(crate) trait Abi {
    fn field_offset(&self, ty_storage: &TyStorage, fields: &[hir::TyIdx], idx: usize) -> usize;
    fn ty_size(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize;
    fn alignment(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize;
}

pub(crate) trait CallingConvention {
    fn lower_signature(
        &self,
        abi: &dyn Abi,
        ty_storage: &TyStorage,
        sig: &hir::Signature,
    ) -> lir::signature::Signature;
}
