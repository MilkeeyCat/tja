pub mod amd64;

use crate::{
    hir::{self, FuncLoweringCtx, TyStorage},
    lir::{self, ParamRanges},
};

#[allow(private_interfaces)]
pub trait Target {
    fn abi(&self) -> &dyn Abi;
}

pub(crate) trait Abi {
    fn field_offset(&self, ty_storage: &TyStorage, fields: &[hir::TyIdx], idx: usize) -> usize;
    fn hir_ty_size(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize;
    fn lir_ty_size(&self, ty: lir::Ty) -> usize;
    fn alignment(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize;

    fn calling_conv(&self) -> &dyn CallingConvention;
}

pub(crate) trait CallingConvention {
    fn lower_signature(
        &self,
        abi: &dyn Abi,
        ty_storage: &TyStorage,
        sig: &hir::Signature,
    ) -> (lir::Signature, ParamRanges);
    fn lower_entry_block_params(
        &self,
        ctx: &mut FuncLoweringCtx,
        hir_params: &[hir::Value],
        lir_params: &[(lir::signature::Value, lir::Value)],
        param_ranges: &ParamRanges,
    );
    fn lower_ret(
        &self,
        ctx: &mut FuncLoweringCtx,
        value: Option<(hir::Value, &[lir::signature::Value])>,
    );
}
