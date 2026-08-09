pub mod amd64;

use crate::{
    hir::{self, FuncLoweringCtx, TargetInstruction, TyStorage},
    lir::{self, ParamRanges},
};

pub(crate) trait InternalTarget {
    type GenericInstr: TargetInstruction;

    fn abi(&self) -> &dyn Abi<TargetInstruction = Self::GenericInstr>;
}

#[allow(private_bounds)]
pub trait Target: InternalTarget<GenericInstr = Self::TargetInstruction> {
    type TargetInstruction: TargetInstruction;
}

impl<TI: TargetInstruction, IT: InternalTarget<GenericInstr = TI>> Target for IT {
    type TargetInstruction = TI;
}

pub(crate) trait Abi {
    type TargetInstruction: TargetInstruction;

    fn field_offset(&self, ty_storage: &TyStorage, fields: &[hir::TyIdx], idx: usize) -> usize;
    fn hir_ty_size(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize;
    fn lir_ty_size(&self, ty: lir::Ty) -> usize;
    fn alignment(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize;

    fn calling_conv(&self) -> &dyn CallingConvention<TargetInstruction = Self::TargetInstruction>;
}

pub(crate) trait CallingConvention {
    type TargetInstruction: TargetInstruction;

    fn lower_signature(
        &self,
        abi: &dyn Abi<TargetInstruction = Self::TargetInstruction>,
        ty_storage: &TyStorage,
        sig: &hir::Signature,
    ) -> (lir::Signature, ParamRanges);
    fn lower_entry_block_params(
        &self,
        ctx: &mut FuncLoweringCtx<'_, Self::TargetInstruction>,
        hir_params: &[hir::Value],
        lir_params: &[(lir::signature::Value, lir::Value)],
        param_ranges: &ParamRanges,
    );
    fn lower_ret(
        &self,
        ctx: &mut FuncLoweringCtx<'_, Self::TargetInstruction>,
        value: Option<(hir::Value, &[lir::signature::Value])>,
    );
}
