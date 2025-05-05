pub mod sysv_amd64;

use super::{CodeGen, LocalLocation, allocator::Allocator};
use crate::repr::ty::TyIdx;

pub trait CallingConvention {
    fn precolor_parameters(
        &self,
        codegen: &CodeGen,
        allocator: &mut Allocator,
        ret_ty: TyIdx,
        tys: &[TyIdx],
    );
    fn arguments(&self, codegen: &CodeGen, ret_ty: TyIdx, tys: &[TyIdx]) -> Vec<LocalLocation>;
    fn returned_value_location(&self, codegen: &CodeGen, ty: TyIdx) -> LocalLocation;
}
