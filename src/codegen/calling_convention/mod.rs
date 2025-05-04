pub mod sysv_amd64;

use super::{CodeGen, LocalLocation, allocator::Allocator};
use crate::repr::ty::TyIdx;

pub trait CallingConvention {
    fn precolor_parameters(&self, codegen: &CodeGen, allocator: &mut Allocator, tys: &[TyIdx]);
    fn arguments(&self, codegen: &CodeGen, tys: &[TyIdx]) -> Vec<LocalLocation>;
}
