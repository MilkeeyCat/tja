pub mod sysv_amd64;

use super::{CodeGen, Location, allocator::Allocator};
use crate::repr::ty::TyIdx;

pub trait CallingConvention {
    fn parameters(
        &self,
        codegen: &CodeGen,
        allocator: &mut Allocator,
        tys: &[TyIdx],
    ) -> Vec<Vec<Location>>;
    fn arguments(&self, codegen: &CodeGen, tys: &[TyIdx]) -> Vec<Vec<Location>>;
}
