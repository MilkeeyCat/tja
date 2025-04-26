pub mod sysv_amd64;

use super::{
    CodeGen,
    allocator::{Allocator, Location},
};
use crate::repr::ty::TyIdx;

pub trait CallingConvention {
    fn compute(
        &self,
        codegen: &CodeGen,
        allocator: &mut Allocator,
        tys: &[TyIdx],
    ) -> Vec<Vec<Location>>;
}
