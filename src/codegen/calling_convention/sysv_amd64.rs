use super::CallingConvention;
use crate::{
    codegen::{
        CodeGen,
        allocator::{Allocator, Location},
    },
    repr::ty::TyIdx,
};

pub struct SysVAmd64;

impl SysVAmd64 {
    pub fn new() -> Self {
        Self
    }
}

impl CallingConvention for SysVAmd64 {
    fn compute(
        &self,
        codegen: &CodeGen,
        allocator: &mut Allocator,
        tys: &[TyIdx],
    ) -> Vec<Vec<Location>> {
        vec![]
    }
}
