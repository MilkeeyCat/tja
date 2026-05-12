use crate::hir::ty::TyIdx;
use index_vec::define_index_type;

define_index_type! {
    pub struct FunctionIdx = usize;
}

pub struct Signature {
    params: Vec<TyIdx>,
    returns: Vec<TyIdx>,
}

impl Signature {
    pub fn new(params: Vec<TyIdx>, returns: Vec<TyIdx>) -> Self {
        Self { params, returns }
    }
}

#[derive(Default)]
pub struct Function {}
