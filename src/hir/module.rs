use super::{Function, GlobalIdx};
use crate::{Const, FunctionIdx, Global, ty::TyIdx};
use index_vec::IndexVec;
use index_vec::define_index_type;

define_index_type! {
    pub struct ModuleIdx = usize;
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub globals: IndexVec<GlobalIdx, Global>,
    pub functions: IndexVec<FunctionIdx, Function>,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            globals: IndexVec::new(),
            functions: IndexVec::new(),
        }
    }

    pub fn create_global(&mut self, name: String, ty: TyIdx, value: Option<Const>) -> GlobalIdx {
        self.globals.push(Global { name, ty, value })
    }

    pub fn create_fn(&mut self, name: String, params: Vec<TyIdx>, ret_ty: TyIdx) -> FunctionIdx {
        self.functions.push(Function {
            name,
            ret_ty,
            params_count: params.len(),
            blocks: IndexVec::new(),
            locals: params.into_iter().collect(),
        })
    }

    pub fn get_fn_mut(&mut self, idx: FunctionIdx) -> &mut Function {
        &mut self.functions[idx]
    }
}
