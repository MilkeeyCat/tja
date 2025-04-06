use super::{Function, Global, Wrapper, function::FunctionIdx, ty::TyIdx};
use std::rc::Rc;

pub type ModuleIdx = usize;

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub globals: Vec<Rc<Global>>,
    pub functions: Vec<Function>,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            globals: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn create_fn(&mut self, name: String, params: Vec<TyIdx>, ret_ty: TyIdx) -> FunctionIdx {
        let idx = self.functions.len();
        self.functions.push(Function {
            name,
            ret_ty,
            params_count: params.len(),
            blocks: Vec::new(),
            locals: params,
        });

        idx
    }

    pub fn get_fn_mut(&mut self, idx: FunctionIdx) -> &mut Function {
        &mut self.functions[idx]
    }
}

impl Wrapper<'_, &mut Module> {
    pub fn get_fn(&mut self, idx: FunctionIdx) -> Wrapper<&mut Function> {
        Wrapper {
            ty_storage: self.ty_storage,
            inner: &mut self.inner.functions[idx],
        }
    }
}
