use super::{Function, Global, function::FunctionIdx, ty::Ty};
use std::rc::Rc;

#[derive(Debug)]
pub struct Module {
    pub globals: Vec<Rc<Global>>,
    pub functions: Vec<Function>,
    //TODO: add aggregate types
}

impl Module {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn create_fn(&mut self, name: String, params: Vec<Ty>, ret_ty: Ty) -> FunctionIdx {
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

    pub fn get_fn(&self, idx: FunctionIdx) -> &Function {
        &self.functions[idx]
    }

    pub fn get_fn_mut(&mut self, idx: FunctionIdx) -> &mut Function {
        &mut self.functions[idx]
    }
}
