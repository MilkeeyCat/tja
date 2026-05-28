use crate::{FunctionIdx, GlobalVariableIdx, Immediate, lir::Ty};
use index_vec::IndexVec;
use std::collections::HashMap;

pub(crate) enum GlobalVariableValue {
    GlobalVariable(GlobalVariableIdx),
    Function(FunctionIdx),
    Imm(Ty, Immediate),
}

pub(crate) enum GlobalVariable {
    Zero(usize),
    Value(Vec<(usize, GlobalVariableValue)>),
}

pub(crate) struct GlobalVariableDeclaration {
    pub(crate) name: String,
}

#[derive(Default)]
pub(crate) struct Declarations {
    pub(crate) global_vars: IndexVec<GlobalVariableIdx, GlobalVariableDeclaration>,
}

#[derive(Default)]
pub(crate) struct Module {
    decls: Declarations,
    global_vars: HashMap<GlobalVariableIdx, GlobalVariable>,
}

impl Module {
    pub(crate) fn new(decls: Declarations) -> Self {
        Self {
            decls,
            ..Default::default()
        }
    }
}

pub(crate) struct Builder<'a>(&'a mut Module);

impl<'a> Builder<'a> {
    pub(crate) fn new(module: &'a mut Module) -> Self {
        Self(module)
    }

    pub(crate) fn define_global_var(&mut self, idx: GlobalVariableIdx, var: GlobalVariable) {
        self.0.global_vars.insert(idx, var);
    }
}
