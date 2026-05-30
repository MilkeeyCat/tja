use crate::{
    FunctionIdx, GlobalVariableIdx, Immediate,
    lir::{Function, Ty, signature::Signature},
};
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

pub(crate) struct FunctionDeclaration {
    pub(crate) name: String,
    pub(crate) sig: Signature,
}

#[derive(Default)]
pub(crate) struct Declarations {
    pub(crate) funcs: IndexVec<FunctionIdx, FunctionDeclaration>,
    pub(crate) global_vars: IndexVec<GlobalVariableIdx, GlobalVariableDeclaration>,
}

#[derive(Default)]
pub(crate) struct Module {
    pub(super) decls: Declarations,
    global_vars: HashMap<GlobalVariableIdx, GlobalVariable>,
    pub(super) funcs: HashMap<FunctionIdx, Function>,
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

    pub(crate) fn define_function(&mut self, func: FunctionIdx) {
        self.0.funcs.insert(func, Function::new(func));
    }
}
