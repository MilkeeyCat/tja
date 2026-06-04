use crate::{
    FunctionIdx, GlobalVariableIdx,
    lir::{DisplayGlobalVariable, Function, GlobalVariable, Signature, function::DisplayFunction},
};
use index_vec::IndexVec;
use std::{collections::HashMap, fmt::Display};

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
    pub(super) global_vars: HashMap<GlobalVariableIdx, GlobalVariable>,
    pub(super) funcs: HashMap<FunctionIdx, Function>,
}

impl Module {
    pub(crate) fn new(decls: Declarations) -> Self {
        Self {
            decls,
            ..Default::default()
        }
    }

    #[allow(
        unused,
        reason = "not publicly exported but useful for debugging purposes"
    )]
    pub(crate) fn display<'a>(&'a self) -> DisplayModule<'a> {
        DisplayModule(self)
    }

    pub(crate) fn display_function<'a>(&'a self, func: FunctionIdx) -> DisplayFunction<'a> {
        DisplayFunction::new(self, func)
    }

    pub(crate) fn display_global_var<'a>(
        &'a self,
        var: GlobalVariableIdx,
    ) -> DisplayGlobalVariable<'a> {
        DisplayGlobalVariable { module: self, var }
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

pub(crate) struct DisplayModule<'a>(&'a Module);

impl Display for DisplayModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for idx in self.0.decls.global_vars.indices() {
            write!(f, "{}", self.0.display_global_var(idx))?;
        }

        if !self.0.decls.global_vars.is_empty() {
            writeln!(f)?;
        }

        let mut iter = self.0.decls.funcs.indices().peekable();

        while let Some(idx) = iter.next() {
            write!(f, "{}", self.0.display_function(idx))?;

            if iter.peek().is_some() {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}
