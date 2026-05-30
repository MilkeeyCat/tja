use crate::hir::{
    DisplayGlobalVariable, Function, FunctionIdx, GlobalVariable, GlobalVariableIdx, Signature,
    TyIdx, TyStorage, function::DisplayFunction,
};
use index_vec::IndexVec;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

pub(super) struct FunctionDeclaration {
    pub(super) name: String,
    pub(super) sig: Signature,
}

pub(super) struct GlobalVariableDeclaration {
    pub(super) name: String,
    pub(super) ty: TyIdx,
}

#[derive(Default)]
pub(super) struct Declarations {
    names: HashSet<String>,
    pub(super) funcs: IndexVec<FunctionIdx, FunctionDeclaration>,
    pub(super) global_vars: IndexVec<GlobalVariableIdx, GlobalVariableDeclaration>,
}

impl Declarations {
    fn declare_function(&mut self, name: String, sig: Signature) -> FunctionIdx {
        assert!(
            !self.names.contains(&name),
            "name '{}' is already declared",
            name,
        );

        let idx = self.funcs.push(FunctionDeclaration {
            name: name.clone(),
            sig,
        });

        self.names.insert(name);

        idx
    }

    fn declare_global_var(&mut self, name: String, ty: TyIdx) -> GlobalVariableIdx {
        assert!(
            !self.names.contains(&name),
            "name '{}' is already declared",
            name,
        );

        let idx = self.global_vars.push(GlobalVariableDeclaration {
            name: name.clone(),
            ty,
        });

        self.names.insert(name);

        idx
    }

    pub(super) fn function(&self, func: FunctionIdx) -> &FunctionDeclaration {
        &self.funcs[func]
    }

    pub(super) fn global_var(&self, var: GlobalVariableIdx) -> &GlobalVariableDeclaration {
        &self.global_vars[var]
    }
}

#[derive(Default)]
pub struct Module {
    pub(super) decls: Declarations,
    pub(super) funcs: HashMap<FunctionIdx, Function>,
    pub(super) global_vars: HashMap<GlobalVariableIdx, GlobalVariable>,
}

impl Module {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn declare_function(&mut self, name: String, sig: Signature) -> FunctionIdx {
        self.decls.declare_function(name, sig)
    }

    pub fn define_function(&mut self, func: FunctionIdx) {
        assert!(
            self.funcs.insert(func, Function::new(func)).is_none(),
            "function is already defined"
        );
    }

    pub fn declare_global_variable(&mut self, name: String, ty: TyIdx) -> GlobalVariableIdx {
        self.decls.declare_global_var(name, ty)
    }

    pub fn define_global_variable(&mut self, idx: GlobalVariableIdx, var: GlobalVariable) {
        assert!(
            self.global_vars.insert(idx, var).is_none(),
            "global variable is already defined"
        );
    }

    pub fn display<'a>(&'a self, ty_storage: &'a TyStorage) -> DisplayModule<'a> {
        DisplayModule {
            module: self,
            ty_storage: ty_storage,
        }
    }

    pub fn display_function<'a>(
        &'a self,
        ty_storage: &'a TyStorage,
        func: FunctionIdx,
    ) -> DisplayFunction<'a> {
        DisplayFunction::new(self, ty_storage, func)
    }

    pub fn display_global_var<'a>(
        &'a self,
        ty_storage: &'a TyStorage,
        var: GlobalVariableIdx,
    ) -> DisplayGlobalVariable<'a> {
        DisplayGlobalVariable {
            module: self,
            ty_storage,
            var,
        }
    }
}

pub struct DisplayModule<'a> {
    module: &'a Module,
    ty_storage: &'a TyStorage,
}

impl Display for DisplayModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for idx in self.module.decls.global_vars.indices() {
            writeln!(
                f,
                "{}",
                self.module.display_global_var(self.ty_storage, idx)
            )?;
        }

        if !self.module.decls.global_vars.is_empty() {
            writeln!(f)?;
        }

        let mut iter = self.module.decls.funcs.indices().peekable();

        while let Some(idx) = iter.next() {
            write!(f, "{}", self.module.display_function(self.ty_storage, idx))?;

            if iter.peek().is_some() {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}
