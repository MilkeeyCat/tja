use crate::hir::{
    DisplayGlobal, Function, FunctionIdx, Global, GlobalIdx, Signature, TyIdx, TyStorage,
    function::DisplayFunction,
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

pub(super) struct GlobalDeclaration {
    pub(super) name: String,
    pub(super) ty: TyIdx,
}

#[derive(Default)]
pub(super) struct Declarations {
    names: HashSet<String>,
    funcs: IndexVec<FunctionIdx, FunctionDeclaration>,
    globals: IndexVec<GlobalIdx, GlobalDeclaration>,
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

    fn declare_global(&mut self, name: String, ty: TyIdx) -> GlobalIdx {
        assert!(
            !self.names.contains(&name),
            "name '{}' is already declared",
            name,
        );

        let idx = self.globals.push(GlobalDeclaration {
            name: name.clone(),
            ty,
        });

        self.names.insert(name);

        idx
    }

    pub(super) fn function(&self, func: FunctionIdx) -> &FunctionDeclaration {
        &self.funcs[func]
    }

    pub(super) fn global(&self, global: GlobalIdx) -> &GlobalDeclaration {
        &self.globals[global]
    }
}

#[derive(Default)]
pub struct Module {
    pub(super) decls: Declarations,
    pub(super) funcs: HashMap<FunctionIdx, Function>,
    pub(super) globals: HashMap<GlobalIdx, Global>,
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

    pub fn declare_global(&mut self, name: String, ty: TyIdx) -> GlobalIdx {
        self.decls.declare_global(name, ty)
    }

    pub fn define_global(&mut self, idx: GlobalIdx, global: Global) {
        assert!(
            self.globals.insert(idx, global).is_none(),
            "global is already defined"
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

    pub fn display_global<'a>(
        &'a self,
        ty_storage: &'a TyStorage,
        global: GlobalIdx,
    ) -> DisplayGlobal<'a> {
        DisplayGlobal {
            module: self,
            ty_storage,
            global,
        }
    }
}

pub struct DisplayModule<'a> {
    module: &'a Module,
    ty_storage: &'a TyStorage,
}

impl Display for DisplayModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for idx in self.module.decls.globals.indices() {
            writeln!(f, "{}", self.module.display_global(self.ty_storage, idx))?;
        }

        if !self.module.decls.globals.is_empty() {
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
