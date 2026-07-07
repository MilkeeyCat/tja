use crate::{
    Amd64Instruction, DefaultInstruction,
    hir::{
        DisplayGlobalVariable, Function, FunctionIdx, GlobalVariable, GlobalVariableIdx, Signature,
        TargetInstruction, TyIdx, TyStorage, function::DisplayFunction,
    },
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
pub struct Module<TI: TargetInstruction> {
    pub(super) decls: Declarations,
    pub(super) funcs: HashMap<FunctionIdx, Function<TI>>,
    pub(super) global_vars: HashMap<GlobalVariableIdx, GlobalVariable>,
}

impl<TI: TargetInstruction> Module<TI> {
    pub fn new() -> Self {
        Self {
            decls: Declarations::default(),
            funcs: HashMap::new(),
            global_vars: HashMap::new(),
        }
    }

    pub fn display<'a>(&'a self, ty_storage: &'a TyStorage) -> DisplayModule<'a, TI> {
        DisplayModule {
            module: self,
            ty_storage: ty_storage,
        }
    }

    pub fn display_function<'a>(
        &'a self,
        ty_storage: &'a TyStorage,
        func: FunctionIdx,
    ) -> DisplayFunction<'a, TI> {
        DisplayFunction::new(self, ty_storage, func)
    }

    pub fn display_global_var<'a>(
        &'a self,
        ty_storage: &'a TyStorage,
        var: GlobalVariableIdx,
    ) -> DisplayGlobalVariable<'a, TI> {
        DisplayGlobalVariable {
            module: self,
            ty_storage,
            var,
        }
    }

    fn convert<TI2: TargetInstruction + From<TI>>(self) -> Module<TI2> {
        Module {
            decls: self.decls,
            funcs: self
                .funcs
                .into_iter()
                .map(|(idx, func)| (idx, func.convert()))
                .collect(),
            global_vars: self.global_vars,
        }
    }
}

macro_rules! impl_from_default_instr {
    ($($ty:ty),+ $(,)*) => {
        $(
            impl From<DefaultInstruction> for $ty {
                fn from(_instr: DefaultInstruction) -> Self {
                    unreachable!();
                }
            }

            impl From<Module<DefaultInstruction>> for Module<$ty> {
                fn from(module: Module<DefaultInstruction>) -> Self {
                    module.convert()
                }
            }
        )+
    };
}

impl_from_default_instr! {
    Amd64Instruction,
}

pub struct Builder<'a, TI: TargetInstruction> {
    module: &'a mut Module<TI>,
    ty_storage: &'a TyStorage,
}

impl<'a, TI: TargetInstruction> Builder<'a, TI> {
    pub fn new(module: &'a mut Module<TI>, ty_storage: &'a TyStorage) -> Self {
        Self { module, ty_storage }
    }

    pub fn declare_function(&mut self, name: String, sig: Signature) -> FunctionIdx {
        self.module.decls.declare_function(name, sig)
    }

    pub fn define_function(&mut self, func: FunctionIdx) {
        assert!(
            self.module
                .funcs
                .insert(func, Function::new(func))
                .is_none(),
            "function is already defined"
        );
    }

    pub fn declare_global_variable(&mut self, name: String, ty: TyIdx) -> GlobalVariableIdx {
        self.module.decls.declare_global_var(name, ty)
    }

    pub fn define_global_variable(&mut self, idx: GlobalVariableIdx, var: GlobalVariable) {
        if let GlobalVariable::Const(const_) = &var {
            if let Err(err) = const_.validate(self.module.decls.global_var(idx).ty, self.ty_storage)
            {
                panic!(
                    "{}",
                    err.display(const_, &self.module.decls, self.ty_storage)
                );
            }
        }

        assert!(
            self.module.global_vars.insert(idx, var).is_none(),
            "global variable is already defined"
        );
    }
}

pub struct DisplayModule<'a, TI: TargetInstruction> {
    module: &'a Module<TI>,
    ty_storage: &'a TyStorage,
}

impl<TI: TargetInstruction> Display for DisplayModule<'_, TI> {
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
