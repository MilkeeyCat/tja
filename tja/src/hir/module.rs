use crate::hir::{Function, FunctionIdx, Signature, TyIdx, TyStorage, function::DisplayFunction};
use derive_more::From;
use index_vec::{IndexVec, define_index_type};
use std::{collections::HashMap, fmt::Display};
use thiserror::Error;

#[derive(From)]
#[from(u8, i8, u16, i16, u32, i32, i64)]
pub struct Immediate(i64);

#[derive(From)]
pub enum Constant {
    Global(GlobalIdx),
    Function(FunctionIdx),
    Imm(Immediate),
    Aggregate(Vec<Self>),
}

impl Constant {
    pub fn display<'a>(&'a self, decls: &'a Declarations) -> DisplayConstant<'a> {
        DisplayConstant {
            decls,
            const_: self,
        }
    }
}

pub struct DisplayConstant<'a> {
    decls: &'a Declarations,
    const_: &'a Constant,
}

impl Display for DisplayConstant<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.const_ {
            Constant::Global(idx) => write!(f, "{}", self.decls.global(*idx).name),
            Constant::Function(idx) => write!(f, "{}", self.decls.function(*idx).name),
            Constant::Imm(imm) => write!(f, "{}", imm.0),
            Constant::Aggregate(consts) => {
                write!(f, "{{")?;

                let mut iter = consts.iter().peekable();

                while let Some(const_) = iter.next() {
                    write!(f, "{}", const_.display(self.decls))?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "}}")?;

                Ok(())
            }
        }
    }
}

pub(super) struct FunctionDeclaration {
    pub(super) name: String,
    pub(super) sig: Signature,
}

pub(super) struct GlobalDeclaration {
    name: String,
    ty: TyIdx,
}

define_index_type! {
    pub struct GlobalIdx = usize;
}

pub enum Global {
    Zero,
    Const(Constant),
}

pub struct DisplayGlobal<'a> {
    module: &'a Module,
    ty_storage: &'a TyStorage,
    global: GlobalIdx,
}

impl Display for DisplayGlobal<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decl = self.module.decls.global(self.global);
        let global = self.module.globals.get(&self.global);

        write!(f, "{} = ", decl.name)?;

        if global.is_none() {
            write!(f, "external ")?;
        }

        write!(f, "global {}", decl.ty.display(self.ty_storage))?;

        if let Some(global) = global {
            write!(f, ", ")?;

            match global {
                Global::Zero => write!(f, "zero")?,
                Global::Const(const_) => write!(f, "{}", const_.display(&self.module.decls))?,
            };
        }

        Ok(())
    }
}

enum FunctionOrGlobalIdx {
    Function(FunctionIdx),
    Global(GlobalIdx),
}

#[derive(Debug, Error)]
#[error("declaration already exists")]
pub struct RedeclarationError;

#[derive(Default)]
pub struct Declarations {
    names: HashMap<String, FunctionOrGlobalIdx>,
    funcs: IndexVec<FunctionIdx, FunctionDeclaration>,
    globals: IndexVec<GlobalIdx, GlobalDeclaration>,
}

impl Declarations {
    pub fn declare_function(
        &mut self,
        name: String,
        sig: Signature,
    ) -> Result<FunctionIdx, RedeclarationError> {
        if self.names.contains_key(&name) {
            return Err(RedeclarationError);
        }

        let idx = self.funcs.push(FunctionDeclaration {
            name: name.clone(),
            sig,
        });

        self.names.insert(name, FunctionOrGlobalIdx::Function(idx));

        Ok(idx)
    }

    pub(super) fn function(&self, func: FunctionIdx) -> &FunctionDeclaration {
        &self.funcs[func]
    }

    pub(super) fn global(&self, global: GlobalIdx) -> &GlobalDeclaration {
        &self.globals[global]
    }

    pub fn declare_global(
        &mut self,
        name: String,
        ty: TyIdx,
    ) -> Result<GlobalIdx, RedeclarationError> {
        if self.names.contains_key(&name) {
            return Err(RedeclarationError);
        }

        let idx = self.globals.push(GlobalDeclaration {
            name: name.clone(),
            ty,
        });

        self.names.insert(name, FunctionOrGlobalIdx::Global(idx));

        Ok(idx)
    }
}

#[derive(Debug, Error)]
#[error("function is already defined")]
pub struct FunctionRedefinitionError;

#[derive(Debug, Error)]
#[error("global is already defined")]
pub struct GlobalRedefinitionError;

#[derive(Default)]
pub struct Module {
    pub(super) decls: Declarations,
    pub(super) funcs: HashMap<FunctionIdx, Function>,
    globals: HashMap<GlobalIdx, Global>,
}

impl Module {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn declare_function(
        &mut self,
        name: String,
        sig: Signature,
    ) -> Result<FunctionIdx, RedeclarationError> {
        self.decls.declare_function(name, sig)
    }

    pub fn define_function(&mut self, func: FunctionIdx) -> Result<(), FunctionRedefinitionError> {
        if self.funcs.contains_key(&func) {
            return Err(FunctionRedefinitionError);
        }

        self.funcs.insert(func, Function::new(func));

        Ok(())
    }

    pub fn declare_global(
        &mut self,
        name: String,
        ty: TyIdx,
    ) -> Result<GlobalIdx, RedeclarationError> {
        self.decls.declare_global(name, ty)
    }

    pub fn define_global(
        &mut self,
        idx: GlobalIdx,
        global: Global,
    ) -> Result<(), GlobalRedefinitionError> {
        if self.globals.contains_key(&idx) {
            return Err(GlobalRedefinitionError);
        }

        self.globals.insert(idx, global);

        Ok(())
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
