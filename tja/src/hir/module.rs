use crate::hir::{Function, FunctionIdx, Signature, TyIdx};
use index_vec::{IndexVec, define_index_type};
use std::collections::HashMap;
use thiserror::Error;

pub struct Immediate(i64);

pub enum Constant {
    Global(GlobalIdx),
    Function(FunctionIdx),
    Imm(Immediate),
    Aggregate(Vec<Self>),
}

struct FunctionDeclaration {
    name: String,
    sig: Signature,
}

struct GlobalDeclaration {
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

        self.funcs.insert(func, Function::default());

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
}
