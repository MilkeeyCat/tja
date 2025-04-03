pub mod basic_block;
mod function;
mod module;
pub mod op;
pub mod ty;

pub use basic_block::{BasicBlock, BlockIdx};
pub use derive_more::From;
pub use function::{Function, FunctionIdx, Patch};
pub use module::Module;
use module::ModuleIdx;
use op::{BinOp, CmpOp};
use std::{
    collections::HashSet,
    ops::{Deref, DerefMut},
    rc::Rc,
};
use ty::TyIdx;

pub type LocalIdx = usize;
pub type InstructionIdx = usize;

#[derive(Debug, Clone, From, PartialEq, Eq, Hash)]
pub enum Const {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    #[from(ignore)]
    Aggregate(Vec<Self>),
}

impl Const {
    pub fn usize_unchecked(&self) -> usize {
        match self {
            Self::I8(value) => (*value).try_into().unwrap(),
            Self::U8(value) => (*value).try_into().unwrap(),
            Self::I16(value) => (*value).try_into().unwrap(),
            Self::U16(value) => (*value).try_into().unwrap(),
            Self::I32(value) => (*value).try_into().unwrap(),
            Self::U32(value) => (*value).try_into().unwrap(),
            Self::I64(value) => (*value).try_into().unwrap(),
            Self::U64(value) => (*value).try_into().unwrap(),
            Self::Aggregate(_) => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Global(Rc<Global>),
    Local(LocalIdx),
    Const(Const, TyIdx),
}

macro_rules! int_const_impl {
    ($([$($ty: ident),+] => $field: ident),+) => {
        $(
            $(
                paste::item! {
                    pub fn [< const_ $ty >](value: $ty, storage: &ty::Storage) -> Self {
                        Self::Const(Const::from(value), storage.[< $field _ty >])
                    }
                }
            )+
        )+
    };
}

impl Operand {
    pub fn local_idx(&self) -> Option<LocalIdx> {
        match self {
            Self::Local(local_idx) => Some(*local_idx),
            Self::Global(_) | Self::Const(_, _) => None,
        }
    }

    pub fn ty<T: LocalStorage>(&self, storage: &T) -> TyIdx {
        match self {
            Self::Global(global) => global.ty,
            Self::Local(idx) => storage.get_local_ty(*idx),
            Self::Const(_, ty) => *ty,
        }
    }

    int_const_impl! {
        [u8, i8] => i8,
        [u16, i16] => i16,
        [u32, i32] => i32,
        [u64, i64] => i64
    }
}

pub trait LocalStorage {
    fn get_local_ty(&self, idx: LocalIdx) -> TyIdx;
}

#[derive(Debug, Clone)]
pub enum Branch {
    Conditional {
        condition: Operand,
        iftrue: BlockIdx,
        iffalse: BlockIdx,
    },
    Unconditional {
        block_idx: BlockIdx,
    },
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Binary {
        kind: BinOp,
        lhs: Operand,
        rhs: Operand,
        out: LocalIdx,
    },
    Sext {
        operand: Operand,
        out: LocalIdx,
    },
    Zext {
        operand: Operand,
        out: LocalIdx,
    },
    Copy {
        operand: Operand,
        out: LocalIdx,
    },
    Alloca {
        ty: TyIdx,
        out: LocalIdx,
    },
    Store {
        ptr: Operand,
        value: Operand,
    },
    Load {
        ptr: Operand,
        out: LocalIdx,
    },
    GetElementPtr {
        ptr: Operand,
        ptr_ty: TyIdx,
        indices: Vec<Operand>,
        out: LocalIdx,
    },
    Icmp {
        kind: CmpOp,
        lhs: Operand,
        rhs: Operand,
        out: LocalIdx,
    },
}

impl Instruction {
    pub fn def(&self) -> Option<LocalIdx> {
        match self {
            Self::Binary { out, .. } => Some(*out),
            Self::Copy { out, .. } => Some(*out),
            Self::Sext { out, .. } => Some(*out),
            Self::Zext { out, .. } => Some(*out),
            Self::Alloca { out, .. } => Some(*out),
            Self::Store { .. } => None,
            Self::Load { out, .. } => Some(*out),
            Self::GetElementPtr { out, .. } => Some(*out),
            Self::Icmp { out, .. } => Some(*out),
        }
    }

    pub fn uses(&self) -> HashSet<LocalIdx> {
        match self {
            Self::Binary { lhs, rhs, .. } => vec![lhs.local_idx(), rhs.local_idx()],
            Self::Copy { operand, .. } => vec![operand.local_idx()],
            Self::Sext { operand, .. } => vec![operand.local_idx()],
            Self::Zext { operand, .. } => vec![operand.local_idx()],
            Self::Alloca { .. } => Vec::new(),
            Self::Store { ptr, value } => vec![ptr.local_idx(), value.local_idx()],
            Self::Load { ptr, .. } => vec![ptr.local_idx()],
            Self::GetElementPtr { indices, ptr, .. } => {
                let mut uses: Vec<_> = indices.iter().map(|operand| operand.local_idx()).collect();

                uses.push(ptr.local_idx());

                uses
            }
            Self::Icmp { lhs, rhs, .. } => vec![lhs.local_idx(), rhs.local_idx()],
        }
        .into_iter()
        .flatten()
        .collect()
    }
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return(Option<Operand>),
    Br(Branch),
}

impl Terminator {
    pub fn uses(&self) -> HashSet<LocalIdx> {
        match self {
            Self::Return(Some(Operand::Local(idx))) => HashSet::from([*idx]),
            Self::Return(_) => HashSet::new(),
            Self::Br(Branch::Conditional {
                condition: Operand::Local(idx),
                ..
            }) => HashSet::from([*idx]),
            Self::Br(_) => HashSet::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Global {
    pub name: String,
    pub ty: TyIdx,
    pub value: Const,
}

pub struct Context {
    pub ty_storage: ty::Storage,
    modules: Vec<Module>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            ty_storage: ty::Storage::new(),
            modules: Vec::new(),
        }
    }

    pub fn create_module(&mut self, name: String) -> ModuleIdx {
        let idx = self.modules.len();
        self.modules.push(Module::new(name));

        idx
    }

    pub fn get_module(&mut self, idx: ModuleIdx) -> Wrapper<&mut Module> {
        Wrapper {
            ty_storage: &mut self.ty_storage,
            inner: &mut self.modules[idx],
        }
    }
}

/// [`Wrapper`] struct is used to wrap [`Module`], [`Function`] to have access
/// to [`Context::ty_storage`]
pub struct Wrapper<'ctx, T> {
    pub ty_storage: &'ctx mut ty::Storage,
    inner: T,
}

impl<T> Deref for Wrapper<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Wrapper<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
