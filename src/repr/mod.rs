mod basic_block;
mod function;
mod module;
pub mod op;
pub mod ty;

pub use basic_block::{BasicBlock, Builder};
pub use function::Function;
pub use module::Module;
use op::BinOp;
use std::{collections::HashSet, rc::Rc};
use ty::Ty;

pub type LocalIdx = usize;
pub type BlockIdx = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Aggregate(Vec<Self>),
}

impl Const {
    pub fn ty(&self) -> Ty {
        match self {
            Self::I8(_) | Self::U8(_) => Ty::I8,
            Self::I16(_) | Self::U16(_) => Ty::I16,
            Self::I32(_) | Self::U32(_) => Ty::I32,
            Self::I64(_) | Self::U64(_) => Ty::I64,
            Self::Aggregate(values) => Ty::Struct(values.iter().map(|c| c.ty()).collect()),
        }
    }

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

#[derive(Debug)]
pub enum Operand {
    Global(Rc<Global>),
    Local(LocalIdx),
    Const(Const),
}

impl Operand {
    pub fn local_idx(&self) -> Option<LocalIdx> {
        match self {
            Self::Local(local_idx) => Some(*local_idx),
            Self::Global(_) | Self::Const(_) => None,
        }
    }

    pub fn ty<T: LocalStorage>(&self, storage: &T) -> Ty {
        match self {
            Self::Global(global) => global.ty.clone(),
            Self::Local(idx) => storage.get_local_ty(*idx).clone(),
            Self::Const(c) => c.ty(),
        }
    }
}

pub trait LocalStorage {
    fn get_local_ty(&self, idx: LocalIdx) -> &Ty;
}

#[derive(Debug)]
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
        ty: Ty,
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
        ptr_ty: Ty,
        indices: Vec<Operand>,
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
        }
        .into_iter()
        .flatten()
        .collect()
    }
}

#[derive(Debug)]
pub enum Terminator {
    Goto(BlockIdx),
    Return(Option<Operand>),
}

impl Terminator {
    pub fn uses(&self) -> HashSet<LocalIdx> {
        match self {
            Self::Return(Some(Operand::Local(idx))) => HashSet::from([*idx]),
            Self::Return(_) | Self::Goto(_) => HashSet::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Global {
    pub name: String,
    pub ty: Ty,
    pub value: Const,
}
