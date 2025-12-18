pub mod basic_block;
mod function;
mod module;
pub mod op;
pub mod pass;
pub mod passes;

use crate::{
    ConditionCode, Const, GlobalIdx,
    ty::{self, TyIdx},
};
pub use basic_block::{BasicBlock, BlockIdx, Builder as BasicBlockBuilder};
pub use derive_more::From;
pub use function::Function;
use index_vec::define_index_type;
pub use module::{Module, ModuleIdx};
use op::BinOp;
use std::collections::HashSet;

define_index_type! {
    pub struct LocalIdx = usize;
}

define_index_type! {
    pub struct InstructionIdx = usize;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum Operand {
    Local(LocalIdx),
    #[from(ignore)]
    Const(Const, TyIdx),
}

macro_rules! int_const_impl {
    ($($bits: literal),+) => {
        $(
            paste::item! {
                pub fn [< const_i $bits >](value: [< u $bits >], storage: &ty::Storage) -> Self {
                    Self::Const(Const::Int(value as u64), storage.[< i $bits _ty >])
                }
            }
        )+
    };
}

impl Operand {
    pub fn local_idx(&self) -> Option<LocalIdx> {
        match self {
            Self::Local(local_idx) => Some(*local_idx),
            Self::Const(_, _) => None,
        }
    }

    pub fn ty<T: LocalStorage>(&self, storage: &T) -> TyIdx {
        match self {
            Self::Local(idx) => storage.get_local_ty(*idx),
            Self::Const(_, ty) => *ty,
        }
    }

    int_const_impl! { 8, 16, 32, 64 }

    pub fn const_int(value: u64, ty: TyIdx) -> Self {
        Self::Const(Const::Int(value), ty)
    }

    pub fn const_global(idx: GlobalIdx, storage: &ty::Storage) -> Self {
        Self::Const(Const::Global(idx), storage.ptr_ty)
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
        cond_code: ConditionCode,
        lhs: Operand,
        rhs: Operand,
        out: LocalIdx,
    },
    Call {
        operand: Operand,
        arguments: Vec<Operand>,
        out: Option<LocalIdx>,
    },
}

impl Instruction {
    pub fn def(&self) -> Option<LocalIdx> {
        match self {
            Self::Binary { out, .. } => Some(*out),
            Self::Sext { out, .. } => Some(*out),
            Self::Zext { out, .. } => Some(*out),
            Self::Alloca { out, .. } => Some(*out),
            Self::Store { .. } => None,
            Self::Load { out, .. } => Some(*out),
            Self::GetElementPtr { out, .. } => Some(*out),
            Self::Icmp { out, .. } => Some(*out),
            Self::Call { out, .. } => out.clone(),
        }
    }

    pub fn uses(&self) -> HashSet<LocalIdx> {
        match self {
            Self::Binary { lhs, rhs, .. } => vec![lhs.local_idx(), rhs.local_idx()],
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
            Self::Call { arguments, .. } => arguments
                .iter()
                .map(|operand| operand.local_idx())
                .collect(),
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
