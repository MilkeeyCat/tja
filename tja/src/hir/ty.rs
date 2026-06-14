use crate::{lir, mir::Abi};
use index_vec::{IndexVec, define_index_type};
use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Display,
};

define_index_type! {
    pub struct TyIdx = usize;
}

impl TyIdx {
    pub fn display<'a>(&self, ty_storage: &'a Storage) -> DisplayTy<'a> {
        DisplayTy {
            ty_storage,
            ty: *self,
        }
    }

    pub(crate) fn lir_ty_iter<'a>(&self, ty_storage: &'a Storage) -> LirTyIter<'a> {
        LirTyIter::new(*self, ty_storage)
    }

    pub(crate) fn offset_iter<'a>(
        &self,
        ty_storage: &'a Storage,
        abi: &'a dyn Abi,
    ) -> OffsetIter<'a> {
        OffsetIter::new(*self, ty_storage, abi)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    I8,
    I16,
    I32,
    I64,
    Ptr,
    Struct(Vec<TyIdx>),
    Array { ty: TyIdx, len: usize },
}

pub struct DisplayTy<'a> {
    ty_storage: &'a Storage,
    ty: TyIdx,
}

impl Display for DisplayTy<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty_storage.get(self.ty) {
            Ty::I8 => write!(f, "i8"),
            Ty::I16 => write!(f, "i16"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "i64"),
            Ty::Ptr => write!(f, "ptr"),
            Ty::Struct(tys) => {
                write!(f, "{{")?;

                let mut iter = tys.iter().peekable();

                while let Some(ty) = iter.next() {
                    write!(f, "{}", ty.display(self.ty_storage))?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "}}")?;

                Ok(())
            }
            Ty::Array { ty, len } => {
                write!(f, "[{} x {}]", ty.display(self.ty_storage), len)
            }
        }
    }
}

pub struct Storage {
    types: IndexVec<TyIdx, Ty>,
    types_map: HashMap<Ty, TyIdx>,

    pub i8_ty: TyIdx,
    pub i16_ty: TyIdx,
    pub i32_ty: TyIdx,
    pub i64_ty: TyIdx,
    pub ptr_ty: TyIdx,
}

impl Storage {
    pub fn new() -> Self {
        let simple_tys = [Ty::I8, Ty::I16, Ty::I32, Ty::I64, Ty::Ptr];
        let types = IndexVec::from_iter(simple_tys.clone());
        let types_map: HashMap<Ty, TyIdx> =
            HashMap::from_iter(simple_tys.into_iter().zip(types.indices()));
        let [i8_ty, i16_ty, i32_ty, i64_ty, ptr_ty] =
            types.indices().collect::<Vec<_>>().try_into().unwrap();

        Self {
            types,
            types_map,

            i8_ty,
            i16_ty,
            i32_ty,
            i64_ty,
            ptr_ty,
        }
    }

    pub fn add(&mut self, ty: Ty) -> TyIdx {
        match self.types_map.entry(ty.clone()) {
            Entry::Vacant(entry) => {
                let idx = self.types.push(ty);

                entry.insert(idx);

                idx
            }
            Entry::Occupied(entry) => *entry.get(),
        }
    }

    pub fn get(&self, idx: TyIdx) -> &Ty {
        &self.types[idx]
    }
}

enum LirTyIterNode<'a> {
    Struct {
        fields: &'a [TyIdx],
        field_idx: usize,
    },
    Array {
        ty: TyIdx,
        len: usize,
        cur_len: usize,
    },
    Scalar(lir::Ty),
}

pub(crate) struct LirTyIter<'a> {
    ty_storage: &'a Storage,
    stack: Vec<LirTyIterNode<'a>>,
}

impl<'a> LirTyIter<'a> {
    fn new(ty: TyIdx, ty_storage: &'a Storage) -> Self {
        let mut iter = Self {
            ty_storage,
            stack: Vec::new(),
        };

        iter.push(ty);

        iter
    }

    fn push(&mut self, ty: TyIdx) {
        match self.ty_storage.get(ty) {
            Ty::I8 => self.stack.push(LirTyIterNode::Scalar(lir::Ty::I8)),
            Ty::I16 => self.stack.push(LirTyIterNode::Scalar(lir::Ty::I16)),
            Ty::I32 => self.stack.push(LirTyIterNode::Scalar(lir::Ty::I32)),
            Ty::I64 => self.stack.push(LirTyIterNode::Scalar(lir::Ty::I64)),
            Ty::Ptr => self.stack.push(LirTyIterNode::Scalar(lir::Ty::PTR)),
            Ty::Struct(fields) => self.stack.push(LirTyIterNode::Struct {
                fields,
                field_idx: 0,
            }),
            &Ty::Array { ty, len } => self.stack.push(LirTyIterNode::Array {
                ty,
                len,
                cur_len: 0,
            }),
        }
    }
}

impl Iterator for LirTyIter<'_> {
    type Item = lir::Ty;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node) = self.stack.pop() {
            match node {
                LirTyIterNode::Struct { fields, field_idx } => {
                    if field_idx == fields.len() {
                        continue;
                    }

                    self.stack.push(LirTyIterNode::Struct {
                        fields,
                        field_idx: field_idx + 1,
                    });
                    self.push(fields[field_idx]);
                }
                LirTyIterNode::Array { ty, len, cur_len } => {
                    if cur_len == len {
                        continue;
                    }

                    self.stack.push(LirTyIterNode::Array {
                        ty,
                        len,
                        cur_len: cur_len + 1,
                    });
                    self.push(ty);
                }
                LirTyIterNode::Scalar(ty) => return Some(ty),
            }
        }

        None
    }
}

enum OffsetIterNode<'a> {
    Struct {
        fields: &'a [TyIdx],
        field_idx: usize,
        base_offset: usize,
    },
    Array {
        ty: TyIdx,
        len: usize,
        cur_len: usize,
        base_offset: usize,
    },
    Scalar(usize),
}

pub(crate) struct OffsetIter<'a> {
    ty_storage: &'a Storage,
    abi: &'a dyn Abi,
    stack: Vec<OffsetIterNode<'a>>,
}

impl<'a> OffsetIter<'a> {
    fn new(ty: TyIdx, ty_storage: &'a Storage, abi: &'a dyn Abi) -> Self {
        let mut iter = Self {
            ty_storage,
            abi,
            stack: Vec::new(),
        };

        iter.push(ty, 0);

        iter
    }

    fn push(&mut self, ty: TyIdx, offset: usize) {
        match self.ty_storage.get(ty) {
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::Ptr => {
                self.stack.push(OffsetIterNode::Scalar(offset))
            }
            Ty::Struct(fields) => self.stack.push(OffsetIterNode::Struct {
                fields,
                field_idx: 0,
                base_offset: offset,
            }),
            &Ty::Array { ty, len } => self.stack.push(OffsetIterNode::Array {
                ty,
                len,
                cur_len: 0,
                base_offset: offset,
            }),
        }
    }
}

impl Iterator for OffsetIter<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(node) = self.stack.pop() {
            match node {
                OffsetIterNode::Struct {
                    fields,
                    field_idx,
                    base_offset,
                } => {
                    if field_idx == fields.len() {
                        continue;
                    }

                    self.stack.push(OffsetIterNode::Struct {
                        fields,
                        field_idx: field_idx + 1,
                        base_offset,
                    });
                    self.push(
                        fields[field_idx],
                        base_offset + self.abi.field_offset(self.ty_storage, fields, field_idx),
                    );
                }
                OffsetIterNode::Array {
                    ty,
                    len,
                    cur_len,
                    base_offset,
                } => {
                    if cur_len == len {
                        continue;
                    }

                    self.stack.push(OffsetIterNode::Array {
                        ty,
                        len,
                        cur_len: cur_len + 1,
                        base_offset,
                    });
                    self.push(
                        ty,
                        base_offset + self.abi.hir_ty_size(self.ty_storage, ty) * cur_len,
                    );
                }
                OffsetIterNode::Scalar(offset) => return Some(offset),
            }
        }

        None
    }
}
