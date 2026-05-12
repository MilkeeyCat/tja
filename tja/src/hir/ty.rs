use index_vec::{IndexVec, define_index_type};
use std::collections::{HashMap, hash_map::Entry};

define_index_type! {
    pub struct TyIdx = usize;
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
