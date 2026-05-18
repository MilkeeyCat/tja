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
