use index_vec::{IndexVec, define_index_type, index_vec};

define_index_type! {
    pub struct TyIdx = usize;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Void,
    I8,
    I16,
    I32,
    I64,
    Ptr,
    Struct(Vec<TyIdx>),
    Array { ty: TyIdx, len: usize },
}

impl Ty {
    pub fn size(&self) -> usize {
        match self {
            Self::Void => 0,
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 | Self::Ptr => 8,
            // aggregate types size is determined by abi
            Self::Struct(_) | Self::Array { .. } => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct Storage {
    pub void_ty: TyIdx,
    pub i8_ty: TyIdx,
    pub i16_ty: TyIdx,
    pub i32_ty: TyIdx,
    pub i64_ty: TyIdx,
    pub ptr_ty: TyIdx,
    types: IndexVec<TyIdx, Ty>,
}

impl Storage {
    pub fn new() -> Self {
        Self {
            void_ty: 0.into(),
            i8_ty: 1.into(),
            i16_ty: 2.into(),
            i32_ty: 3.into(),
            i64_ty: 4.into(),
            ptr_ty: 5.into(),
            types: index_vec![Ty::Void, Ty::I8, Ty::I16, Ty::I32, Ty::I64, Ty::Ptr],
        }
    }

    pub fn add_ty(&mut self, ty: Ty) -> TyIdx {
        if let Some((idx, _)) = self.types.iter().enumerate().find(|&(_, pred)| pred == &ty) {
            idx.into()
        } else {
            self.types.push(ty)
        }
    }

    pub fn get_ty(&self, idx: TyIdx) -> &Ty {
        &self.types[idx]
    }
}
