pub type TyIdx = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Void,
    I8,
    I16,
    I32,
    I64,
    Ptr,
    Struct(Vec<TyIdx>),
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
            Self::Struct(_) => unreachable!(),
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
    types: Vec<Ty>,
}

impl Storage {
    pub fn new() -> Self {
        Self {
            void_ty: 0,
            i8_ty: 1,
            i16_ty: 2,
            i32_ty: 3,
            i64_ty: 4,
            ptr_ty: 5,
            types: vec![Ty::Void, Ty::I8, Ty::I16, Ty::I32, Ty::I64, Ty::Ptr],
        }
    }

    pub fn add_ty(&mut self, ty: Ty) -> TyIdx {
        if let Some((idx, _)) = self.types.iter().enumerate().find(|&(_, pred)| pred == &ty) {
            idx
        } else {
            let idx = self.types.len();
            self.types.push(ty);

            idx
        }
    }

    pub fn get_ty(&self, idx: TyIdx) -> &Ty {
        &self.types[idx]
    }
}
