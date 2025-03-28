use crate::repr::ty::{self, Ty, TyIdx};

pub struct Abi;

impl Abi {
    pub fn field_offset(storage: &ty::Storage, fields: &[TyIdx], i: usize) -> usize {
        fields
            .iter()
            .take(i)
            .fold(0usize, |offset, ty| {
                offset.next_multiple_of(Self::alignment(storage, *ty)) + Self::ty_size(storage, *ty)
            })
            .next_multiple_of(Self::alignment(storage, fields[i]))
    }

    pub fn ty_size(storage: &ty::Storage, ty: TyIdx) -> usize {
        match storage.get_ty(ty) {
            Ty::Struct(fields) => {
                if fields.is_empty() {
                    0
                } else {
                    let last_field = fields.len() - 1;
                    (Self::field_offset(storage, fields, last_field)
                        + Self::ty_size(storage, fields[last_field]))
                    .next_multiple_of(Self::alignment(storage, ty))
                }
            }
            _ => storage.get_ty(ty).size(),
        }
    }

    pub fn alignment(storage: &ty::Storage, ty: TyIdx) -> usize {
        match storage.get_ty(ty) {
            Ty::Struct(fields) => fields
                .iter()
                .map(|ty| Self::alignment(storage, *ty))
                .max()
                .unwrap_or_default(),
            _ => Self::ty_size(storage, ty),
        }
    }
}
