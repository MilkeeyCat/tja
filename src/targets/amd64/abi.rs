use crate::{
    hir::ty::{self, Ty, TyIdx},
    targets::{Abi, amd64::sysv_calling_convention},
};

pub struct SysVAmd64 {
    default_cc: sysv_calling_convention::SysVAmd64,
}

impl SysVAmd64 {
    pub fn new() -> Self {
        Self {
            default_cc: sysv_calling_convention::SysVAmd64::new(),
        }
    }
}

impl Abi for SysVAmd64 {
    type CallingConvention = sysv_calling_convention::SysVAmd64;

    fn field_offset(&self, storage: &ty::Storage, fields: &[TyIdx], i: usize) -> usize {
        fields
            .iter()
            .take(i)
            .fold(0usize, |offset, ty| {
                offset.next_multiple_of(self.alignment(storage, *ty)) + self.ty_size(storage, *ty)
            })
            .next_multiple_of(self.alignment(storage, fields[i]))
    }

    fn ty_size(&self, storage: &ty::Storage, ty: TyIdx) -> usize {
        match storage.get_ty(ty) {
            Ty::Struct(fields) => {
                if fields.is_empty() {
                    0
                } else {
                    let last_field = fields.len() - 1;
                    (self.field_offset(storage, fields, last_field)
                        + self.ty_size(storage, fields[last_field]))
                    .next_multiple_of(self.alignment(storage, ty))
                }
            }
            Ty::Array { ty, len } => self.ty_size(storage, *ty) * len,
            _ => storage.get_ty(ty).size(),
        }
    }

    fn alignment(&self, storage: &ty::Storage, ty: TyIdx) -> usize {
        match storage.get_ty(ty) {
            Ty::Struct(fields) => fields
                .iter()
                .map(|ty| self.alignment(storage, *ty))
                .max()
                .unwrap_or_default(),
            _ => self.ty_size(storage, ty),
        }
    }

    fn calling_convention(&self) -> &Self::CallingConvention {
        &self.default_cc
    }
}
