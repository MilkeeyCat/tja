use crate::{
    mir::PhysicalRegister,
    targets::{
        Abi,
        amd64::{Register, sysv_calling_convention},
    },
    ty::{self, Ty, TyIdx},
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

    fn callee_saved_regs(&self) -> &'static [PhysicalRegister] {
        const {
            &[
                Register::Rbx.into_physical_reg(),
                Register::Rsp.into_physical_reg(),
                Register::Rbp.into_physical_reg(),
                Register::R12.into_physical_reg(),
                Register::R13.into_physical_reg(),
                Register::R14.into_physical_reg(),
                Register::R15.into_physical_reg(),
            ]
        }
    }

    fn caller_saved_regs(&self) -> &'static [PhysicalRegister] {
        const {
            &[
                Register::Rax.into_physical_reg(),
                Register::Rcx.into_physical_reg(),
                Register::Rdx.into_physical_reg(),
                Register::Rsi.into_physical_reg(),
                Register::Rdi.into_physical_reg(),
                Register::R8.into_physical_reg(),
                Register::R9.into_physical_reg(),
                Register::R10.into_physical_reg(),
                Register::R11.into_physical_reg(),
            ]
        }
    }
}
