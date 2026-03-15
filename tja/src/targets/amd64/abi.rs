use crate::{
    targets::{Abi, amd64::Register},
    ty::{self, Ty, TyIdx},
};

#[derive(Default)]
pub struct SysV;

impl Abi for SysV {
    type Register = Register;

    fn field_offset(storage: &ty::Storage, fields: &[TyIdx], i: usize) -> usize {
        fields
            .iter()
            .take(i)
            .fold(0usize, |offset, ty| {
                offset.next_multiple_of(Self::alignment(storage, *ty)) + Self::ty_size(storage, *ty)
            })
            .next_multiple_of(Self::alignment(storage, fields[i]))
    }

    fn ty_size(storage: &ty::Storage, ty: TyIdx) -> usize {
        storage
            .get_ty(ty)
            .size()
            .unwrap_or_else(|| match storage.get_ty(ty) {
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
                Ty::Array { ty, len } => Self::ty_size(storage, *ty) * len,
                _ => panic!("unknown type"),
            })
    }

    fn alignment(storage: &ty::Storage, ty: TyIdx) -> usize {
        match storage.get_ty(ty) {
            Ty::Struct(fields) => fields
                .iter()
                .map(|ty| Self::alignment(storage, *ty))
                .max()
                .unwrap_or_default(),
            _ => Self::ty_size(storage, ty),
        }
    }

    fn callee_saved_regs() -> &'static [Self::Register] {
        &[
            Register::Rbx,
            Register::Rsp,
            Register::Rbp,
            Register::R12,
            Register::R13,
            Register::R14,
            Register::R15,
        ]
    }

    fn caller_saved_regs() -> &'static [Self::Register] {
        &[
            Register::Rax,
            Register::Rcx,
            Register::Rdx,
            Register::Rsi,
            Register::Rdi,
            Register::R8,
            Register::R9,
            Register::R10,
            Register::R11,
        ]
    }
}
