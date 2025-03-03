use crate::repr::ty::Ty;

pub struct Abi;

impl Abi {
    pub fn field_offset(fields: &[Ty], i: usize) -> usize {
        fields
            .iter()
            .take(i)
            .fold(0usize, |offset, ty| {
                offset.next_multiple_of(Self::alignment(ty)) + Self::ty_size(ty)
            })
            .next_multiple_of(Self::alignment(&fields[i]))
    }

    pub fn ty_size(ty: &Ty) -> usize {
        match ty {
            Ty::Struct(fields) => {
                if fields.is_empty() {
                    0
                } else {
                    let last_field = fields.len() - 1;
                    (Self::field_offset(fields, last_field) + Self::ty_size(&fields[last_field]))
                        .next_multiple_of(Self::alignment(ty))
                }
            }
            _ => ty.size(),
        }
    }

    pub fn alignment(ty: &Ty) -> usize {
        match ty {
            Ty::Struct(fields) => fields
                .iter()
                .map(|ty| Self::alignment(ty))
                .max()
                .unwrap_or_default(),
            _ => Self::ty_size(ty),
        }
    }
}
