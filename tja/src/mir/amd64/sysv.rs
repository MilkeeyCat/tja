use crate::{
    hir::{self, TyStorage},
    lir::{self, ParamRanges},
    mir,
};

pub(super) struct Abi {
    calling_conv: CallingConv,
}

impl Abi {
    pub(super) fn new() -> Self {
        Self {
            calling_conv: CallingConv,
        }
    }
}

impl mir::Abi for Abi {
    fn field_offset(&self, ty_storage: &TyStorage, fields: &[hir::TyIdx], idx: usize) -> usize {
        fields[0..idx]
            .iter()
            .fold(0usize, |offset, ty| {
                offset.next_multiple_of(self.alignment(ty_storage, *ty))
                    + self.ty_size(ty_storage, *ty)
            })
            .next_multiple_of(self.alignment(ty_storage, fields[idx]))
    }

    fn ty_size(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize {
        match ty_storage.get(ty) {
            hir::Ty::I8 => 1,
            hir::Ty::I16 => 2,
            hir::Ty::I32 => 4,
            hir::Ty::I64 | hir::Ty::Ptr => 8,
            hir::Ty::Struct(fields) => {
                if fields.is_empty() {
                    0
                } else {
                    let last = fields.len() - 1;

                    (self.field_offset(ty_storage, fields, last)
                        + self.ty_size(ty_storage, fields[last]))
                    .next_multiple_of(self.alignment(ty_storage, ty))
                }
            }
            hir::Ty::Array { ty, len } => self.ty_size(ty_storage, *ty) * len,
        }
    }

    fn alignment(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize {
        match ty_storage.get(ty) {
            hir::Ty::Struct(fields) => fields
                .iter()
                .map(|ty| self.alignment(ty_storage, *ty))
                .max()
                .unwrap_or(1),
            hir::Ty::Array { ty, .. } => self.alignment(ty_storage, *ty),
            _ => self.ty_size(ty_storage, ty),
        }
    }

    fn calling_conv(&self) -> &dyn mir::CallingConvention {
        &self.calling_conv
    }
}

#[derive(PartialEq, Clone, Copy, Eq, Hash)]
enum ValueClass {
    NoClass,
    Integer,
    Sse,
    SseUp,
    Memory,
}

impl ValueClass {
    fn to_sig_value(
        &self,
        abi: &dyn mir::Abi,
        ty_storage: &TyStorage,
        ty: hir::TyIdx,
    ) -> lir::signature::Value {
        match self {
            ValueClass::NoClass => unreachable!(),
            ValueClass::Integer => lir::signature::Value {
                ty: lir::Ty::I64,
                kind: lir::signature::ValueKind::Normal,
            },
            ValueClass::Sse | ValueClass::SseUp => {
                unimplemented!("SSE/SSEUP classes are unsupported yet!")
            }
            ValueClass::Memory => {
                unreachable!("`ValueClass::Memory` should've been handled separately!")
            }
        }
    }
}

pub(super) struct CallingConv;

impl CallingConv {
    fn classify(
        eightbytes: &mut [ValueClass],
        ty: hir::TyIdx,
        ty_storage: &TyStorage,
        abi: &dyn mir::Abi,
        offset: usize,
    ) {
        match ty_storage.get(ty) {
            hir::Ty::I8 | hir::Ty::I16 | hir::Ty::I32 | hir::Ty::I64 | hir::Ty::Ptr => {
                let idx = offset / 8;

                eightbytes[idx] = Self::merge(eightbytes[idx], ValueClass::Integer);
            }
            hir::Ty::Struct(tys) => {
                for (idx, &ty) in tys.iter().enumerate() {
                    Self::classify(
                        eightbytes,
                        ty,
                        ty_storage,
                        abi,
                        offset + abi.field_offset(ty_storage, tys, idx),
                    );
                }
            }
            &hir::Ty::Array { ty, len } => {
                for idx in 0..len {
                    Self::classify(
                        eightbytes,
                        ty,
                        ty_storage,
                        abi,
                        offset + abi.ty_size(ty_storage, ty) * idx,
                    );
                }
            }
        }
    }

    fn ty_class(ty: hir::TyIdx, ty_storage: &TyStorage, abi: &dyn mir::Abi) -> Vec<ValueClass> {
        let size = abi.ty_size(ty_storage, ty);

        if size > 64 {
            return vec![ValueClass::Memory];
        }

        let mut eightbytes = vec![ValueClass::NoClass; size.div_ceil(8)];

        Self::classify(&mut eightbytes, ty, ty_storage, abi, 0);

        // post merger cleanup
        {
            // 5.a
            if eightbytes.contains(&ValueClass::Memory) {
                eightbytes.clear();
                eightbytes.push(ValueClass::Memory);
            }

            // 5.c
            if size > 16
                && let Some((first, rest)) = eightbytes.split_first()
                && (first != &ValueClass::Sse
                    || !rest.iter().all(|class| class == &ValueClass::SseUp))
            {
                eightbytes.clear();
                eightbytes.push(ValueClass::Memory);
            }
        }

        eightbytes
    }

    fn merge(lhs: ValueClass, rhs: ValueClass) -> ValueClass {
        match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs => lhs,
            (ValueClass::NoClass, other) | (other, ValueClass::NoClass) => other,
            (ValueClass::Memory, _) | (_, ValueClass::Memory) => ValueClass::Memory,
            (ValueClass::Integer, _) | (_, ValueClass::Integer) => ValueClass::Integer,
            _ => ValueClass::Sse,
        }
    }
}

impl mir::CallingConvention for CallingConv {
    fn lower_signature(
        &self,
        abi: &dyn mir::Abi,
        ty_storage: &TyStorage,
        sig: &hir::Signature,
    ) -> (lir::Signature, ParamRanges) {
        let mut params = vec![];
        let returns = sig
            .return_
            .map(|ty| {
                let classes = Self::ty_class(ty, ty_storage, abi);

                if classes.contains(&ValueClass::Memory) {
                    params.push(lir::signature::Value {
                        ty: lir::Ty::PTR,
                        kind: lir::signature::ValueKind::StructReturn,
                    });
                }

                classes
                    .into_iter()
                    .map(|class| {
                        if class == ValueClass::Memory {
                            lir::signature::Value {
                                ty: lir::Ty::PTR,
                                kind: lir::signature::ValueKind::Normal,
                            }
                        } else {
                            class.to_sig_value(abi, ty_storage, ty)
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        let mut param_ranges = ParamRanges::new(sig.params.len());
        let mut param_regs = ParamRegs::new();

        for &ty in &sig.params {
            let mut classes = Self::ty_class(ty, ty_storage, abi);

            if !classes.contains(&ValueClass::Memory) && !param_regs.try_alloc(&classes) {
                classes.clear();
                classes.push(ValueClass::Memory);
            }

            param_ranges.add(params.len());
            params.extend(classes.into_iter().map(|class| {
                if class == ValueClass::Memory {
                    lir::signature::Value {
                        ty: lir::Ty::PTR,
                        kind: lir::signature::ValueKind::StructArgument(
                            abi.ty_size(ty_storage, ty),
                        ),
                    }
                } else {
                    class.to_sig_value(abi, ty_storage, ty)
                }
            }));
        }

        param_ranges.finalize(params.len());

        (lir::Signature { params, returns }, param_ranges)
    }
}

#[derive(Clone, Copy)]
struct ParamRegs {
    integer: u8,
}

impl ParamRegs {
    fn new() -> Self {
        Self { integer: 6 }
    }

    fn try_alloc(&mut self, classes: &[ValueClass]) -> bool {
        let mut new = *self;

        for class in classes {
            match class {
                ValueClass::Integer => {
                    if new.integer == 0 {
                        return false;
                    }

                    new.integer -= 1;
                }
                ValueClass::Sse | ValueClass::SseUp => {
                    unimplemented!("SSE/SSEUP classes are unsupported yet!")
                }
                ValueClass::NoClass | ValueClass::Memory => unreachable!(),
            }
        }

        *self = new;

        true
    }
}
