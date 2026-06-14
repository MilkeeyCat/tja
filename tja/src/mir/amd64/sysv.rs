use crate::{
    hir::{self, FuncLoweringCtx, TyStorage},
    lir::{self, ParamRanges, Ty},
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
                    + self.hir_ty_size(ty_storage, *ty)
            })
            .next_multiple_of(self.alignment(ty_storage, fields[idx]))
    }

    fn hir_ty_size(&self, ty_storage: &TyStorage, ty: hir::TyIdx) -> usize {
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
                        + self.hir_ty_size(ty_storage, fields[last]))
                    .next_multiple_of(self.alignment(ty_storage, ty))
                }
            }
            hir::Ty::Array { ty, len } => self.hir_ty_size(ty_storage, *ty) * len,
        }
    }

    fn lir_ty_size(&self, ty: lir::Ty) -> usize {
        match ty {
            lir::Ty::I8 => 1,
            lir::Ty::I16 => 2,
            lir::Ty::I32 => 4,
            lir::Ty::I64 | lir::Ty::PTR => 8,
            _ => unreachable!(),
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
            _ => self.hir_ty_size(ty_storage, ty),
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

impl From<ValueClass> for lir::signature::Value {
    fn from(class: ValueClass) -> Self {
        match class {
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
                        offset + abi.hir_ty_size(ty_storage, ty) * idx,
                    );
                }
            }
        }
    }

    fn ty_class(ty: hir::TyIdx, ty_storage: &TyStorage, abi: &dyn mir::Abi) -> Vec<ValueClass> {
        let size = abi.hir_ty_size(ty_storage, ty);

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
                            class.into()
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
                            abi.hir_ty_size(ty_storage, ty),
                        ),
                    }
                } else {
                    class.into()
                }
            }));
        }

        param_ranges.finalize(params.len());

        (lir::Signature { params, returns }, param_ranges)
    }

    fn lower_entry_block_params(
        &self,
        ctx: &mut FuncLoweringCtx,
        hir_params: &[hir::Value],
        lir_params: &[(lir::signature::Value, lir::Value)],
        param_ranges: &ParamRanges,
    ) {
        for (hir_param, lir_params) in hir_params
            .iter()
            .zip((0..hir_params.len()).map(|idx| &lir_params[param_ranges.get(idx)]))
        {
            let ty = hir_param.ty();
            let mut param_values = Vec::new();

            match lir_params {
                [
                    (
                        lir::signature::Value {
                            kind: lir::signature::ValueKind::StructArgument(size),
                            ..
                        },
                        value,
                    ),
                ] => {
                    assert_eq!(ctx.abi.hir_ty_size(ctx.ty_storage, ty), *size);

                    let mut builder = ctx.lir_func_builder.block_builder();

                    for (offset, ty) in ty
                        .offset_iter(ctx.ty_storage, ctx.abi)
                        .zip(ty.lir_ty_iter(ctx.ty_storage))
                    {
                        let offset = builder.iconst(i64::try_from(offset).unwrap(), Ty::I64);
                        let ptr = builder.ptr_add(*value, offset);

                        param_values.push(builder.load(ptr, ty));
                    }
                }
                values
                    if values
                        .iter()
                        .all(|(value, _)| value.kind == lir::signature::ValueKind::Normal) =>
                {
                    let size = ctx.abi.lir_ty_size(values[0].1.ty());

                    for (offset, ty) in ty
                        .offset_iter(ctx.ty_storage, ctx.abi)
                        .zip(ty.lir_ty_iter(ctx.ty_storage))
                    {
                        let (_, value) = &values[offset / size];
                        let relative_offset = offset % size;
                        let value = if ctx.abi.lir_ty_size(ty) == size {
                            if ty == Ty::PTR {
                                ctx.lir_func_builder.block_builder().int_to_ptr(*value)
                            } else {
                                assert_eq!(ty, value.ty());

                                *value
                            }
                        } else {
                            let mut builder = ctx.lir_func_builder.block_builder();
                            let shift_amount =
                                builder.iconst(u8::try_from(relative_offset * 8).unwrap(), Ty::I8);
                            let shifted = builder.lshr(*value, shift_amount);

                            builder.trunc(shifted, ty)
                        };

                        param_values.push(value);
                    }
                }
                _ => unreachable!(),
            }

            ctx.lower_param(*hir_param, param_values);
        }
    }

    fn lower_ret(
        &self,
        ctx: &mut FuncLoweringCtx,
        value: Option<(hir::Value, &[lir::signature::Value])>,
    ) {
        let values = if let Some((value, sig_values)) = value {
            let ty = value.ty();
            let values = ctx.lowered_value(value).to_vec();

            if ctx.lir_func_builder.decls.funcs[ctx.lir_func_builder.func.idx]
                .sig
                .params[0]
                .kind
                == lir::signature::ValueKind::StructReturn
            {
                let base = ctx
                    .lir_func_builder
                    .func
                    .block_params(ctx.lir_func_builder.func.entry_block().unwrap())[0];
                let mut builder = ctx.lir_func_builder.block_builder();

                for (offset, value) in ty.offset_iter(ctx.ty_storage, ctx.abi).zip(values) {
                    let offset = builder.iconst(i64::try_from(offset).unwrap(), Ty::I64);
                    let ptr = builder.ptr_add(base, offset);

                    builder.store(ptr, value);
                }

                vec![base]
            } else {
                let ret_ty = sig_values[0].ty;
                let size = ctx.abi.lir_ty_size(ret_ty);
                let mut ret_values = Vec::new();
                let mut ret_value: Option<lir::Value> = None;
                let mut iter = values
                    .into_iter()
                    .zip(ty.offset_iter(ctx.ty_storage, ctx.abi))
                    .zip(ty.lir_ty_iter(ctx.ty_storage))
                    .peekable();

                while let Some(((value, offset), ty)) = iter.next() {
                    let value = if ctx.abi.lir_ty_size(ty) == size {
                        if ty == Ty::PTR {
                            ctx.lir_func_builder
                                .block_builder()
                                .ptr_to_int(value, Ty::I64)
                        } else {
                            assert_eq!(ty, value.ty());

                            value
                        }
                    } else {
                        let relative_offset = offset % size;
                        let mut builder = ctx.lir_func_builder.block_builder();
                        let extended = builder.zext(value, ret_ty);
                        let shift_amount =
                            builder.iconst(u8::try_from(relative_offset * 8).unwrap(), Ty::I8);

                        builder.shl(extended, shift_amount)
                    };
                    let value = ret_value.map_or(value, |ret_value| {
                        ctx.lir_func_builder.block_builder().or(ret_value, value)
                    });
                    let next_offset = iter.peek().map(|&((_, offset), _)| offset);

                    if let Some(next_offset) = next_offset
                        && offset / size != next_offset / size
                    {
                        ret_values.push(value);
                        ret_value = None;
                    } else {
                        ret_value = Some(value);
                    }
                }

                if let Some(ret_value) = ret_value {
                    ret_values.push(ret_value);
                }

                ret_values
            }
        } else {
            Vec::new()
        };

        ctx.lir_func_builder.block_builder().ret(values);
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
