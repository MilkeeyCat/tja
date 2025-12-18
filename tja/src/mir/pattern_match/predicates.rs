use super::operands::{Immediate, Register};
use crate::{
    mir::{self, PhysicalRegister, RegisterRole, pattern_match::PatternCtx},
    targets::{Abi, RegisterInfo, Target, amd64::RegisterClass},
    ty::{Ty, TyIdx},
};

pub trait Predicate<T: Target, O> {
    fn matches(&self, ctx: &PatternCtx<'_, '_, T>, operand: &O) -> bool;
}

pub struct Eq<T>(T);

impl<T> Eq<T> {
    pub fn new(value: T) -> Box<Self> {
        Box::new(Self(value))
    }
}

impl<T: Target> Predicate<T, Immediate> for Eq<u64> {
    fn matches(&self, _ctx: &PatternCtx<'_, '_, T>, value: &Immediate) -> bool {
        value.value == self.0
    }
}

impl<T: Target> Predicate<T, Register> for Eq<PhysicalRegister> {
    fn matches(&self, _ctx: &PatternCtx<'_, '_, T>, value: &Register) -> bool {
        value.0 == mir::Register::Physical(self.0)
    }
}

pub struct HasType(TyIdx);

impl HasType {
    pub fn new(ty: TyIdx) -> Box<Self> {
        Box::new(Self(ty))
    }
}

impl<T: Target> Predicate<T, Register> for HasType {
    fn matches(&self, ctx: &PatternCtx<'_, '_, T>, value: &Register) -> bool {
        match &value.0 {
            mir::Register::Virtual(vreg_idx) => ctx.func.vreg_info.get_vreg(*vreg_idx).ty == self.0,
            mir::Register::Physical(reg) => {
                ctx.ctx.target.abi().ty_size(ctx.ctx.ty_storage, self.0)
                    == ctx.ctx.target.register_info().get_register_size(reg)
            }
        }
    }
}

impl<T: Target> Predicate<T, Immediate> for HasType {
    fn matches(&self, ctx: &PatternCtx<'_, '_, T>, value: &Immediate) -> bool {
        match value.ty {
            Some(ty) => self.0 == ty,
            None => match ctx.ctx.ty_storage.get_ty(self.0) {
                Ty::I8 => value.value <= u8::MAX as u64,
                Ty::I16 => value.value <= u16::MAX as u64,
                Ty::I32 => value.value <= u32::MAX as u64,
                Ty::I64 => value.value <= u64::MAX,
                _ => false,
            },
        }
    }
}

pub struct HasRegisterClass(RegisterClass);

impl HasRegisterClass {
    pub fn new(class: RegisterClass) -> Box<Self> {
        Box::new(Self(class))
    }
}

impl<T: Target> Predicate<T, Register> for HasRegisterClass {
    fn matches(&self, ctx: &PatternCtx<'_, '_, T>, value: &Register) -> bool {
        match &value.0 {
            mir::Register::Virtual(vreg_idx) => ctx
                .func
                .vreg_info
                .get_vreg(*vreg_idx)
                .class
                .map(|class| class == self.0.into())
                .unwrap_or_default(),
            mir::Register::Physical(reg) => ctx
                .ctx
                .target
                .register_info()
                .get_registers_by_class(&self.0.into())
                .contains(reg),
        }
    }
}

pub struct Def;

impl Def {
    pub fn new() -> Box<Self> {
        Box::new(Self)
    }
}

impl<T: Target> Predicate<T, Register> for Def {
    fn matches(&self, _ctx: &PatternCtx<'_, '_, T>, value: &Register) -> bool {
        value.1 == RegisterRole::Def
    }
}

pub struct Use;

impl Use {
    pub fn new() -> Box<Self> {
        Box::new(Self)
    }
}

impl<T: Target> Predicate<T, Register> for Use {
    fn matches(&self, _ctx: &PatternCtx<'_, '_, T>, value: &Register) -> bool {
        value.1 == RegisterRole::Use
    }
}
