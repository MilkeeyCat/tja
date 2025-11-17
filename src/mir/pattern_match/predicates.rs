use super::operands::{Immediate, Register};
use crate::{
    mir::{self, PhysicalRegister, RegisterRole, pattern_match::PatternCtx},
    targets::{Abi, RegisterInfo, Target, amd64::RegisterClass},
    ty::TyIdx,
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
        value.0 == self.0
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
