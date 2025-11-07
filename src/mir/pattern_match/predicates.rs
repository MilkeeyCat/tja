use super::operands::{Immediate, Register};
use crate::{
    mir::{self, PhysicalRegister, RegisterRole, pattern_match::PatternCtx},
    targets::Target,
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
