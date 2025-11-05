use super::operands::{Immediate, Register};
use crate::mir::{self, PhysicalRegister, RegisterRole};

pub trait Predicate<O> {
    fn matches(&self, operand: &O) -> bool;
}

pub struct Eq<T>(T);

impl<T> Eq<T> {
    pub fn new(value: T) -> Box<Self> {
        Box::new(Self(value))
    }
}

impl Predicate<Immediate> for Eq<u64> {
    fn matches(&self, value: &Immediate) -> bool {
        value.0 == self.0
    }
}

impl Predicate<Register> for Eq<PhysicalRegister> {
    fn matches(&self, value: &Register) -> bool {
        value.0 == mir::Register::Physical(self.0)
    }
}

pub struct Def;

impl Def {
    pub fn new() -> Box<Self> {
        Box::new(Self)
    }
}

impl Predicate<Register> for Def {
    fn matches(&self, value: &Register) -> bool {
        value.1 == RegisterRole::Def
    }
}

pub struct Use;

impl Use {
    pub fn new() -> Box<Self> {
        Box::new(Self)
    }
}

impl Predicate<Register> for Use {
    fn matches(&self, value: &Register) -> bool {
        value.1 == RegisterRole::Use
    }
}
