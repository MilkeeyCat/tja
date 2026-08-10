use crate::mir::PhysicalRegister;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct Register(u8);

include!(concat!(env!("OUT_DIR"), "/amd64/register.rs"));

impl PhysicalRegister for Register {}
