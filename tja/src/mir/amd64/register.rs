use crate::mir::PhysicalRegister;

#[derive(Debug, PartialEq)]
pub(crate) struct Register(u8);

impl PhysicalRegister for Register {}

include!(concat!(env!("OUT_DIR"), "/amd64/register.rs"));
