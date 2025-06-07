mod abi;
mod opcode;
pub mod register;

use crate::mir;
use abi::SysVAmd64;
use derive_more::Display;
pub use opcode::Opcode;
pub use register::Register;
use std::collections::HashMap;

// The terms "above" and "below" are associated with the CF flag and refer to
// the relationship between two unsigned integer values. The terms "greater" and
// "less" are associated with the SF and OF flags and refer to the relationship
// between two signed integer values.

#[derive(Debug, Display, Eq, PartialEq)]
#[repr(usize)]
pub enum Condition {
    #[display("a")]
    Above,
    #[display("ae")]
    AboveEqual,
    #[display("b")]
    Below,
    #[display("be")]
    BelowEqual,
    #[display("e")]
    Equal,
    #[display("ne")]
    NotEqual,
    #[display("g")]
    Greater,
    #[display("ge")]
    GreaterEqual,
    #[display("l")]
    Less,
    #[display("le")]
    LessEqual,

    Num,
}

impl From<usize> for Condition {
    fn from(value: usize) -> Self {
        assert!(value < Self::Num as usize);

        unsafe { std::mem::transmute::<_, Self>(value) }
    }
}

#[repr(usize)]
pub enum RegisterClass {
    Gpr64,
    Gpr32,
    Gpr16,
    Gpr8,
}

struct RegisterInfo(HashMap<mir::RegisterClass, Vec<mir::PhysicalRegister>>);

impl RegisterInfo {
    fn new() -> Self {
        Self(HashMap::from([
            (
                RegisterClass::Gpr64 as mir::RegisterClass,
                vec![
                    Register::R15 as mir::PhysicalRegister,
                    Register::R14 as mir::PhysicalRegister,
                    Register::R13 as mir::PhysicalRegister,
                    Register::R12 as mir::PhysicalRegister,
                    Register::R11 as mir::PhysicalRegister,
                    Register::R10 as mir::PhysicalRegister,
                    Register::R9 as mir::PhysicalRegister,
                    Register::Rcx as mir::PhysicalRegister,
                    Register::Rdx as mir::PhysicalRegister,
                    Register::Rsi as mir::PhysicalRegister,
                    Register::Rdi as mir::PhysicalRegister,
                    Register::Rax as mir::PhysicalRegister,
                    //Register::R8 as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr32 as mir::RegisterClass,
                vec![
                    Register::R15d as mir::PhysicalRegister,
                    Register::R14d as mir::PhysicalRegister,
                    Register::R13d as mir::PhysicalRegister,
                    Register::R12d as mir::PhysicalRegister,
                    Register::R11d as mir::PhysicalRegister,
                    Register::R10d as mir::PhysicalRegister,
                    Register::R9d as mir::PhysicalRegister,
                    Register::Ecx as mir::PhysicalRegister,
                    Register::Edx as mir::PhysicalRegister,
                    Register::Esi as mir::PhysicalRegister,
                    Register::Edi as mir::PhysicalRegister,
                    Register::Eax as mir::PhysicalRegister,
                    //Register::R8d as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr16 as mir::RegisterClass,
                vec![
                    Register::R15w as mir::PhysicalRegister,
                    Register::R14w as mir::PhysicalRegister,
                    Register::R13w as mir::PhysicalRegister,
                    Register::R12w as mir::PhysicalRegister,
                    Register::R11w as mir::PhysicalRegister,
                    Register::R10w as mir::PhysicalRegister,
                    Register::R9w as mir::PhysicalRegister,
                    Register::Cx as mir::PhysicalRegister,
                    Register::Dx as mir::PhysicalRegister,
                    Register::Si as mir::PhysicalRegister,
                    Register::Di as mir::PhysicalRegister,
                    Register::Ax as mir::PhysicalRegister,
                    //Register::R8w as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr8 as mir::RegisterClass,
                vec![
                    Register::R15b as mir::PhysicalRegister,
                    Register::R14b as mir::PhysicalRegister,
                    Register::R13b as mir::PhysicalRegister,
                    Register::R12b as mir::PhysicalRegister,
                    Register::R11b as mir::PhysicalRegister,
                    Register::R10b as mir::PhysicalRegister,
                    Register::R9b as mir::PhysicalRegister,
                    Register::Cl as mir::PhysicalRegister,
                    Register::Dl as mir::PhysicalRegister,
                    Register::Sil as mir::PhysicalRegister,
                    Register::Dil as mir::PhysicalRegister,
                    Register::Ah as mir::PhysicalRegister,
                    Register::Al as mir::PhysicalRegister,
                    //Register::R8b as mir::Register,
                ],
            ),
        ]))
    }
}

impl super::RegisterInfo for RegisterInfo {
    fn get_registers_by_class(&self, class: &mir::RegisterClass) -> &[mir::PhysicalRegister] {
        &self.0[class]
    }

    fn overlaps(&self, a: &mir::PhysicalRegister, b: &mir::PhysicalRegister) -> bool {
        let a: Register = (*a).into();
        let b: Register = (*b).into();

        a == b || a.contains(b) || b.contains(a)
    }

    fn get_name(&self, r: &mir::PhysicalRegister) -> &'static str {
        let r: Register = (*r).into();

        r.name()
    }
}

pub struct Target {
    register_info: RegisterInfo,
    abi: SysVAmd64,
}

impl Target {
    pub fn new() -> Self {
        Self {
            register_info: RegisterInfo::new(),
            abi: SysVAmd64::new(),
        }
    }
}

impl super::Target for Target {
    fn register_info(&self) -> &dyn super::RegisterInfo {
        &self.register_info
    }

    fn abi(&self) -> &dyn super::Abi {
        &self.abi
    }
}
