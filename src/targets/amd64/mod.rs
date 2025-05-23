mod register;

use crate::mir;
use derive_more::Display;
use register::Register;
use std::collections::HashMap;

pub enum Opcode {
    Add,
    Sub,
    Imul,
    Idiv,
    Mov,
    Lea,
    Jmp,
    Test,
    Jcc,
}

// The terms "above" and "below" are associated with the CF flag and refer to
// the relationship between two unsigned integer values. The terms "greater" and
// "less" are associated with the SF and OF flags and refer to the relationship
// between two signed integer values.

#[derive(Debug, Display, Eq, PartialEq)]
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
}

#[repr(usize)]
pub enum RegisterClass {
    Gpr64,
    Gpr32,
    Gpr16,
    Gpr8,
}

struct RegisterInfo(HashMap<mir::RegisterClass, Vec<mir::Register>>);

impl RegisterInfo {
    fn new() -> Self {
        Self(HashMap::from([
            (
                RegisterClass::Gpr64 as mir::RegisterClass,
                vec![
                    Register::R15 as mir::Register,
                    Register::R14 as mir::Register,
                    Register::R13 as mir::Register,
                    Register::R12 as mir::Register,
                    Register::R11 as mir::Register,
                    Register::R10 as mir::Register,
                    Register::R9 as mir::Register,
                    Register::Rcx as mir::Register,
                    Register::Rdx as mir::Register,
                    Register::Rsi as mir::Register,
                    Register::Rdi as mir::Register,
                    Register::Rax as mir::Register,
                    //Register::R8 as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr32 as mir::RegisterClass,
                vec![
                    Register::R15d as mir::Register,
                    Register::R14d as mir::Register,
                    Register::R13d as mir::Register,
                    Register::R12d as mir::Register,
                    Register::R11d as mir::Register,
                    Register::R10d as mir::Register,
                    Register::R9d as mir::Register,
                    Register::Ecx as mir::Register,
                    Register::Edx as mir::Register,
                    Register::Esi as mir::Register,
                    Register::Edi as mir::Register,
                    Register::Eax as mir::Register,
                    //Register::R8d as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr16 as mir::RegisterClass,
                vec![
                    Register::R15w as mir::Register,
                    Register::R14w as mir::Register,
                    Register::R13w as mir::Register,
                    Register::R12w as mir::Register,
                    Register::R11w as mir::Register,
                    Register::R10w as mir::Register,
                    Register::R9w as mir::Register,
                    Register::Cx as mir::Register,
                    Register::Dx as mir::Register,
                    Register::Si as mir::Register,
                    Register::Di as mir::Register,
                    Register::Ax as mir::Register,
                    //Register::R8w as mir::Register,
                ],
            ),
            (
                RegisterClass::Gpr8 as mir::RegisterClass,
                vec![
                    Register::R15b as mir::Register,
                    Register::R14b as mir::Register,
                    Register::R13b as mir::Register,
                    Register::R12b as mir::Register,
                    Register::R11b as mir::Register,
                    Register::R10b as mir::Register,
                    Register::R9b as mir::Register,
                    Register::Cl as mir::Register,
                    Register::Dl as mir::Register,
                    Register::Sil as mir::Register,
                    Register::Dil as mir::Register,
                    Register::Ah as mir::Register,
                    Register::Al as mir::Register,
                    //Register::R8b as mir::Register,
                ],
            ),
        ]))
    }
}

impl super::RegisterInfo for RegisterInfo {
    fn get_registers_by_class(&self, class: &mir::RegisterClass) -> &[mir::Register] {
        &self.0[class]
    }

    fn overlaps(&self, a: &mir::Register, b: &mir::Register) -> bool {
        let a: Register = (*a).into();
        let b: Register = (*b).into();

        a == b || a.contains(b) || b.contains(a)
    }

    fn get_name(&self, r: &mir::Register) -> &'static str {
        let r: Register = (*r).into();

        r.name()
    }
}

pub struct Target {
    register_info: RegisterInfo,
}

impl Target {
    pub fn new() -> Self {
        Self {
            register_info: RegisterInfo::new(),
        }
    }
}

impl super::Target for Target {
    fn register_info(&self) -> &dyn super::RegisterInfo {
        &self.register_info
    }
}
