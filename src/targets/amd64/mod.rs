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

impl super::RegisterInfo for RegisterInfo {
    fn get_registers_by_class(&self, class: &mir::RegisterClass) -> &[mir::Register] {
        &self.0[class]
    }

    fn get_name(r: &mir::Register) -> &'static str {
        let r: Register = (*r).into();

        r.name()
    }
}
