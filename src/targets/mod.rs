pub mod amd64;

use crate::mir::{Register, RegisterClass};

pub trait RegisterInfo {
    fn get_registers_by_class(&self, class: &RegisterClass) -> &[Register];
    fn get_name(r: &Register) -> &'static str;
}
