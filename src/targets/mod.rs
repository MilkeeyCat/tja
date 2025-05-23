pub mod amd64;

use crate::mir::{Register, RegisterClass};

pub trait RegisterInfo {
    fn get_registers_by_class(&self, class: &RegisterClass) -> &[Register];
    fn overlaps(&self, a: &Register, b: &Register) -> bool;
    fn get_name(&self, r: &Register) -> &'static str;
}

pub trait Target {
    fn register_info(&self) -> &dyn RegisterInfo;
}
