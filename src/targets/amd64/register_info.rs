use super::{
    RegisterClass,
    register::{Register, RegisterDesc},
};
use crate::mir;
use std::collections::HashMap;

pub struct RegisterInfo {
    register_descs: HashMap<Register, RegisterDesc>,
    register_classes: HashMap<mir::RegisterClass, Vec<mir::PhysicalRegister>>,
}

include!(concat!(env!("OUT_DIR"), "/amd64/register_info.rs"));

impl crate::targets::RegisterInfo for RegisterInfo {
    fn get_registers_by_class(&self, class: &mir::RegisterClass) -> &[mir::PhysicalRegister] {
        &self.register_classes[class]
    }

    fn overlaps(&self, a: &mir::PhysicalRegister, b: &mir::PhysicalRegister) -> bool {
        fn contains(
            reg_info: &RegisterInfo,
            a: &mir::PhysicalRegister,
            b: &mir::PhysicalRegister,
        ) -> bool {
            reg_info.register_descs[&(*a).into()]
                .subregs
                .iter()
                .any(|a: &Register| {
                    &a.into_physical_reg() == b || contains(reg_info, &(*a).into(), b)
                })
        }

        a == b || contains(self, a, b) || contains(self, b, a)
    }

    fn get_name(&self, r: &mir::PhysicalRegister) -> &'static str {
        let r: Register = (*r).into();

        self.register_descs[&r].name
    }

    fn get_register_size(&self, r: &mir::PhysicalRegister) -> usize {
        let r: Register = (*r).into();

        self.register_descs[&r].size
    }
}
