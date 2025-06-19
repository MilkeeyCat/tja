use crate::mir::{Operand, PhysicalRegister, Register, RegisterRole};

#[derive(Clone, Debug)]
pub enum Base {
    Register(PhysicalRegister),
    Label(String),
}

#[derive(Clone, Debug)]
pub struct AddressMode {
    pub base: Base,
    pub index: Option<PhysicalRegister>,
    pub scale: Option<usize>,
    pub displacement: Option<isize>,
}

impl AddressMode {
    pub fn write(self, operands: &mut Vec<Operand>, idx: usize) {
        match self.base {
            Base::Register(r) => operands.insert(
                idx,
                Operand::Register(Register::Physical(r), RegisterRole::Use),
            ),
            Base::Label(_) => unimplemented!(),
        }

        match self.index {
            Some(r) => operands.insert(
                idx + 1,
                Operand::Register(Register::Physical(r), RegisterRole::Use),
            ),
            None => operands.insert(idx + 1, Operand::Immediate(0)),
        }

        operands.insert(idx + 2, Operand::Immediate(self.scale.unwrap_or(0) as u64));
        operands.insert(
            idx + 3,
            Operand::Immediate(self.displacement.unwrap_or(0) as u64),
        );
    }
}
