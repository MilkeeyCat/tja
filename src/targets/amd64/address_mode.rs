use crate::{
    hir::FunctionIdx,
    mir::{Operand, PhysicalRegister, Register, RegisterRole, StackFrameIdx},
};

#[derive(Clone, Debug)]
pub enum Base {
    Register(Register),
    Frame(StackFrameIdx),
    Function(FunctionIdx),
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
        let base = match self.base {
            Base::Register(r) => Operand::Register(r, RegisterRole::Use),
            Base::Frame(stack_frame_idx) => Operand::Frame(stack_frame_idx),
            Base::Function(idx) => Operand::Function(idx),
        };

        operands.insert(idx, base);

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
