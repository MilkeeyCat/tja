use crate::{
    FunctionIdx, GlobalIdx,
    mir::{Operand, Register, RegisterRole, StackFrameIdx},
};

#[derive(Clone, Debug)]
pub enum Base {
    Register(Register),
    Frame(StackFrameIdx),
    Function(FunctionIdx),
    Global(GlobalIdx),
}

#[derive(Clone, Debug)]
pub struct AddressMode {
    pub base: Base,
    pub index: Option<Register>,
    pub scale: usize,
    pub displacement: Option<isize>,
}

impl AddressMode {
    pub fn write(self, operands: &mut Vec<Operand>, idx: usize) {
        let base = match self.base {
            Base::Register(r) => Operand::Register(r, RegisterRole::Use),
            Base::Frame(stack_frame_idx) => Operand::Frame(stack_frame_idx),
            Base::Function(idx) => Operand::Function(idx),
            Base::Global(idx) => Operand::Global(idx),
        };

        operands.insert(idx, base);

        match self.index {
            Some(r) => operands.insert(idx + 1, Operand::Register(r, RegisterRole::Use)),
            None => operands.insert(idx + 1, Operand::Immediate(0)),
        }

        assert!(self.scale == 1 || self.scale == 2 || self.scale == 4 || self.scale == 8);
        operands.insert(idx + 2, Operand::Immediate(self.scale as u64));
        operands.insert(
            idx + 3,
            Operand::Immediate(self.displacement.unwrap_or(0) as u64),
        );
    }
}
