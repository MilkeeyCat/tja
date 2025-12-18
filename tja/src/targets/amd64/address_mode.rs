use crate::{
    FunctionIdx, GlobalIdx,
    mir::{FrameIdx, Operand, OperandIdx, OperandInfo, Register, RegisterRole},
};
use index_vec::IndexVec;

#[derive(Clone, Debug)]
pub enum Base {
    Register(Register),
    Frame(FrameIdx),
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
    pub fn write(self, operands: &mut IndexVec<OperandIdx, Operand>, idx: OperandIdx) {
        let base = match self.base {
            Base::Register(reg) => Operand::Register(reg, RegisterRole::Use),
            Base::Frame(frame_idx) => Operand::Frame(frame_idx),
            Base::Function(idx) => Operand::Function(idx),
            Base::Global(idx) => Operand::Global(idx),
        };

        operands.insert(idx, base);

        match self.index {
            Some(reg) => operands.insert(idx + 1, Operand::Register(reg, RegisterRole::Use)),
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

impl IntoIterator for AddressMode {
    type Item = Operand;
    type IntoIter = <[Self::Item; 4] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        assert!(self.scale == 1 || self.scale == 2 || self.scale == 4 || self.scale == 8);

        let base = match self.base {
            Base::Register(reg) => Operand::Register(reg, RegisterRole::Use),
            Base::Frame(frame_idx) => Operand::Frame(frame_idx),
            Base::Function(idx) => Operand::Function(idx),
            Base::Global(idx) => Operand::Global(idx),
        };
        let index = match self.index {
            Some(reg) => Operand::Register(reg, RegisterRole::Use),
            None => Operand::Immediate(0),
        };

        [
            base,
            index,
            Operand::Immediate(self.scale as u64),
            Operand::Immediate(self.displacement.unwrap_or(0) as u64),
        ]
        .into_iter()
    }
}

impl OperandInfo for AddressMode {
    const LEN: usize = 4;
}
