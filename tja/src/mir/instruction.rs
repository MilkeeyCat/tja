use crate::{
    FunctionIdx, GlobalIdx,
    mir::{BlockIdx, Function},
    targets::Register,
};
use derive_more::From;
pub use generated::{
    Add, Br, BrCond, Copy, FrameIndex, GenericInstruction, GlobalValue, ICmp, Load, Mul, PtrAdd,
    SDiv, Store, Sub, UDiv,
};
use slotmap::new_key_type;

mod generated {
    use super::{GlobalOrFunction, Instruction, Register, RegisterOrImmediate};
    use crate::{
        ConditionCode,
        mir::{BlockIdx, FrameIdx},
    };

    include!(concat!(env!("OUT_DIR"), "/generic_instruction.rs"));
}

new_key_type! {
    pub struct InstructionIdx;
}

pub trait Instruction {
    type Register: Register;
}

#[derive(Debug)]
pub enum RegisterOrImmediate<R: Register> {
    Register(R),
    Immediate(i64),
}

impl<R: Register> From<R> for RegisterOrImmediate<R> {
    fn from(reg: R) -> Self {
        Self::Register(reg)
    }
}

impl<R: Register> From<i64> for RegisterOrImmediate<R> {
    fn from(value: i64) -> Self {
        Self::Immediate(value)
    }
}

#[derive(Debug, From)]
pub enum GlobalOrFunction {
    Global(GlobalIdx),
    Immediate(FunctionIdx),
}

impl<I: Instruction> Instruction for GenericInstruction<I> {
    type Register = I::Register;
}

pub struct InstructionWrapper<I: Instruction> {
    pub instruction: I,
    pub next: Option<InstructionIdx>,
    pub prev: Option<InstructionIdx>,
}

pub struct Cursor<'a, I: Instruction> {
    pub func: &'a Function<I>,

    bb_idx: BlockIdx,
    idx: Option<InstructionIdx>,
}

impl<'a, I: Instruction> Cursor<'a, I> {
    pub fn new(func: &'a Function<I>, bb_idx: BlockIdx) -> Self {
        Self {
            func,
            bb_idx,
            idx: None,
        }
    }

    pub fn at_head(mut self) -> Self {
        self.move_to_head();

        self
    }

    pub fn at_tail(mut self) -> Self {
        self.move_to_tail();

        self
    }

    pub fn move_to_head(&mut self) {
        self.idx = self.func.blocks[self.bb_idx].instruction_head;
    }

    pub fn move_to_tail(&mut self) {
        self.idx = self.func.blocks[self.bb_idx].instruction_tail;
    }

    pub fn idx(&self) -> Option<InstructionIdx> {
        self.idx
    }

    pub fn set_idx(&mut self, idx: InstructionIdx) {
        self.idx = Some(idx);
    }

    pub fn current(&self) -> Option<&InstructionWrapper<I>> {
        self.idx.map(|idx| &self.func.instructions[idx])
    }

    pub fn peek_prev(&self) -> Option<InstructionIdx> {
        match self.idx {
            None => self.func.blocks[self.bb_idx].instruction_tail,
            Some(idx) => self.func.instructions[idx].prev,
        }
    }

    pub fn peek_next(&self) -> Option<InstructionIdx> {
        match self.idx {
            None => self.func.blocks[self.bb_idx].instruction_head,
            Some(idx) => self.func.instructions[idx].next,
        }
    }

    pub fn move_prev(&mut self) -> Option<InstructionIdx> {
        self.idx = self.peek_prev();

        self.idx
    }

    pub fn move_next(&mut self) -> Option<InstructionIdx> {
        self.idx = self.peek_next();

        self.idx
    }
}

pub struct CursorMut<'a, I: Instruction> {
    pub func: &'a mut Function<I>,

    bb_idx: BlockIdx,
    idx: Option<InstructionIdx>,
}

impl<'a, I: Instruction> CursorMut<'a, I> {
    pub fn new(func: &'a mut Function<I>, bb_idx: BlockIdx) -> Self {
        Self {
            func,
            bb_idx,
            idx: None,
        }
    }

    pub fn at_head(mut self) -> Self {
        self.move_to_head();

        self
    }

    pub fn at_tail(mut self) -> Self {
        self.move_to_tail();

        self
    }

    pub fn move_to_head(&mut self) {
        self.idx = self.func.blocks[self.bb_idx].instruction_head;
    }

    pub fn move_to_tail(&mut self) {
        self.idx = self.func.blocks[self.bb_idx].instruction_tail;
    }

    pub fn idx(&self) -> Option<InstructionIdx> {
        self.idx
    }

    pub fn set_idx(&mut self, idx: InstructionIdx) {
        self.idx = Some(idx);
    }

    pub fn current(&self) -> Option<&InstructionWrapper<I>> {
        self.idx.map(|idx| &self.func.instructions[idx])
    }

    pub fn current_mut(&mut self) -> Option<&mut InstructionWrapper<I>> {
        self.idx.map(|idx| &mut self.func.instructions[idx])
    }

    pub fn peek_prev(&self) -> Option<InstructionIdx> {
        match self.idx {
            None => self.func.blocks[self.bb_idx].instruction_tail,
            Some(idx) => self.func.instructions[idx].prev,
        }
    }

    pub fn peek_next(&self) -> Option<InstructionIdx> {
        match self.idx {
            None => self.func.blocks[self.bb_idx].instruction_head,
            Some(idx) => self.func.instructions[idx].next,
        }
    }

    pub fn move_prev(&mut self) -> Option<InstructionIdx> {
        self.idx = self.peek_prev();

        self.idx
    }

    pub fn move_next(&mut self) -> Option<InstructionIdx> {
        self.idx = self.peek_next();

        self.idx
    }

    fn splice(
        &mut self,
        existing_prev: Option<InstructionIdx>,
        existing_next: Option<InstructionIdx>,
        splice_start: InstructionIdx,
        splice_end: InstructionIdx,
    ) {
        if let Some(prev) = existing_prev {
            self.func.instructions[prev].next = Some(splice_start);
        } else {
            self.func.blocks[self.bb_idx].instruction_head = Some(splice_start);
        }

        if let Some(next) = existing_next {
            self.func.instructions[next].prev = Some(splice_end);
        } else {
            self.func.blocks[self.bb_idx].instruction_tail = Some(splice_end);
        }

        self.func.instructions[splice_start].prev = existing_prev;
        self.func.instructions[splice_end].next = existing_next;
    }

    pub fn insert_before(&mut self, idx: InstructionIdx) {
        let prev = match self.idx {
            None => self.func.blocks[self.bb_idx].instruction_tail,
            Some(idx) => self.func.instructions[idx].prev,
        };

        self.splice(prev, self.idx, idx, idx);
        self.idx = Some(idx);
    }

    pub fn insert_after(&mut self, idx: InstructionIdx) {
        let next = match self.idx {
            None => self.func.blocks[self.bb_idx].instruction_head,
            Some(idx) => self.func.instructions[idx].next,
        };

        self.splice(self.idx, next, idx, idx);
        self.idx = Some(idx);
    }

    pub fn remove_current(&mut self) {
        match self.idx {
            Some(idx) => {
                self.idx = self.func.instructions[idx].next;

                match self.func.instructions[idx].prev {
                    Some(prev) => {
                        self.func.instructions[prev].next = self.func.instructions[idx].next
                    }
                    None => {
                        self.func.blocks[self.bb_idx].instruction_head =
                            self.func.instructions[idx].next
                    }
                }

                match self.func.instructions[idx].next {
                    Some(prev) => {
                        self.func.instructions[prev].prev = self.func.instructions[idx].prev
                    }
                    None => {
                        self.func.blocks[self.bb_idx].instruction_tail =
                            self.func.instructions[idx].prev
                    }
                }
            }
            None => (),
        }
    }
}
