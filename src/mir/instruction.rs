use crate::{
    dataflow::DefsUses,
    mir::{
        BlockIdx, FrameIdx, Function, GenericOpcode, Opcode, Operand, OperandIdx, Register,
        RegisterRole,
    },
};
use index_vec::IndexVec;
use std::collections::HashSet;
use typed_generational_arena::StandardIndex;

pub type InstructionIdx = StandardIndex<Instruction>;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: IndexVec<OperandIdx, Operand>,
    pub tied_operands: Option<(OperandIdx, OperandIdx)>,
    pub implicit_defs: HashSet<Register>,
    pub implicit_uses: HashSet<Register>,
    pub next: Option<InstructionIdx>,
    pub prev: Option<InstructionIdx>,
}

impl Instruction {
    pub fn new(opcode: Opcode) -> Self {
        Self {
            opcode,
            operands: IndexVec::new(),
            tied_operands: None,
            implicit_defs: HashSet::new(),
            implicit_uses: HashSet::new(),
            next: None,
            prev: None,
        }
    }

    pub fn is_copy(&self) -> bool {
        self.opcode == GenericOpcode::Copy.into()
    }

    pub fn defs_uses(&self) -> DefsUses {
        DefsUses {
            defs: self
                .operands
                .iter()
                .filter_map(|operand| match operand {
                    Operand::Register(r, RegisterRole::Def) => Some(r.clone()),
                    _ => None,
                })
                .chain(self.implicit_defs.iter().cloned())
                .collect(),
            uses: self
                .operands
                .iter()
                .filter_map(|operand| match operand {
                    Operand::Register(r, RegisterRole::Use) => Some(r.clone()),
                    _ => None,
                })
                .chain(self.implicit_uses.iter().cloned())
                .collect(),
        }
    }
}

pub struct Cursor<'a> {
    pub func: &'a Function,

    bb_idx: BlockIdx,
    idx: Option<InstructionIdx>,
}

impl<'a> Cursor<'a> {
    pub fn new(func: &'a Function, bb_idx: BlockIdx) -> Self {
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

    pub fn current(&self) -> Option<&Instruction> {
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

pub struct CursorMut<'a> {
    pub func: &'a mut Function,

    bb_idx: BlockIdx,
    idx: Option<InstructionIdx>,
}

impl<'a> CursorMut<'a> {
    pub fn new(func: &'a mut Function, bb_idx: BlockIdx) -> Self {
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

    pub fn current(&self) -> Option<&Instruction> {
        self.idx.map(|idx| &self.func.instructions[idx])
    }

    pub fn current_mut(&mut self) -> Option<&mut Instruction> {
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

pub type GenericBuilder<'a> = Builder<'a, ()>;
pub type ManualBuilder<'a> = Builder<'a, InstructionIdx>;

pub struct Builder<'a, S> {
    pub(crate) func: &'a mut Function,
    pub(crate) state: S,
}

impl<'a> GenericBuilder<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self { func, state: () }
    }

    pub fn with_opcode(self, opcode: Opcode) -> ManualBuilder<'a> {
        let idx = self.func.instructions.insert(Instruction::new(opcode));

        Builder {
            func: self.func,
            state: idx,
        }
    }

    pub fn frame_idx(self, lhs: Register, rhs: FrameIdx) -> InstructionIdx {
        self.with_opcode(GenericOpcode::FrameIndex.into())
            .add_def(lhs)
            .add_operand(rhs.into())
            .idx()
    }

    pub fn ptr_add(self, out: Register, lhs: Operand, rhs: Operand) -> InstructionIdx {
        assert!(matches!(
            rhs,
            Operand::Register(_, _) | Operand::Immediate(_)
        ));

        self.with_opcode(GenericOpcode::PtrAdd.into())
            .add_def(out)
            .add_operand(lhs)
            .add_operand(rhs)
            .idx()
    }

    pub fn load(self, lhs: Register, rhs: Operand) -> InstructionIdx {
        self.with_opcode(GenericOpcode::Load.into())
            .add_def(lhs)
            .add_operand(rhs)
            .idx()
    }

    pub fn store(self, lhs: Register, rhs: Operand) -> InstructionIdx {
        self.with_opcode(GenericOpcode::Store.into())
            .add_use(lhs)
            .add_operand(rhs)
            .idx()
    }

    pub fn br(self, idx: BlockIdx) -> InstructionIdx {
        self.with_opcode(GenericOpcode::Br.into())
            .add_operand(Operand::Block(idx))
            .idx()
    }

    pub fn global_value(self, lhs: Register, rhs: Operand) -> InstructionIdx {
        assert!(matches!(rhs, Operand::Global(..) | Operand::Function(..)));

        self.with_opcode(GenericOpcode::GlobalValue.into())
            .add_def(lhs)
            .add_operand(rhs)
            .idx()
    }

    pub fn copy(self, lhs: Register, rhs: Operand) -> InstructionIdx {
        self.with_opcode(GenericOpcode::Copy.into())
            .add_def(lhs)
            .add_operand(rhs)
            .idx()
    }
}

impl ManualBuilder<'_> {
    pub fn add_operand(&mut self, operand: Operand) -> &mut Self {
        self.func.add_operand(self.state, operand);

        self
    }

    pub fn add_def(&mut self, reg: Register) -> &mut Self {
        self.add_operand(Operand::def(reg));

        self
    }

    pub fn add_use(&mut self, reg: Register) -> &mut Self {
        self.add_operand(Operand::not_def(reg));

        self
    }

    pub fn add_implicit_def(&mut self, reg: Register) -> &mut Self {
        self.func.add_implicit_def(self.state, reg);

        self
    }

    pub fn idx(&self) -> InstructionIdx {
        self.state
    }
}
