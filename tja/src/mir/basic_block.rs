use crate::mir::{
    Function,
    instruction::{Instruction, InstructionIdx},
};
use std::collections::HashSet;
use typed_generational_arena::StandardIndex;

pub type BlockIdx<I> = StandardIndex<BasicBlock<I>>;

#[derive(Debug)]
pub struct BasicBlock<I: Instruction> {
    pub name: String,
    pub successors: HashSet<BlockIdx<I>>,
    pub instruction_head: Option<InstructionIdx<I>>,
    pub instruction_tail: Option<InstructionIdx<I>>,
    pub next: Option<BlockIdx<I>>,
    pub prev: Option<BlockIdx<I>>,
}

impl<I: Instruction> BasicBlock<I> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            successors: HashSet::new(),
            instruction_head: None,
            instruction_tail: None,
            next: None,
            prev: None,
        }
    }
}

pub struct Cursor<'a, I: Instruction> {
    pub func: &'a Function<I>,

    idx: Option<BlockIdx<I>>,
}

impl<'a, I: Instruction> Cursor<'a, I> {
    pub fn new(func: &'a Function<I>) -> Self {
        Self { func, idx: None }
    }

    pub fn at_head(mut self) -> Self {
        self.move_to_head();

        self
    }

    pub fn move_to_head(&mut self) {
        self.idx = self.func.block_head;
    }

    pub fn at_tail(mut self) -> Self {
        self.move_to_tail();

        self
    }

    pub fn move_to_tail(&mut self) {
        self.idx = self.func.block_tail;
    }

    pub fn idx(&self) -> Option<BlockIdx<I>> {
        self.idx
    }

    pub fn set_idx(&mut self, idx: BlockIdx<I>) {
        self.idx = Some(idx);
    }

    pub fn current(&self) -> Option<&BasicBlock<I>> {
        self.idx.map(|idx| &self.func.blocks[idx])
    }

    pub fn peek_prev(&self) -> Option<BlockIdx<I>> {
        match self.idx {
            None => self.func.block_tail,
            Some(idx) => self.func.blocks[idx].prev,
        }
    }

    pub fn peek_next(&self) -> Option<BlockIdx<I>> {
        match self.idx {
            None => self.func.block_head,
            Some(idx) => self.func.blocks[idx].next,
        }
    }

    pub fn move_prev(&mut self) -> Option<BlockIdx<I>> {
        self.idx = self.peek_prev();

        self.idx
    }

    pub fn move_next(&mut self) -> Option<BlockIdx<I>> {
        self.idx = self.peek_next();

        self.idx
    }
}

pub struct CursorMut<'a, I: Instruction> {
    pub func: &'a mut Function<I>,

    idx: Option<BlockIdx<I>>,
}

impl<'a, I: Instruction> CursorMut<'a, I> {
    pub fn new(func: &'a mut Function<I>) -> Self {
        Self { func, idx: None }
    }

    pub fn at_head(mut self) -> Self {
        self.move_to_head();

        self
    }

    pub fn move_to_head(&mut self) {
        self.idx = self.func.block_head;
    }

    pub fn at_tail(mut self) -> Self {
        self.move_to_tail();

        self
    }

    pub fn move_to_tail(&mut self) {
        self.idx = self.func.block_tail;
    }

    pub fn idx(&self) -> Option<BlockIdx<I>> {
        self.idx
    }

    pub fn set_idx(&mut self, idx: BlockIdx<I>) {
        self.idx = Some(idx);
    }

    pub fn current(&self) -> Option<&BasicBlock<I>> {
        self.idx.map(|idx| &self.func.blocks[idx])
    }

    pub fn current_mut(&mut self) -> Option<&mut BasicBlock<I>> {
        self.idx.map(|idx| &mut self.func.blocks[idx])
    }

    pub fn peek_prev(&self) -> Option<BlockIdx<I>> {
        match self.idx {
            None => self.func.block_tail,
            Some(idx) => self.func.blocks[idx].prev,
        }
    }

    pub fn peek_next(&self) -> Option<BlockIdx<I>> {
        match self.idx {
            None => self.func.block_head,
            Some(idx) => self.func.blocks[idx].next,
        }
    }

    pub fn move_prev(&mut self) -> Option<BlockIdx<I>> {
        self.idx = self.peek_prev();

        self.idx
    }

    pub fn move_next(&mut self) -> Option<BlockIdx<I>> {
        self.idx = self.peek_next();

        self.idx
    }

    fn splice(
        &mut self,
        existing_prev: Option<BlockIdx<I>>,
        existing_next: Option<BlockIdx<I>>,
        splice_start: BlockIdx<I>,
        splice_end: BlockIdx<I>,
    ) {
        if let Some(prev) = existing_prev {
            self.func.blocks[prev].next = Some(splice_start);
        } else {
            self.func.block_head = Some(splice_start);
        }

        if let Some(next) = existing_next {
            self.func.blocks[next].prev = Some(splice_end);
        } else {
            self.func.block_tail = Some(splice_end);
        }

        self.func.blocks[splice_start].prev = existing_prev;
        self.func.blocks[splice_end].next = existing_next;
    }

    pub fn insert_before(&mut self, idx: BlockIdx<I>) {
        let prev = match self.idx {
            None => self.func.block_tail,
            Some(idx) => self.func.blocks[idx].prev,
        };

        self.splice(prev, self.idx, idx, idx);
        self.idx = Some(idx);
    }

    pub fn insert_after(&mut self, idx: BlockIdx<I>) {
        let next = match self.idx {
            None => self.func.block_head,
            Some(idx) => self.func.blocks[idx].next,
        };

        self.splice(self.idx, next, idx, idx);
        self.idx = Some(idx);
    }
}
