use crate::mir::{Function, InstructionIdx};
use std::collections::HashSet;
use typed_generational_arena::StandardIndex;

pub type BlockIdx = StandardIndex<BasicBlock>;

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub successors: HashSet<BlockIdx>,
    pub instruction_head: Option<InstructionIdx>,
    pub instruction_tail: Option<InstructionIdx>,
    pub next: Option<BlockIdx>,
    pub prev: Option<BlockIdx>,
}

impl BasicBlock {
    pub fn new(name: String) -> Self {
        Self {
            name,
            successors: HashSet::new(),
            next: None,
            prev: None,
            instruction_head: None,
            instruction_tail: None,
        }
    }
}

pub struct Cursor<'a> {
    pub func: &'a Function,

    idx: Option<BlockIdx>,
}

impl<'a> Cursor<'a> {
    pub fn new(func: &'a Function) -> Self {
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

    pub fn idx(&self) -> Option<BlockIdx> {
        self.idx
    }

    pub fn set_idx(&mut self, idx: BlockIdx) {
        self.idx = Some(idx);
    }

    pub fn current(&self) -> Option<&BasicBlock> {
        self.idx.map(|idx| &self.func.blocks[idx])
    }

    pub fn peek_prev(&self) -> Option<BlockIdx> {
        match self.idx {
            None => self.func.block_tail,
            Some(idx) => self.func.blocks[idx].prev,
        }
    }

    pub fn peek_next(&self) -> Option<BlockIdx> {
        match self.idx {
            None => self.func.block_head,
            Some(idx) => self.func.blocks[idx].next,
        }
    }

    pub fn move_prev(&mut self) -> Option<BlockIdx> {
        self.idx = self.peek_prev();

        self.idx
    }

    pub fn move_next(&mut self) -> Option<BlockIdx> {
        self.idx = self.peek_next();

        self.idx
    }
}

pub struct CursorMut<'a> {
    pub func: &'a mut Function,

    idx: Option<BlockIdx>,
}

impl<'a> CursorMut<'a> {
    pub fn new(func: &'a mut Function) -> Self {
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

    pub fn idx(&self) -> Option<BlockIdx> {
        self.idx
    }

    pub fn set_idx(&mut self, idx: BlockIdx) {
        self.idx = Some(idx);
    }

    pub fn current(&self) -> Option<&BasicBlock> {
        self.idx.map(|idx| &self.func.blocks[idx])
    }

    pub fn current_mut(&mut self) -> Option<&mut BasicBlock> {
        self.idx.map(|idx| &mut self.func.blocks[idx])
    }

    pub fn peek_prev(&self) -> Option<BlockIdx> {
        match self.idx {
            None => self.func.block_tail,
            Some(idx) => self.func.blocks[idx].prev,
        }
    }

    pub fn peek_next(&self) -> Option<BlockIdx> {
        match self.idx {
            None => self.func.block_head,
            Some(idx) => self.func.blocks[idx].next,
        }
    }

    pub fn move_prev(&mut self) -> Option<BlockIdx> {
        self.idx = self.peek_prev();

        self.idx
    }

    pub fn move_next(&mut self) -> Option<BlockIdx> {
        self.idx = self.peek_next();

        self.idx
    }

    fn splice(
        &mut self,
        existing_prev: Option<BlockIdx>,
        existing_next: Option<BlockIdx>,
        splice_start: BlockIdx,
        splice_end: BlockIdx,
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

    pub fn insert_before(&mut self, idx: BlockIdx) {
        let prev = match self.idx {
            None => self.func.block_tail,
            Some(idx) => self.func.blocks[idx].prev,
        };

        self.splice(prev, self.idx, idx, idx);
        self.idx = Some(idx);
    }

    pub fn insert_after(&mut self, idx: BlockIdx) {
        let next = match self.idx {
            None => self.func.block_head,
            Some(idx) => self.func.blocks[idx].next,
        };

        self.splice(self.idx, next, idx, idx);
        self.idx = Some(idx);
    }
}
