use crate::mir::{Block, BlockId, Instruction, InstructionId, Target};
use slotmap::SlotMap;

struct InstructionNode<I: Instruction> {
    instr: I,
    prev: Option<InstructionId>,
    next: Option<InstructionId>,
    block: Option<BlockId>,
}

struct BlockNode {
    block: Block,
    prev: Option<BlockId>,
    next: Option<BlockId>,
}

pub(super) struct Function<T: Target> {
    instrs: SlotMap<InstructionId, InstructionNode<T::TargetInstr>>,
    blocks: SlotMap<BlockId, BlockNode>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}

impl<T: Target> Function<T> {
    fn create_block(&mut self, block: Block) -> BlockId {
        self.blocks.insert(BlockNode {
            block,
            prev: None,
            next: None,
        })
    }

    fn create_instr<I: Into<T::TargetInstr>>(&mut self, instr: I) -> InstructionId {
        self.instrs.insert(InstructionNode {
            instr: instr.into(),
            prev: None,
            next: None,
            block: None,
        })
    }

    fn append_block(&mut self, block: BlockId) {
        self.blocks[block].prev = self.last_block;

        if let Some(last) = self.last_block {
            self.blocks[last].next = Some(block);
        } else {
            self.first_block = Some(block);
        }

        self.last_block = Some(block);
    }

    pub(super) fn append_instr(&mut self, instr: InstructionId, block_id: BlockId) {
        let block = &mut self.blocks[block_id].block;
        let node = &mut self.instrs[instr];

        node.prev = block.last_instr;
        node.block = Some(block_id);

        if let Some(last) = block.last_instr {
            self.instrs[last].next = Some(instr);
        } else {
            block.first_instr = Some(instr);
        }

        block.last_instr = Some(instr);
    }

    fn insert_block_before(&mut self, block: BlockId, before: BlockId) {
        let after = self.blocks[before].prev;

        {
            let node = &mut self.blocks[block];

            node.prev = after;
            node.next = Some(before);
        }

        self.blocks[before].prev = Some(block);

        match after {
            Some(after) => self.blocks[after].next = Some(block),
            None => self.first_block = Some(block),
        }
    }

    fn insert_instr_before(&mut self, instr: InstructionId, before: InstructionId) {
        let block = self.instrs[before].block.unwrap();
        let after = self.instrs[before].prev;

        {
            let node = &mut self.instrs[instr];

            node.block = Some(block);
            node.prev = after;
            node.next = Some(before);
        }

        self.instrs[before].prev = Some(instr);

        match after {
            Some(after) => self.instrs[after].next = Some(instr),
            None => self.blocks[block].block.first_instr = Some(instr),
        }
    }

    fn entry_block(&self) -> Option<BlockId> {
        self.first_block
    }

    fn first_instr(&self, block: BlockId) -> Option<InstructionId> {
        self.blocks[block].block.first_instr
    }

    fn last_block(&self) -> Option<BlockId> {
        self.last_block
    }

    fn last_instr(&self, block: BlockId) -> Option<InstructionId> {
        self.blocks[block].block.last_instr
    }

    fn prev_block(&self, block: BlockId) -> Option<BlockId> {
        self.blocks[block].prev
    }

    fn prev_instr(&self, instr: InstructionId) -> Option<InstructionId> {
        self.instrs[instr].prev
    }

    fn next_block(&self, block: BlockId) -> Option<BlockId> {
        self.blocks[block].next
    }

    fn next_instr(&self, instr: InstructionId) -> Option<InstructionId> {
        self.instrs[instr].next
    }

    fn instr_block(&self, instr: InstructionId) -> Option<BlockId> {
        self.instrs[instr].block
    }
}

enum CursorPosition {
    Nowhere,
    Block(BlockId),
    Instr(InstructionId),
}

struct Cursor<'a, T: Target> {
    func: &'a mut Function<T>,

    pos: CursorPosition,
}

impl<'a, T: Target> Cursor<'a, T> {
    fn new(func: &'a mut Function<T>) -> Self {
        Self {
            func,
            pos: CursorPosition::Nowhere,
        }
    }

    fn current_block(&self) -> Option<BlockId> {
        match self.pos {
            CursorPosition::Nowhere => None,
            CursorPosition::Block(block) => Some(block),
            CursorPosition::Instr(instr) => self.func.instr_block(instr),
        }
    }

    fn current_instr(&self) -> Option<InstructionId> {
        match self.pos {
            CursorPosition::Nowhere | CursorPosition::Block(_) => None,
            CursorPosition::Instr(instr) => Some(instr),
        }
    }

    fn insert_block(&mut self, block: Block) {
        let block = self.func.create_block(block);

        match self.current_block() {
            Some(cur) => self.func.insert_block_before(block, cur),
            None => self.func.append_block(block),
        }
    }

    fn insert_instr<I: Into<T::TargetInstr>>(&mut self, instr: I) {
        let instr = self.func.create_instr(instr);

        match self.current_instr() {
            Some(cur) => self.func.insert_instr_before(instr, cur),
            None => self.func.append_instr(instr, self.current_block().unwrap()),
        }
    }

    fn prev_block(&mut self) -> Option<BlockId> {
        let prev = match self.current_block() {
            Some(block) => self.func.prev_block(block),
            None => self.func.last_block(),
        };

        if let Some(block) = prev {
            self.pos = CursorPosition::Block(block);
        }

        prev
    }

    fn prev_instr(&mut self) -> Option<InstructionId> {
        let prev = match self.current_instr() {
            Some(instr) => self.func.prev_instr(instr),
            None => self.func.last_instr(self.current_block().unwrap()),
        };

        if let Some(instr) = prev {
            self.pos = CursorPosition::Instr(instr);
        }

        prev
    }

    fn next_block(&mut self) -> Option<BlockId> {
        let next = match self.current_block() {
            Some(block) => self.func.next_block(block),
            None => self.func.entry_block(),
        };

        if let Some(block) = next {
            self.pos = CursorPosition::Block(block);
        }

        next
    }

    fn next_instr(&mut self) -> Option<InstructionId> {
        let next = match self.current_instr() {
            Some(instr) => self.func.next_instr(instr),
            None => self.func.first_instr(self.current_block().unwrap()),
        };

        if let Some(instr) = next {
            self.pos = CursorPosition::Instr(instr);
        }

        next
    }
}
