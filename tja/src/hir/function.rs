use crate::hir::{
    Block, BlockBuilder, BlockId, Instruction, InstructionId, Module, Terminator, TyIdx, TyStorage,
    Value, basic_block::AppendInstrInserter, module::Declarations,
};
use index_vec::define_index_type;
use slotmap::SlotMap;
use std::collections::HashMap;

define_index_type! {
    pub struct FunctionIdx = usize;
}

pub struct Signature {
    params: Vec<TyIdx>,
    returns: Vec<TyIdx>,
}

impl Signature {
    pub fn new(params: Vec<TyIdx>, returns: Vec<TyIdx>) -> Self {
        Self { params, returns }
    }
}

struct InstructionNode {
    instr: Instruction,
    prev: Option<InstructionId>,
    next: Option<InstructionId>,
}

impl InstructionNode {
    fn new(instr: Instruction) -> Self {
        Self {
            instr,
            prev: None,
            next: None,
        }
    }
}

struct BlockNode {
    block: Block,
    prev: Option<BlockId>,
    next: Option<BlockId>,
}

impl BlockNode {
    fn new(block: Block) -> Self {
        Self {
            block,
            prev: None,
            next: None,
        }
    }
}

#[derive(Default)]
pub struct Function {
    instrs: SlotMap<InstructionId, InstructionNode>,
    instr_results: HashMap<InstructionId, Vec<Value>>,
    blocks: SlotMap<BlockId, BlockNode>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}

impl Function {
    pub(super) fn create_instr(&mut self, instr: Instruction) -> InstructionId {
        self.instrs.insert(InstructionNode::new(instr))
    }

    pub(super) fn create_block(&mut self, params: Vec<TyIdx>) -> BlockId {
        self.blocks
            .insert_with_key(|block| BlockNode::new(Block::new(block, params)))
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

    pub(super) fn create_instr_results(&mut self, instr: InstructionId, values: Vec<Value>) {
        assert!(
            self.instr_results.insert(instr, values).is_none(),
            "instruction results already created"
        );
    }

    pub(super) fn instr_results(&self, instr: InstructionId) -> &[Value] {
        self.instr_results
            .get(&instr)
            .map(|values| values.as_ref())
            .unwrap_or(&[])
    }

    pub(super) fn set_terminator(&mut self, block: BlockId, terminator: Terminator) {
        self.blocks.get_mut(block).unwrap().block.terminator = Some(terminator);
    }
}

pub struct Builder<'a> {
    func: &'a mut Function,
    decls: &'a Declarations,
    ty_storage: &'a TyStorage,
    current_block: Option<BlockId>,
}

impl<'a> Builder<'a> {
    pub fn new(module: &'a mut Module, func: FunctionIdx, ty_storage: &'a TyStorage) -> Self {
        Self {
            func: module.funcs.get_mut(&func).unwrap(),
            decls: &module.decls,
            ty_storage,
            current_block: None,
        }
    }

    pub fn create_block(&mut self, params: Vec<TyIdx>) -> BlockId {
        let block = self.func.create_block(params);

        self.func.append_block(block);

        block
    }

    pub fn select_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn block_builder<'s>(&'s mut self) -> BlockBuilder<'s, AppendInstrInserter<'s>> {
        BlockBuilder::new(
            AppendInstrInserter::new(self.decls, self.ty_storage, self.current_block.unwrap()),
            self.func,
            self.decls,
            self.ty_storage,
        )
    }
}
