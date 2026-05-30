use crate::{
    FunctionIdx,
    lir::{
        Block, BlockBuilder, BlockId, Instruction, InstructionId, Module, Ty, Value,
        basic_block::AppendInstrInserter, module::Declarations,
    },
};
use slotmap::SlotMap;
use std::collections::HashMap;

struct InstructionNode {
    instr: Instruction,
    prev: Option<InstructionId>,
    next: Option<InstructionId>,
}

struct BlockNode {
    block: Block,
    prev: Option<BlockId>,
    next: Option<BlockId>,
}

pub(super) struct Function {
    pub(super) idx: FunctionIdx,
    instrs: SlotMap<InstructionId, InstructionNode>,
    instr_results: HashMap<InstructionId, Vec<Value>>,
    blocks: SlotMap<BlockId, BlockNode>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}

impl Function {
    pub(super) fn new(idx: FunctionIdx) -> Self {
        Self {
            idx,
            instrs: SlotMap::with_key(),
            instr_results: HashMap::new(),
            blocks: SlotMap::with_key(),
            first_block: None,
            last_block: None,
        }
    }

    pub(super) fn create_instr(&mut self, instr: Instruction) -> InstructionId {
        self.instrs.insert(InstructionNode {
            instr,
            prev: None,
            next: None,
        })
    }

    pub(super) fn create_block(&mut self, params: Vec<Ty>) -> BlockId {
        self.blocks.insert_with_key(|block| BlockNode {
            block: Block::new(block, params),
            prev: None,
            next: None,
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

    pub(super) fn append_instr(&mut self, instr: InstructionId, block: BlockId) {
        let block = &mut self.blocks[block].block;
        let node = &mut self.instrs[instr];

        node.prev = block.last_instr;

        if let Some(last) = block.last_instr {
            self.instrs[last].next = Some(instr);
        } else {
            block.first_instr = Some(instr);
        }

        block.last_instr = Some(instr);
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

    pub(super) fn block_mut(&mut self, block: BlockId) -> &mut Block {
        &mut self.blocks[block].block
    }
}

pub(crate) struct Builder<'a> {
    func: &'a mut Function,
    decls: &'a Declarations,
    current_block: Option<BlockId>,
}

impl<'a> Builder<'a> {
    pub(crate) fn new(module: &'a mut Module, func: FunctionIdx) -> Self {
        Self {
            func: module.funcs.get_mut(&func).unwrap(),
            decls: &module.decls,
            current_block: None,
        }
    }

    pub(crate) fn create_block(&mut self, params: Vec<Ty>) -> BlockId {
        if self.func.first_block.is_none() {
            assert_eq!(
                self.decls.funcs[self.func.idx]
                    .sig
                    .params
                    .iter()
                    .map(|value| value.ty)
                    .collect::<Vec<_>>(),
                params,
                "entry basic block params must be equal to function's params"
            );
        }

        let block = self.func.create_block(params);

        self.func.append_block(block);

        block
    }

    pub(crate) fn select_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub(crate) fn block_builder<'s>(&'s mut self) -> BlockBuilder<'s, AppendInstrInserter<'s>> {
        BlockBuilder::new(
            AppendInstrInserter::new(
                self.decls,
                self.current_block
                    .expect("basic block must be selected, consider calling `select_block` first"),
            ),
            self.func,
            self.decls,
        )
    }
}
