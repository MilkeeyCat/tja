use crate::hir::{
    Block, BlockBuilder, BlockId, Instruction, InstructionId, Module, TyIdx, TyStorage, Value,
    basic_block::{AppendInstrInserter, BlocksIter},
    instruction::{DisplayInstr, DisplayTerminator, InstrsIter},
    module::Declarations,
};
use index_vec::define_index_type;
use slotmap::SlotMap;
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

define_index_type! {
    pub struct FunctionIdx = usize;
}

pub struct Signature {
    pub(super) params: Vec<TyIdx>,
    pub(super) returns: Vec<TyIdx>,
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

struct BlockNode {
    block: Block,
    prev: Option<BlockId>,
    next: Option<BlockId>,
}

pub struct Function {
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

    pub(super) fn create_block(&mut self, params: Vec<TyIdx>) -> BlockId {
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

    pub(super) fn next_block(&self, block: BlockId) -> Option<BlockId> {
        self.blocks.get(block)?.next
    }

    pub(super) fn next_instr(&self, instr: InstructionId) -> Option<InstructionId> {
        self.instrs.get(instr)?.next
    }

    pub fn blocks_iter<'a>(&'a self) -> BlocksIter<'a> {
        BlocksIter::new(self, self.first_block)
    }

    pub fn instrs_iter<'a>(&'a self, block: BlockId) -> InstrsIter<'a> {
        InstrsIter::new(self, self.blocks[block].block.first_instr)
    }

    pub(super) fn block(&self, block: BlockId) -> &Block {
        &self.blocks[block].block
    }

    pub(super) fn block_mut(&mut self, block: BlockId) -> &mut Block {
        &mut self.blocks[block].block
    }

    pub(super) fn instr(&self, instr: InstructionId) -> &Instruction {
        &self.instrs[instr].instr
    }

    pub(super) fn display_instr<'a>(
        &'a self,
        instr: InstructionId,
        decls: &'a Declarations,
        ty_storage: &'a TyStorage,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    ) -> DisplayInstr<'a> {
        DisplayInstr::new(decls, self, ty_storage, instr_to_idx, instr)
    }

    pub(super) fn display_terminator<'a>(
        &'a self,
        block: BlockId,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    ) -> DisplayTerminator<'a> {
        DisplayTerminator::new(self, instr_to_idx, block)
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
        if self.func.first_block.is_none() {
            assert_eq!(
                self.decls.function(self.func.idx).sig.params,
                params,
                "entry basic block params must be equal to function's params"
            );
        }

        let block = self.func.create_block(params);

        self.func.append_block(block);

        block
    }

    pub fn block_params(&self, block: BlockId) -> &[Value] {
        &self.func.block(block).params
    }

    pub fn select_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn block_builder<'s>(&'s mut self) -> BlockBuilder<'s, AppendInstrInserter<'s>> {
        BlockBuilder::new(
            AppendInstrInserter::new(
                self.decls,
                self.ty_storage,
                self.current_block
                    .expect("basic block must be selected, consider calling `select_block` first"),
            ),
            self.func,
            self.decls,
            self.ty_storage,
        )
    }
}

pub struct DisplayFunction<'a> {
    module: &'a Module,
    ty_storage: &'a TyStorage,
    func: FunctionIdx,
}

impl<'a> DisplayFunction<'a> {
    pub(super) fn new(module: &'a Module, ty_storage: &'a TyStorage, func: FunctionIdx) -> Self {
        Self {
            module,
            ty_storage,
            func,
        }
    }
}

impl Display for DisplayFunction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decl = self.module.decls.function(self.func);
        let func = self.module.funcs.get(&self.func);

        if func.is_some() {
            write!(f, "define")?;
        } else {
            write!(f, "declare")?;
        }

        write!(f, " {}(", decl.name)?;

        let mut iter = decl.sig.params.iter().peekable();

        while let Some(ty) = iter.next() {
            write!(f, "{}", ty.display(self.ty_storage))?;

            if iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }

        write!(f, ")")?;

        if !decl.sig.returns.is_empty() {
            write!(f, " -> ")?;

            let mut iter = decl.sig.returns.iter().peekable();

            while let Some(ty) = iter.next() {
                write!(f, "{}", ty.display(self.ty_storage))?;

                if iter.peek().is_some() {
                    write!(f, ", ")?;
                }
            }
        }

        if let Some(func) = func {
            writeln!(f, " {{")?;

            let block_to_idx: BTreeMap<BlockId, usize> = func
                .blocks
                .keys()
                .enumerate()
                .map(|(idx, block)| (block, idx))
                .collect();
            let instr_to_idx: BTreeMap<_, _> = func
                .instrs
                .keys()
                .enumerate()
                .map(|(idx, instr)| (instr, idx))
                .collect();

            for block in func.blocks_iter() {
                write!(f, "bb{}(", block_to_idx[&block])?;

                let mut iter = func.block(block).params.iter().peekable();

                while let Some(value) = iter.next() {
                    write!(
                        f,
                        "{}: {}",
                        value.display(&instr_to_idx),
                        value.ty().display(self.ty_storage),
                    )?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                writeln!(f, "):")?;

                for instr in func.instrs_iter(block) {
                    writeln!(
                        f,
                        "\t{}",
                        func.display_instr(
                            instr,
                            &self.module.decls,
                            self.ty_storage,
                            &instr_to_idx
                        )
                    )?;
                }

                writeln!(f, "\t{}", func.display_terminator(block, &instr_to_idx))?;
            }

            writeln!(f, "}}")?;
        }

        Ok(())
    }
}
