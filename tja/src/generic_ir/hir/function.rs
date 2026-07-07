use crate::{
    FunctionIdx,
    hir::{
        Block, BlockBuilder, BlockId, Instruction, InstructionId, Module, TargetInstruction,
        Terminator, TyIdx, TyStorage, Value,
        basic_block::{AppendInstrInserter, BlocksIter},
        instruction::{DisplayInstr, DisplayTerminator, InstrsIter},
        module::Declarations,
    },
};
use slotmap::{SecondaryMap, SlotMap};
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

pub struct Signature {
    pub(crate) params: Vec<TyIdx>,
    pub(crate) return_: Option<TyIdx>,
}

impl Signature {
    pub fn new(params: Vec<TyIdx>, return_: Option<TyIdx>) -> Self {
        Self { params, return_ }
    }
}

struct InstructionNode<TI: TargetInstruction> {
    instr: Instruction<TI>,
    prev: Option<InstructionId>,
    next: Option<InstructionId>,
}

struct BlockNode {
    block: Block,
    prev: Option<BlockId>,
    next: Option<BlockId>,
}

pub(crate) struct Function<TI: TargetInstruction> {
    pub(super) idx: FunctionIdx,
    // NOTE: it's not an elegant solution, but there has to be separate
    // `SlotMap` & `SecondaryMap`. `SlotMap` doesn't support mapping values
    // while preserving keys, so it's pain in the ass to implement
    // `Function::convert`. To make everything easier `Function::instr_ids` is
    // used only for generating keys, and `Function::instrs` acts as a map, this
    // way it's possible to keep using already generated keys
    instr_ids: SlotMap<InstructionId, ()>,
    instrs: SecondaryMap<InstructionId, InstructionNode<TI>>,
    instr_results: HashMap<InstructionId, Vec<Value>>,
    blocks: SlotMap<BlockId, BlockNode>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}

impl<TI: TargetInstruction> Function<TI> {
    pub(super) fn new(idx: FunctionIdx) -> Self {
        Self {
            idx,
            instr_ids: SlotMap::with_key(),
            instrs: SecondaryMap::new(),
            instr_results: HashMap::new(),
            blocks: SlotMap::with_key(),
            first_block: None,
            last_block: None,
        }
    }

    pub(super) fn create_instr(&mut self, instr: Instruction<TI>) -> InstructionId {
        let id = self.instr_ids.insert(());

        assert!(
            self.instrs
                .insert(
                    id,
                    InstructionNode {
                        instr,
                        prev: None,
                        next: None,
                    },
                )
                .is_none()
        );

        id
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

    pub(crate) fn instr_results(&self, instr: InstructionId) -> &[Value] {
        self.instr_results
            .get(&instr)
            .map(|values| values.as_ref())
            .unwrap_or(&[])
    }

    pub(super) fn block_params(&self, block: BlockId) -> &[Value] {
        &self.block(block).params
    }

    pub(super) fn next_block(&self, block: BlockId) -> Option<BlockId> {
        self.blocks.get(block)?.next
    }

    pub(super) fn next_instr(&self, instr: InstructionId) -> Option<InstructionId> {
        self.instrs.get(instr)?.next
    }

    pub(super) fn blocks_iter<'a>(&'a self) -> BlocksIter<'a, TI> {
        BlocksIter::new(self, self.first_block)
    }

    pub(super) fn instrs_iter<'a>(&'a self, block: BlockId) -> InstrsIter<'a, TI> {
        InstrsIter::new(self, self.blocks[block].block.first_instr)
    }

    pub(super) fn block(&self, block: BlockId) -> &Block {
        &self.blocks[block].block
    }

    pub(super) fn block_mut(&mut self, block: BlockId) -> &mut Block {
        &mut self.blocks[block].block
    }

    pub(super) fn instr(&self, instr: InstructionId) -> &Instruction<TI> {
        &self.instrs[instr].instr
    }

    pub(super) fn terminator(&self, block: BlockId) -> &Terminator {
        self.block(block).terminator()
    }

    pub(super) fn display_instr<'a>(
        &'a self,
        instr: InstructionId,
        decls: &'a Declarations,
        ty_storage: &'a TyStorage,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    ) -> DisplayInstr<'a, TI> {
        DisplayInstr::new(decls, self, ty_storage, instr_to_idx, instr)
    }

    pub(super) fn display_terminator<'a>(
        &'a self,
        block: BlockId,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    ) -> DisplayTerminator<'a, TI> {
        DisplayTerminator::new(self, instr_to_idx, block)
    }

    pub(super) fn convert<TI2: TargetInstruction + From<TI>>(self) -> Function<TI2> {
        let instrs = self
            .instrs
            .into_iter()
            .map(|(id, InstructionNode { instr, prev, next })| {
                (
                    id,
                    InstructionNode {
                        instr: match instr {
                            Instruction::Const { const_ } => Instruction::Const { const_ },
                            Instruction::Target(instr) => Instruction::Target(instr.into()),
                        },
                        prev,
                        next,
                    },
                )
            })
            .collect();

        Function {
            idx: self.idx,
            instr_ids: self.instr_ids,
            instrs,
            instr_results: self.instr_results,
            blocks: self.blocks,
            first_block: self.first_block,
            last_block: self.last_block,
        }
    }
}

pub struct Builder<'a, TI: TargetInstruction> {
    func: &'a mut Function<TI>,
    decls: &'a Declarations,
    ty_storage: &'a mut TyStorage,
    current_block: Option<BlockId>,
}

impl<'a, TI: TargetInstruction> Builder<'a, TI> {
    pub fn new(
        module: &'a mut Module<TI>,
        func: FunctionIdx,
        ty_storage: &'a mut TyStorage,
    ) -> Self {
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
        self.func.block_params(block)
    }

    pub fn select_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn block_builder<'s>(&'s mut self) -> BlockBuilder<'s, TI, AppendInstrInserter<'s>> {
        BlockBuilder::new(
            AppendInstrInserter::new(
                self.decls,
                self.current_block
                    .expect("basic block must be selected, consider calling `select_block` first"),
            ),
            self.func,
            self.decls,
            self.ty_storage,
        )
    }
}

pub struct DisplayFunction<'a, TI: TargetInstruction> {
    module: &'a Module<TI>,
    ty_storage: &'a TyStorage,
    func: FunctionIdx,
}

impl<'a, TI: TargetInstruction> DisplayFunction<'a, TI> {
    pub(super) fn new(
        module: &'a Module<TI>,
        ty_storage: &'a TyStorage,
        func: FunctionIdx,
    ) -> Self {
        Self {
            module,
            ty_storage,
            func,
        }
    }
}

impl<TI: TargetInstruction> Display for DisplayFunction<'_, TI> {
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

        if let Some(ty) = decl.sig.return_ {
            write!(f, " -> {}", ty.display(self.ty_storage))?;
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
