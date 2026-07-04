use crate::{
    FunctionIdx,
    lir::{
        Block, BlockBuilder, BlockId, Instruction, InstructionId, Module, TargetInstruction, Ty,
        Value,
        basic_block::{AppendInstrInserter, BlocksIter},
        instruction::{DisplayInstr, DisplayTerminator, InstrsIter},
        module::Declarations,
        signature,
    },
};
use slotmap::SlotMap;
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

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
    pub(crate) idx: FunctionIdx,
    instrs: SlotMap<InstructionId, InstructionNode<TI>>,
    instr_results: HashMap<InstructionId, Vec<Value>>,
    blocks: SlotMap<BlockId, BlockNode>,
    first_block: Option<BlockId>,
    last_block: Option<BlockId>,
}

impl<TI: TargetInstruction> Function<TI> {
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

    pub(super) fn create_instr(&mut self, instr: Instruction<TI>) -> InstructionId {
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

    pub(crate) fn instr_results(&self, instr: InstructionId) -> &[Value] {
        self.instr_results
            .get(&instr)
            .map(|values| values.as_ref())
            .unwrap_or(&[])
    }

    pub(crate) fn block_params(&self, block: BlockId) -> &[Value] {
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

    pub(crate) fn entry_block(&self) -> Option<BlockId> {
        self.first_block
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

    pub(super) fn display_instr<'a>(
        &'a self,
        instr: InstructionId,
        decls: &'a Declarations,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    ) -> DisplayInstr<'a, TI> {
        DisplayInstr::new(decls, self, instr_to_idx, instr)
    }

    pub(super) fn display_terminator<'a>(
        &'a self,
        block: BlockId,
        instr_to_idx: &'a BTreeMap<InstructionId, usize>,
    ) -> DisplayTerminator<'a, TI> {
        DisplayTerminator::new(self, instr_to_idx, block)
    }
}

pub(crate) struct Builder<'a, TI: TargetInstruction> {
    pub(crate) func: &'a mut Function<TI>,
    pub(crate) decls: &'a Declarations,
    current_block: Option<BlockId>,
}

impl<'a, TI: TargetInstruction> Builder<'a, TI> {
    pub(crate) fn new(module: &'a mut Module<TI>, func: FunctionIdx) -> Self {
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

    pub(crate) fn block_builder<'s>(&'s mut self) -> BlockBuilder<'s, TI, AppendInstrInserter<'s>> {
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

pub(crate) struct DisplayFunction<'a, TI: TargetInstruction> {
    module: &'a Module<TI>,
    func: FunctionIdx,
}

impl<'a, TI: TargetInstruction> DisplayFunction<'a, TI> {
    pub(super) fn new(module: &'a Module<TI>, func: FunctionIdx) -> Self {
        Self { module, func }
    }
}

impl<TI: TargetInstruction> Display for DisplayFunction<'_, TI> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let decl = &self.module.decls.funcs[self.func];
        let func = self.module.funcs.get(&self.func);

        if func.is_some() {
            write!(f, "define")?;
        } else {
            write!(f, "declare")?;
        }

        write!(f, " {}(", decl.name)?;

        let write_sig_values =
            |f: &mut std::fmt::Formatter<'_>, values: &[signature::Value]| -> std::fmt::Result {
                let mut iter = values.iter().peekable();

                while let Some(value) = iter.next() {
                    write!(f, "{} {}", value.ty, value.kind)?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                Ok(())
            };

        write_sig_values(f, &decl.sig.params)?;

        write!(f, ")")?;

        if !decl.sig.returns.is_empty() {
            write!(f, " -> ")?;
        }

        write_sig_values(f, &decl.sig.returns)?;

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
                    write!(f, "{}: {}", value.display(&instr_to_idx), value.ty())?;

                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }

                writeln!(f, "):")?;

                for instr in func.instrs_iter(block) {
                    writeln!(
                        f,
                        "\t{}",
                        func.display_instr(instr, &self.module.decls, &instr_to_idx)
                    )?;
                }

                writeln!(f, "\t{}", func.display_terminator(block, &instr_to_idx))?;
            }

            writeln!(f, "}}")?;
        }

        Ok(())
    }
}
