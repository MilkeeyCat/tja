use std::collections::HashMap;

use super::{Operand, Register};
use crate::{
    dataflow::{DefsUses, Liveness},
    datastructures::vecset::VecSet,
    macros::usize_wrapper,
    mir::{
        BasicBlock, BasicBlockCursor, BasicBlockCursorMut, BlockIdx, Instruction,
        InstructionCursor, InstructionCursorMut, InstructionIdx, instruction,
    },
    ty::TyIdx,
};
use index_vec::{IndexVec, define_index_type};
use typed_generational_arena::StandardArena;

usize_wrapper! {RegisterClass}

define_index_type! {
    pub struct VregIdx = usize;
}

define_index_type! {
    pub struct FrameIdx = usize;
}

#[derive(Debug)]
pub struct Vreg {
    pub ty: TyIdx,
    pub class: Option<RegisterClass>,
}

#[derive(Default, Debug)]
pub struct VregInfo {
    vreg_info: IndexVec<VregIdx, Vreg>,
}

impl VregInfo {
    pub fn create_vreg(&mut self, ty: TyIdx) -> VregIdx {
        self.vreg_info.push(Vreg { ty, class: None })
    }

    pub fn create_vreg_with_class(&mut self, ty: TyIdx, class: RegisterClass) -> VregIdx {
        self.vreg_info.push(Vreg {
            ty,
            class: Some(class),
        })
    }

    pub fn get_vreg(&self, idx: VregIdx) -> &Vreg {
        &self.vreg_info[idx]
    }

    pub fn set_class(&mut self, idx: VregIdx, class: RegisterClass) {
        self.vreg_info[idx].class = Some(class);
    }
}

#[derive(Default, Debug)]
pub struct StackObject {
    pub size: usize,
}

#[derive(Default, Debug)]
pub struct FrameInfo {
    objects: IndexVec<FrameIdx, StackObject>,
}

impl FrameInfo {
    pub fn create_stack_object(&mut self, size: usize) -> FrameIdx {
        self.objects.push(StackObject { size: size })
    }

    pub fn get_stack_object(&mut self, idx: FrameIdx) -> &StackObject {
        &self.objects[idx]
    }

    pub fn objects_iter(&self) -> impl Iterator<Item = &StackObject> {
        self.objects.iter()
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub vreg_info: VregInfo,
    pub frame_info: FrameInfo,
    pub blocks: StandardArena<BasicBlock>,
    pub instructions: StandardArena<Instruction>,
    pub block_head: Option<BlockIdx>,
    pub block_tail: Option<BlockIdx>,
}

impl Function {
    pub fn new(name: String) -> Self {
        Self {
            name,
            vreg_info: VregInfo::default(),
            frame_info: FrameInfo::default(),
            blocks: StandardArena::new(),
            instructions: StandardArena::new(),
            block_head: None,
            block_tail: None,
        }
    }

    pub fn liveness(&self) -> HashMap<BlockIdx, Liveness> {
        let compute_defs_uses = |func: &Function, bb_idx: BlockIdx| {
            let mut defs_uses = DefsUses::default();
            let mut cursor = func.instr_cursor(bb_idx);

            while cursor.move_next().is_some() {
                let DefsUses { defs, uses } = cursor.current().unwrap().defs_uses();

                defs_uses.defs.extend(defs);
                defs_uses.uses.extend(uses);
            }

            defs_uses.uses = &defs_uses.uses - &defs_uses.defs;

            defs_uses
        };

        let defs_uses = {
            let mut defs_uses = HashMap::new();
            let mut cursor = self.block_cursor();

            while let Some(bb_idx) = cursor.move_next() {
                defs_uses.insert(bb_idx, compute_defs_uses(cursor.func, bb_idx));
            }

            defs_uses
        };
        let mut liveness: HashMap<BlockIdx, Liveness> = defs_uses
            .keys()
            .map(|idx| (*idx, Liveness::default()))
            .collect();

        loop {
            let mut done = true;
            let mut cursor = self.block_cursor();

            // Some dude said that in reverse is better
            while let Some(idx) = cursor.move_prev() {
                let block = cursor.current().unwrap();
                // out[v] = ∪ in[w] where w ∈ succ(v)
                let live_out = block
                    .successors
                    .iter()
                    .map(|idx| liveness[idx].ins.clone())
                    .reduce(|acc, el| acc.union(&el).cloned().collect())
                    .unwrap_or_default();
                // in[v] = use(v) ∪ (out[v] - def(v))
                let live_in = defs_uses[&idx]
                    .uses
                    .union(&(&liveness[&idx].outs - &defs_uses[&idx].defs))
                    .cloned()
                    .collect();

                if &liveness[&idx].ins != &live_in || &liveness[&idx].outs != &live_out {
                    done &= false;
                }

                liveness.get_mut(&idx).unwrap().ins = live_in;
                liveness.get_mut(&idx).unwrap().outs = live_out;
            }

            if done {
                break;
            }
        }

        liveness
    }

    pub fn registers(&self) -> VecSet<Register> {
        let mut registers = VecSet::new();

        for (_, instr) in &self.instructions {
            for operand in &instr.operands {
                if let Operand::Register(reg, _) = operand {
                    registers.insert(reg.clone());
                }
            }
        }

        registers
    }

    pub fn is_declaration(&self) -> bool {
        self.blocks.is_empty()
    }

    pub fn add_operand(&mut self, instr_idx: InstructionIdx, operand: Operand) {
        // TODO: store more info
        self.instructions[instr_idx].operands.push(operand);
    }

    pub fn add_implicit_def(&mut self, instr_idx: InstructionIdx, reg: Register) {
        // TODO: store more info
        self.instructions[instr_idx].implicit_defs.insert(reg);
    }

    pub fn create_block(&mut self, name: String) -> BlockIdx {
        self.blocks.insert(BasicBlock::new(name))
    }

    pub fn block_cursor(&self) -> BasicBlockCursor<'_> {
        BasicBlockCursor::new(self)
    }

    pub fn block_cursor_mut(&mut self) -> BasicBlockCursorMut<'_> {
        BasicBlockCursorMut::new(self)
    }

    pub fn instr_cursor(&self, idx: BlockIdx) -> InstructionCursor<'_> {
        InstructionCursor::new(self, idx)
    }

    pub fn instr_cursor_mut(&mut self, idx: BlockIdx) -> InstructionCursorMut<'_> {
        InstructionCursorMut::new(self, idx)
    }

    pub fn create_instr(&mut self) -> instruction::GenericBuilder<'_> {
        instruction::GenericBuilder::new(self)
    }
}
