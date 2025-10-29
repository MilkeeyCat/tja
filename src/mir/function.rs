use super::{Operand, Register};
use crate::{
    dataflow::Liveness,
    macros::usize_wrapper,
    mir::{BasicBlock, BlockIdx, Instruction, InstructionIdx},
    ty::TyIdx,
};
use index_vec::{IndexVec, define_index_type, index_vec};

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
    pub blocks: IndexVec<BlockIdx, BasicBlock>,
}

impl Function {
    pub fn liveness(&self) -> IndexVec<BlockIdx, Liveness> {
        let defs_uses: Vec<_> = self.blocks.iter().map(|bb| bb.defs_uses()).collect();
        let mut liveness = index_vec![Liveness::default(); defs_uses.len()];

        loop {
            let mut done = true;

            // Some dude said that in reverse is better
            for (i, (defs_uses, block)) in defs_uses.iter().zip(&self.blocks).enumerate().rev() {
                // out[v] = ∪ in[w] where w ∈ succ(v)
                let live_out = block
                    .successors
                    .iter()
                    .map(|id| liveness[*id].ins.clone())
                    .reduce(|acc, el| acc.union(&el).cloned().collect())
                    .unwrap_or_default();
                // in[v] = use(v) ∪ (out[v] - def(v))
                let live_in = defs_uses
                    .uses
                    .union(&(&liveness[i].outs - &defs_uses.defs))
                    .cloned()
                    .collect();

                if &liveness[i].ins != &live_in || &liveness[i].outs != &live_out {
                    done &= false;
                }

                liveness[i].ins = live_in;
                liveness[i].outs = live_out;
            }

            if done {
                break;
            }
        }

        liveness
    }

    pub fn registers(&self) -> Vec<Register> {
        let mut registers = Vec::new();

        for bb in &self.blocks {
            for instr in &bb.instructions {
                for operand in &instr.operands {
                    if let Operand::Register(reg, _) = operand {
                        if !registers.contains(reg) {
                            registers.push(reg.clone());
                        }
                    }
                }
            }
        }

        registers
    }

    pub fn is_declaration(&self) -> bool {
        self.blocks.is_empty()
    }
}

pub struct FunctionPatch {
    new_basic_blocks: Vec<(BlockIdx, BasicBlock)>,
}

impl FunctionPatch {
    pub fn new() -> Self {
        Self {
            new_basic_blocks: Vec::new(),
        }
    }

    pub fn add_basic_block(&mut self, bb_idx: BlockIdx, bb: BasicBlock) {
        self.new_basic_blocks.push((bb_idx, bb));
    }

    pub fn apply(self, func: &mut Function) {
        let mut new_basic_blocks = self.new_basic_blocks;

        new_basic_blocks.sort_by_key(|(instr_idx, _)| *instr_idx);

        for (bb_idx, bb) in new_basic_blocks.into_iter().rev() {
            for bb in &mut func.blocks[bb_idx..] {
                for instr in &mut bb.instructions {
                    for operand in &mut instr.operands {
                        if let Operand::Block(idx) = operand {
                            if *idx >= bb_idx {
                                *idx = BlockIdx::new(idx.raw() + 1);
                            }
                        }
                    }
                }

                bb.successors = std::mem::take(&mut bb.successors)
                    .into_iter()
                    .map(|idx| BlockIdx::new(idx.raw() + 1))
                    .collect();
            }
            func.blocks.insert(bb_idx, bb);
        }
    }
}

pub struct Cursor<'a> {
    pub func: &'a mut Function,

    bb_idx: Option<BlockIdx>,
    instr_idx: Option<InstructionIdx>,
}

impl<'a> Cursor<'a> {
    pub fn new(func: &'a mut Function) -> Self {
        Self {
            func,
            bb_idx: None,
            instr_idx: None,
        }
    }

    pub fn get_bb_idx(&self) -> Option<BlockIdx> {
        self.bb_idx
    }

    pub fn get_instr_idx(&self) -> Option<InstructionIdx> {
        self.instr_idx
    }

    pub fn set_bb_idx(&mut self, idx: Option<BlockIdx>) {
        self.bb_idx = idx;
    }

    pub fn set_instr_idx(&mut self, idx: Option<InstructionIdx>) {
        self.instr_idx = idx;
    }

    pub fn get_prev_bb_idx(&self) -> Option<BlockIdx> {
        match self.bb_idx {
            Some(idx) => (idx > BlockIdx::from(0)).then(|| idx - 1),
            None => {
                let len = self.func.blocks.len();

                (len > 0).then(|| BlockIdx::from(len - 1))
            }
        }
    }

    pub fn get_prev_instr_idx(&self) -> Option<InstructionIdx> {
        match self.instr_idx {
            Some(idx) => (idx > InstructionIdx::from(0)).then(|| idx - 1),
            None => {
                let len = self.get_bb().instructions.len();

                (len > 0).then(|| InstructionIdx::from(len - 1))
            }
        }
    }

    pub fn prev_bb_idx(&mut self) -> Option<BlockIdx> {
        self.bb_idx = self.get_prev_bb_idx();

        self.bb_idx
    }

    pub fn prev_instr_idx(&mut self) -> Option<InstructionIdx> {
        self.instr_idx = self.get_prev_instr_idx();

        self.instr_idx
    }

    pub fn get_next_bb_idx(&self) -> Option<BlockIdx> {
        let idx = self.bb_idx.map(|idx| idx + 1).unwrap_or(0.into());

        self.func.blocks.get(idx).map(|_| idx)
    }

    pub fn get_next_instr_idx(&self) -> Option<InstructionIdx> {
        let idx = self.instr_idx.map(|idx| idx + 1).unwrap_or(0.into());

        self.get_bb().instructions.get(idx).map(|_| idx)
    }

    pub fn next_bb_idx(&mut self) -> Option<BlockIdx> {
        self.bb_idx = self.get_next_bb_idx();

        self.bb_idx
    }

    pub fn next_instr_idx(&mut self) -> Option<InstructionIdx> {
        self.instr_idx = self.get_next_instr_idx();

        self.instr_idx
    }

    pub fn get_bb(&self) -> &BasicBlock {
        &self.func.blocks[self.bb_idx.unwrap()]
    }

    pub fn get_instr(&self) -> &Instruction {
        &self.get_bb().instructions[self.instr_idx.unwrap()]
    }

    pub fn get_bb_mut(&mut self) -> &mut BasicBlock {
        &mut self.func.blocks[self.bb_idx.unwrap()]
    }

    pub fn get_instr_mut(&mut self) -> &mut Instruction {
        let idx = self.instr_idx.unwrap();

        &mut self.get_bb_mut().instructions[idx]
    }

    pub fn create_bb_before(&mut self, name: String) -> BlockIdx {
        let idx = self.bb_idx.unwrap_or(0.into());

        self.func.blocks.insert(idx, BasicBlock::new(name));
        self.bb_idx = Some(idx);

        idx
    }

    pub fn create_bb_after(&mut self, name: String) -> BlockIdx {
        let idx = self.get_next_bb_idx().unwrap_or(self.func.blocks.len_idx());

        self.func.blocks.insert(idx, BasicBlock::new(name));
        self.bb_idx = Some(idx);

        idx
    }
}
