use super::{Operand, Register};
use crate::{
    dataflow::Liveness,
    macros::usize_wrapper,
    mir::{BasicBlock, BlockIdx},
    ty::TyIdx,
};

usize_wrapper! {VregIdx}
usize_wrapper! {FrameIdx}
usize_wrapper! {RegisterClass}

#[derive(Debug)]
pub struct Vreg {
    pub ty: TyIdx,
    pub class: Option<RegisterClass>,
}

#[derive(Default, Debug)]
pub struct VregInfo {
    vreg_info: Vec<Vreg>,
}

impl VregInfo {
    pub fn create_vreg(&mut self, ty: TyIdx) -> VregIdx {
        let idx = self.vreg_info.len();

        self.vreg_info.push(Vreg { ty, class: None });

        VregIdx(idx)
    }

    pub fn create_vreg_with_class(&mut self, ty: TyIdx, class: RegisterClass) -> VregIdx {
        let idx = self.vreg_info.len();

        self.vreg_info.push(Vreg {
            ty,
            class: Some(class),
        });

        VregIdx(idx)
    }

    pub fn get_vreg(&self, idx: VregIdx) -> &Vreg {
        &self.vreg_info[*idx]
    }

    pub fn set_class(&mut self, idx: VregIdx, class: RegisterClass) {
        self.vreg_info[*idx].class = Some(class);
    }
}

#[derive(Default, Debug)]
pub struct StackObject {
    pub size: usize,
}

#[derive(Default, Debug)]
pub struct FrameInfo {
    objects: Vec<StackObject>,
}

impl FrameInfo {
    pub fn create_stack_object(&mut self, size: usize) -> FrameIdx {
        let idx = self.objects.len();

        self.objects.push(StackObject { size: size });

        FrameIdx(idx)
    }

    pub fn get_stack_object(&mut self, idx: FrameIdx) -> &StackObject {
        &self.objects[*idx]
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
    pub blocks: Vec<BasicBlock>,
}

impl Function {
    pub fn liveness(&self) -> Vec<Liveness> {
        let defs_uses: Vec<_> = self.blocks.iter().map(|bb| bb.defs_uses()).collect();
        let mut liveness = vec![Liveness::default(); defs_uses.len()];

        loop {
            let mut done = true;

            // Some dude said that in reverse is better
            for (i, (defs_uses, block)) in defs_uses.iter().zip(&self.blocks).enumerate().rev() {
                // out[v] = ∪ in[w] where w ∈ succ(v)
                let live_out = block
                    .successors
                    .iter()
                    .map(|id| liveness[**id].ins.clone())
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

        new_basic_blocks.sort_by_key(|(instr_idx, _)| (**instr_idx));

        for (bb_idx, bb) in new_basic_blocks.into_iter().rev() {
            for bb in &mut func.blocks[*bb_idx..] {
                for instr in &mut bb.instructions {
                    for operand in &mut instr.operands {
                        if let Operand::Block(idx) = operand {
                            if **idx >= *bb_idx {
                                *idx = BlockIdx(**idx + 1);
                            }
                        }
                    }
                }

                bb.successors = std::mem::take(&mut bb.successors)
                    .into_iter()
                    .map(|idx| BlockIdx(*idx + 1))
                    .collect();
            }
            func.blocks.insert(*bb_idx, bb);
        }
    }
}
