use crate::{
    hir::{self, Hir, op::BinOp},
    mir::{self, Mir},
    targets::amd64,
};
use std::collections::HashMap;

struct FnLowering<'hir> {
    function: mir::Function<'hir>,
    next_vreg_idx: mir::VregIdx,
    local_to_vreg: HashMap<hir::LocalIdx, mir::VregIdx>,
}

impl<'hir> FnLowering<'hir> {
    fn new(name: &'hir str) -> Self {
        Self {
            function: mir::Function {
                name,
                vregs: HashMap::new(),
                precolored_vregs: HashMap::new(),
                blocks: Vec::new(),
            },
            next_vreg_idx: 0,
            local_to_vreg: HashMap::new(),
        }
    }

    fn lower_basic_block(&mut self, hir_bb: &'hir hir::BasicBlock) {
        let mut bb = mir::BasicBlock {
            name: &hir_bb.name,
            instructions: Vec::new(),
        };

        for instr in &hir_bb.instructions {
            self.lower_instruction(&mut bb, instr);
        }

        self.lower_terminator(&mut bb, &hir_bb.terminator);
        self.function.blocks.push(bb);
    }

    fn lower_operand(
        &mut self,
        _bb: &mut mir::BasicBlock<'_>,
        operand: &hir::Operand,
    ) -> mir::Operand {
        match operand {
            hir::Operand::Local(idx) => {
                mir::Operand::Vreg(self.local_to_vreg[idx], mir::VregRole::Use)
            }
            hir::Operand::Const(c, _) => match c {
                hir::Const::Global(idx) => mir::Operand::Global(*idx),
                hir::Const::Function(idx) => mir::Operand::Function(*idx),
                hir::Const::Int(value) => mir::Operand::Immediate(*value),
                hir::Const::Aggregate(_) => unimplemented!(),
            },
        }
    }

    fn lower_instruction(&mut self, bb: &mut mir::BasicBlock<'hir>, instr: &hir::Instruction) {
        match instr {
            hir::Instruction::Binary {
                kind,
                lhs,
                rhs,
                out,
            } => match kind {
                BinOp::Add => {
                    let vreg =
                        self.create_vreg_from_local(*out, amd64::RegisterClass::Gpr64 as usize);
                    let lhs = self.lower_operand(bb, lhs);

                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Mov as usize,
                        operands: vec![mir::Operand::Vreg(vreg, mir::VregRole::Def), lhs],
                    });

                    let rhs = self.lower_operand(bb, rhs);

                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Add as usize,
                        operands: vec![mir::Operand::Vreg(vreg, mir::VregRole::Use), rhs],
                    });
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn lower_terminator(&mut self, bb: &mut mir::BasicBlock<'hir>, terminator: &hir::Terminator) {
        match terminator {
            hir::Terminator::Return(operand) => {
                if let Some(_operand) = operand {
                    unimplemented!()
                }
            }
            hir::Terminator::Br(branch) => match branch {
                hir::Branch::Conditional {
                    condition,
                    iftrue,
                    iffalse,
                } => {
                    let operand = self.lower_operand(bb, condition);

                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Test as usize,
                        operands: vec![operand],
                    });
                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Jcc as usize,
                        operands: vec![
                            mir::Operand::Block(*iftrue),
                            mir::Operand::Immediate(amd64::Condition::NotEqual as u64),
                        ],
                    });
                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Jmp as usize,
                        operands: vec![mir::Operand::Block(*iffalse)],
                    });
                }
                hir::Branch::Unconditional { block_idx } => {
                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Jmp as usize,
                        operands: vec![mir::Operand::Block(*block_idx)],
                    });
                }
            },
        }
    }

    fn create_vreg(&mut self, class: mir::RegisterClass) -> mir::VregIdx {
        let idx = self.next_vreg_idx;

        self.function.vregs.insert(idx, class);
        self.next_vreg_idx += 1;

        idx
    }

    fn create_vreg_from_local(
        &mut self,
        local_idx: hir::LocalIdx,
        class: mir::RegisterClass,
    ) -> mir::VregIdx {
        let idx = self.create_vreg(class);

        self.local_to_vreg.insert(local_idx, idx);

        idx
    }
}

pub fn lower<'hir>(hir: &'hir Hir) -> Mir<'hir> {
    Mir(hir
        .modules
        .iter()
        .map(|module| mir::Module {
            name: &module.name,
            globals: &module.globals,
            functions: module
                .functions
                .iter()
                .map(|func| {
                    let mut lowering = FnLowering::new(&func.name);

                    for bb in &func.blocks {
                        lowering.lower_basic_block(bb);
                    }

                    lowering.function
                })
                .collect(),
        })
        .collect())
}
