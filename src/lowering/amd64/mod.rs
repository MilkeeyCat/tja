mod instructions;

use crate::{
    hir::{
        self, Hir,
        op::BinOp,
        ty::{self, TyIdx},
    },
    mir::{self, Mir},
    targets::{Target, amd64},
};
use instructions as instr;
use std::collections::HashMap;

#[derive(Clone, Copy)]
enum OperandKind {
    Memory,
    Register,
    Immediate,
}

impl mir::Operand {
    fn kind(&self) -> OperandKind {
        match self {
            Self::Register(_, _) => OperandKind::Register,
            Self::Frame(_) | Self::Global(_) | Self::Function(_) | Self::Block(_) => {
                OperandKind::Memory
            }
            Self::Immediate(_) => OperandKind::Immediate,
        }
    }
}

struct FnLowering<'hir, 'a> {
    function: mir::Function<'hir>,
    ty_storage: &'a ty::Storage,
    locals: &'a [TyIdx],
    target: &'a dyn Target,
    local_to_operand: HashMap<hir::LocalIdx, Vec<mir::Operand>>,
}

impl<'hir, 'a> FnLowering<'hir, 'a> {
    fn new(
        name: &'hir str,
        ty_storage: &'a ty::Storage,
        locals: &'a [TyIdx],
        target: &'a dyn Target,
    ) -> Self {
        Self {
            function: mir::Function {
                name,
                next_vreg_idx: 0,
                vregs: HashMap::new(),
                next_stack_frame_idx: 0,
                stack_slots: HashMap::new(),
                precolored_vregs: HashMap::new(),
                blocks: Vec::new(),
            },
            ty_storage,
            locals,
            target,
            local_to_operand: HashMap::new(),
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
    ) -> Vec<mir::Operand> {
        match operand {
            hir::Operand::Local(idx) => self.local_to_operand[idx].clone(),
            hir::Operand::Const(c, _) => match c {
                hir::Const::Global(idx) => vec![mir::Operand::Global(*idx)],
                hir::Const::Function(idx) => vec![mir::Operand::Function(*idx)],
                hir::Const::Int(value) => vec![mir::Operand::Immediate(*value)],
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
                    let ty_size = self
                        .target
                        .abi()
                        .ty_size(self.ty_storage, self.locals[*out]);
                    let vreg = self.create_vreg_from_local(*out, gpr_by_size(ty_size));
                    let lhs = self.lower_operand(bb, lhs)[0].clone();
                    let lhs_kind = lhs.kind();

                    instr::mov(
                        bb,
                        vec![
                            mir::Operand::Register(
                                mir::Register::Virtual(vreg),
                                mir::RegisterRole::Def,
                            ),
                            lhs,
                        ],
                        OperandKind::Register,
                        lhs_kind,
                        ty_size,
                    );

                    let rhs = self.lower_operand(bb, rhs)[0].clone();
                    let rhs_kind = rhs.kind();

                    instr::add(
                        bb,
                        vec![
                            mir::Operand::Register(
                                mir::Register::Virtual(vreg),
                                mir::RegisterRole::Use,
                            ),
                            rhs,
                        ],
                        OperandKind::Register,
                        rhs_kind,
                        ty_size,
                    );
                }
                _ => unimplemented!(),
            },
            hir::Instruction::Alloca { ty, out } => {
                let idx = self.function.next_stack_frame_idx;
                self.function.next_stack_frame_idx += 1;

                self.function
                    .stack_slots
                    .insert(idx, self.target.abi().ty_size(self.ty_storage, *ty));
                self.local_to_operand
                    .insert(*out, vec![mir::Operand::Frame(idx)]);
            }
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
                    let operand = self.lower_operand(bb, condition)[0].clone();
                    let kind = operand.kind();

                    instr::test(bb, vec![operand.clone(), operand], kind, kind, 1);
                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Jcc as mir::Opcode,
                        operands: vec![
                            mir::Operand::Block(*iftrue),
                            mir::Operand::Immediate(amd64::Condition::NotEqual as u64),
                        ],
                    });
                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Jmp as mir::Opcode,
                        operands: vec![mir::Operand::Block(*iffalse)],
                    });
                }
                hir::Branch::Unconditional { block_idx } => {
                    bb.instructions.push(mir::Instruction {
                        opcode: amd64::Opcode::Jmp as mir::Opcode,
                        operands: vec![mir::Operand::Block(*block_idx)],
                    });
                }
            },
        }
    }

    fn create_vreg(&mut self, class: mir::RegisterClass) -> mir::VregIdx {
        let idx = self.function.next_vreg_idx;

        self.function.vregs.insert(idx, class);
        self.function.next_vreg_idx += 1;

        idx
    }

    fn create_vreg_from_local(
        &mut self,
        local_idx: hir::LocalIdx,
        class: mir::RegisterClass,
    ) -> mir::VregIdx {
        let idx = self.create_vreg(class);

        self.local_to_operand.insert(
            local_idx,
            vec![mir::Operand::Register(
                mir::Register::Virtual(idx),
                mir::RegisterRole::Use,
            )],
        );

        idx
    }
}

fn gpr_by_size(size: usize) -> mir::RegisterClass {
    let class = match size {
        1 => amd64::RegisterClass::Gpr8,
        2 => amd64::RegisterClass::Gpr16,
        4 => amd64::RegisterClass::Gpr32,
        8 => amd64::RegisterClass::Gpr64,
        _ => unreachable!(),
    };

    class as mir::RegisterClass
}

pub fn lower<'hir>(hir: &'hir Hir, target: &dyn Target) -> Mir<'hir> {
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
                    let mut lowering =
                        FnLowering::new(&func.name, &hir.ty_storage, &func.locals, target);

                    for bb in &func.blocks {
                        lowering.lower_basic_block(bb);
                    }

                    lowering.function
                })
                .collect(),
        })
        .collect())
}
