use crate::{
    hir::{
        self, Hir,
        op::BinOp,
        ty::{self, TyIdx},
    },
    mir::{self, Mir},
    targets::{Target, amd64},
};
use std::collections::HashMap;

enum OperandKind {
    Memory,
    Register,
    Immediate,
}

impl mir::Operand {
    fn kind(&self) -> OperandKind {
        match self {
            Self::Vreg(_, _) | Self::Reg(_) => OperandKind::Register,
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
    local_to_operand: HashMap<hir::LocalIdx, mir::Operand>,
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
    ) -> mir::Operand {
        match operand {
            hir::Operand::Local(idx) => self.local_to_operand[idx].clone(),
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
                    let ty_size = self
                        .target
                        .abi()
                        .ty_size(self.ty_storage, self.locals[*out]);
                    let vreg = self.create_vreg_from_local(*out, gpr_by_size(ty_size));
                    let lhs = self.lower_operand(bb, lhs);
                    let lhs_kind = lhs.kind();

                    mov(
                        bb,
                        vec![mir::Operand::Vreg(vreg, mir::VregRole::Def), lhs],
                        OperandKind::Register,
                        lhs_kind,
                        ty_size,
                    );

                    let rhs = self.lower_operand(bb, rhs);
                    let rhs_kind = rhs.kind();

                    add(
                        bb,
                        vec![mir::Operand::Vreg(vreg, mir::VregRole::Use), rhs],
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
                self.local_to_operand.insert(*out, mir::Operand::Frame(idx));
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
                    let operand = self.lower_operand(bb, condition);

                    //bb.instructions.push(mir::Instruction {
                    //    opcode: amd64::Opcode::Test as usize,
                    //    operands: vec![operand],
                    //});
                    //bb.instructions.push(mir::Instruction {
                    //    opcode: amd64::Opcode::Jcc as usize,
                    //    operands: vec![
                    //        mir::Operand::Block(*iftrue),
                    //        mir::Operand::Immediate(amd64::Condition::NotEqual as u64),
                    //    ],
                    //});
                    //bb.instructions.push(mir::Instruction {
                    //    opcode: amd64::Opcode::Jmp as usize,
                    //    operands: vec![mir::Operand::Block(*iffalse)],
                    //});
                }
                hir::Branch::Unconditional { block_idx } => {
                    //bb.instructions.push(mir::Instruction {
                    //    opcode: amd64::Opcode::Jmp as usize,
                    //    operands: vec![mir::Operand::Block(*block_idx)],
                    //});
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

        self.local_to_operand
            .insert(local_idx, mir::Operand::Vreg(idx, mir::VregRole::Use));

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

fn mov(
    bb: &mut mir::BasicBlock,
    operands: Vec<mir::Operand>,
    dest: OperandKind,
    src: OperandKind,
    size: usize,
) {
    let opcode = match (dest, src, size) {
        (OperandKind::Register, OperandKind::Register, 1) => amd64::Opcode::Mov8rr,
        (OperandKind::Register, OperandKind::Memory, 1) => amd64::Opcode::Mov8rm,
        (OperandKind::Memory, OperandKind::Register, 1) => amd64::Opcode::Mov8mr,
        (OperandKind::Memory, OperandKind::Immediate, 1) => amd64::Opcode::Mov8mi,
        (OperandKind::Register, OperandKind::Immediate, 1) => amd64::Opcode::Mov8ri,

        (OperandKind::Register, OperandKind::Register, 2) => amd64::Opcode::Mov16rr,
        (OperandKind::Register, OperandKind::Memory, 2) => amd64::Opcode::Mov16rm,
        (OperandKind::Memory, OperandKind::Register, 2) => amd64::Opcode::Mov16mr,
        (OperandKind::Memory, OperandKind::Immediate, 2) => amd64::Opcode::Mov16mi,
        (OperandKind::Register, OperandKind::Immediate, 2) => amd64::Opcode::Mov16ri,

        (OperandKind::Register, OperandKind::Register, 4) => amd64::Opcode::Mov32rr,
        (OperandKind::Register, OperandKind::Memory, 4) => amd64::Opcode::Mov32rm,
        (OperandKind::Memory, OperandKind::Register, 4) => amd64::Opcode::Mov32mr,
        (OperandKind::Memory, OperandKind::Immediate, 4) => amd64::Opcode::Mov32mi,
        (OperandKind::Register, OperandKind::Immediate, 4) => amd64::Opcode::Mov32ri,

        (OperandKind::Register, OperandKind::Register, 8) => amd64::Opcode::Mov64rr,
        (OperandKind::Register, OperandKind::Memory, 8) => amd64::Opcode::Mov64rm,
        (OperandKind::Memory, OperandKind::Register, 8) => amd64::Opcode::Mov64mr,
        (OperandKind::Memory, OperandKind::Immediate, 8) => amd64::Opcode::Mov64mi,
        (OperandKind::Register, OperandKind::Immediate, 8) => amd64::Opcode::Mov64ri,

        _ => unreachable!(),
    };

    bb.instructions.push(mir::Instruction {
        opcode: opcode as mir::Opcode,
        operands,
    });
}

fn add(
    bb: &mut mir::BasicBlock,
    operands: Vec<mir::Operand>,
    dest: OperandKind,
    src: OperandKind,
    size: usize,
) {
    let opcode = match (dest, src, size) {
        (OperandKind::Register, OperandKind::Register, 1) => amd64::Opcode::Add8rr,
        (OperandKind::Register, OperandKind::Memory, 1) => amd64::Opcode::Add8rm,
        (OperandKind::Memory, OperandKind::Register, 1) => amd64::Opcode::Add8mr,
        (OperandKind::Memory, OperandKind::Immediate, 1) => amd64::Opcode::Add8mi,
        (OperandKind::Register, OperandKind::Immediate, 1) => amd64::Opcode::Add8ri,

        (OperandKind::Register, OperandKind::Register, 2) => amd64::Opcode::Add16rr,
        (OperandKind::Register, OperandKind::Memory, 2) => amd64::Opcode::Add16rm,
        (OperandKind::Memory, OperandKind::Register, 2) => amd64::Opcode::Add16mr,
        (OperandKind::Memory, OperandKind::Immediate, 2) => amd64::Opcode::Add16mi,
        (OperandKind::Register, OperandKind::Immediate, 2) => amd64::Opcode::Add16ri,

        (OperandKind::Register, OperandKind::Register, 4) => amd64::Opcode::Add32rr,
        (OperandKind::Register, OperandKind::Memory, 4) => amd64::Opcode::Add32rm,
        (OperandKind::Memory, OperandKind::Register, 4) => amd64::Opcode::Add32mr,
        (OperandKind::Memory, OperandKind::Immediate, 4) => amd64::Opcode::Add32mi,
        (OperandKind::Register, OperandKind::Immediate, 4) => amd64::Opcode::Add32ri,

        (OperandKind::Register, OperandKind::Register, 8) => amd64::Opcode::Add64rr,
        (OperandKind::Register, OperandKind::Memory, 8) => amd64::Opcode::Add64rm,
        (OperandKind::Memory, OperandKind::Register, 8) => amd64::Opcode::Add64mr,
        (OperandKind::Memory, OperandKind::Immediate, 8) => amd64::Opcode::Add64mi,
        (OperandKind::Register, OperandKind::Immediate, 8) => amd64::Opcode::Add64ri,

        _ => unreachable!(),
    };

    bb.instructions.push(mir::Instruction {
        opcode: opcode as mir::Opcode,
        operands,
    });
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
