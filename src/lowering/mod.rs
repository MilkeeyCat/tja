use crate::{
    hir::{
        self, Hir,
        op::BinOp,
        ty::{self, TyIdx},
    },
    mir::{self, GenericOpcode, Mir, Opcode, Register, RegisterRole},
    targets::Abi,
};
use std::collections::HashMap;

fn lower_module<'hir>(
    module: &'hir hir::Module,
    ty_storage: &ty::Storage,
    abi: &dyn Abi,
) -> mir::Module<'hir> {
    mir::Module {
        name: &module.name,
        globals: &module.globals,
        functions: module
            .functions
            .iter()
            .map(|func| lower_fn(func, ty_storage, abi))
            .collect(),
    }
}

struct FnLowering<'a, 'hir> {
    ty_storage: &'a ty::Storage,
    hir_function: &'a hir::Function,
    mir_function: mir::Function<'hir>,
    local_to_vreg_idx: HashMap<hir::LocalIdx, mir::VregIdx>,
    abi: &'a dyn Abi,
}

impl<'a, 'hir> FnLowering<'a, 'hir> {
    fn lower_operand(&self, operand: &hir::Operand) -> mir::Operand {
        match operand {
            hir::Operand::Local(idx) => mir::Operand::Register(
                Register::Virtual(self.local_to_vreg_idx[idx]),
                RegisterRole::Use,
            ),
            hir::Operand::Const(c, _) => match c {
                hir::Const::Global(idx) => mir::Operand::Global(*idx),
                hir::Const::Function(idx) => mir::Operand::Function(*idx),
                hir::Const::Int(value) => mir::Operand::Immediate(*value),
                hir::Const::Aggregate(_) => unimplemented!(),
            },
        }
    }

    fn create_def(&mut self, idx: hir::LocalIdx) -> mir::Operand {
        let vreg_idx = self.mir_function.next_vreg_idx;

        self.mir_function.next_vreg_idx += 1;
        self.mir_function
            .vreg_types
            .insert(vreg_idx, self.hir_function.locals[idx]);
        self.local_to_vreg_idx.insert(idx, vreg_idx);

        mir::Operand::Register(Register::Virtual(vreg_idx), RegisterRole::Def)
    }

    fn create_frame_idx(&mut self, ty: TyIdx) -> mir::StackFrameIdx {
        let idx = self.mir_function.next_stack_frame_idx;

        self.mir_function.next_stack_frame_idx += 1;
        self.mir_function
            .stack_slots
            .insert(idx, self.abi.ty_size(self.ty_storage, ty));

        idx
    }

    fn lower_instruction(
        &mut self,
        basic_block: &mut mir::BasicBlock<'hir>,
        instruction: &hir::Instruction,
    ) {
        match instruction {
            hir::Instruction::Binary {
                kind,
                lhs,
                rhs,
                out,
            } => {
                let def = self.create_def(*out);
                let lhs = self.lower_operand(lhs);
                let rhs = self.lower_operand(rhs);
                let opcode = match kind {
                    BinOp::Add => GenericOpcode::Add,
                    BinOp::Sub => GenericOpcode::Sub,
                    BinOp::Mul => GenericOpcode::Mul,
                    BinOp::SDiv => GenericOpcode::SDiv,
                    BinOp::UDiv => GenericOpcode::UDiv,
                };

                basic_block.instructions.push(mir::Instruction {
                    opcode: opcode as Opcode,
                    operands: vec![def, lhs, rhs],
                });
            }
            hir::Instruction::Alloca { ty, out } => {
                let def = self.create_def(*out);
                let frame_idx = self.create_frame_idx(*ty);

                basic_block.instructions.push(mir::Instruction {
                    opcode: GenericOpcode::FrameIndex as Opcode,
                    operands: vec![def, mir::Operand::Frame(frame_idx)],
                });
            }
            _ => todo!(),
        }
    }
}

fn lower_fn<'hir>(
    func: &'hir hir::Function,
    ty_storage: &ty::Storage,
    abi: &dyn Abi,
) -> mir::Function<'hir> {
    let mut lowering = FnLowering {
        ty_storage,
        hir_function: func,
        mir_function: mir::Function {
            name: &func.name,
            next_vreg_idx: 0,
            vreg_classes: HashMap::new(),
            vreg_types: HashMap::new(),
            next_stack_frame_idx: 0,
            stack_slots: HashMap::new(),
            precolored_vregs: HashMap::new(),
            blocks: Vec::new(),
        },
        local_to_vreg_idx: HashMap::new(),
        abi,
    };

    for bb in &func.blocks {
        let mut lowered_bb = mir::BasicBlock {
            name: &bb.name,
            instructions: Vec::new(),
        };

        for instr in &bb.instructions {
            lowering.lower_instruction(&mut lowered_bb, instr);
        }

        lowering.mir_function.blocks.push(lowered_bb);
    }

    lowering.mir_function
}

/// Lowers hir into generic mir
pub fn lower<'hir>(hir: &'hir Hir, abi: &dyn Abi) -> Mir<'hir> {
    Mir(hir
        .modules
        .iter()
        .map(|module| lower_module(module, &hir.ty_storage, abi))
        .collect())
}
