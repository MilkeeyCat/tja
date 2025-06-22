use crate::{
    hir::{
        self, Const, Hir, LocalStorage,
        op::BinOp,
        ty::{self, Ty, TyIdx},
    },
    mir::{self, BlockIdx, GenericOpcode, Mir, Opcode, Register, RegisterRole},
    targets::Abi,
};
use std::collections::HashMap;

fn lower_module<'hir, A: Abi>(
    module: &'hir hir::Module,
    ty_storage: &ty::Storage,
    abi: &A,
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

struct FnLowering<'a, 'hir, A: Abi> {
    ty_storage: &'a ty::Storage,
    hir_function: &'a hir::Function,
    mir_function: mir::Function<'hir>,
    operand_to_vreg_indices: HashMap<hir::Operand, Vec<mir::VregIdx>>,
    ty_to_offsets: HashMap<TyIdx, Vec<usize>>,
    abi: &'a A,
    current_bb_idx: BlockIdx,
}

impl<'a, 'hir, A: Abi> FnLowering<'a, 'hir, A> {
    fn create_vreg(&mut self, ty: TyIdx) -> mir::VregIdx {
        let vreg_idx = self.mir_function.next_vreg_idx;

        self.mir_function.next_vreg_idx += 1;
        self.mir_function.vreg_types.insert(vreg_idx, ty);

        vreg_idx
    }

    fn get_or_create_vregs(&mut self, operand: hir::Operand) -> &[mir::VregIdx] {
        if self.operand_to_vreg_indices.contains_key(&operand) {
            return &self.operand_to_vreg_indices[&operand];
        }

        let ty = match operand {
            hir::Operand::Local(idx) => self.hir_function.locals[idx],
            hir::Operand::Const(_, ty) => ty,
        };

        let mut vreg_indices: Vec<mir::VregIdx> = Vec::new();
        let mut types = Vec::new();
        let mut offsets = Vec::new();

        self.lower_ty(ty, &mut types, &mut offsets, 0);

        match &operand {
            hir::Operand::Local(_) => {
                for ty in types {
                    vreg_indices.push(self.create_vreg(ty));
                }
            }
            hir::Operand::Const(c, ty) => match c {
                Const::Global(_global_idx) => unimplemented!(),
                Const::Function(_fn_idx) => unimplemented!(),
                Const::Int(value) => {
                    let vreg_idx = self.create_vreg(*ty);
                    let bb = &mut self.mir_function.blocks[self.current_bb_idx];

                    bb.instructions.push(mir::Instruction::new(
                        mir::GenericOpcode::Copy as mir::Opcode,
                        vec![
                            mir::Operand::Register(Register::Virtual(vreg_idx), RegisterRole::Def),
                            mir::Operand::Immediate(*value),
                        ],
                    ));
                    vreg_indices.push(vreg_idx);
                }
                Const::Aggregate(consts) => {
                    let tys = match self.ty_storage.get_ty(*ty) {
                        Ty::Struct(tys) => tys,
                        _ => unreachable!(),
                    };

                    assert_eq!(consts.len(), tys.len());

                    for (c, ty) in consts.iter().zip(tys) {
                        let tmp = self.get_or_create_vregs(hir::Operand::Const(c.clone(), *ty));

                        vreg_indices.extend_from_slice(tmp);
                    }
                }
            },
        }

        self.ty_to_offsets.entry(ty).or_insert(offsets);
        self.operand_to_vreg_indices
            .entry(operand)
            .or_insert(vreg_indices)
    }

    fn get_or_create_vreg(&mut self, operand: hir::Operand) -> mir::VregIdx {
        match self.get_or_create_vregs(operand) {
            [idx] => *idx,
            _ => unreachable!(),
        }
    }

    fn lower_ty(&self, ty: TyIdx, types: &mut Vec<TyIdx>, offsets: &mut Vec<usize>, offset: usize) {
        match self.ty_storage.get_ty(ty) {
            Ty::Struct(tys) => {
                for (i, &ty) in tys.iter().enumerate() {
                    let field_offset = self.abi.field_offset(self.ty_storage, &tys, i);
                    self.lower_ty(ty, types, offsets, offset + field_offset);
                }
            }
            _ => {
                types.push(ty);
                offsets.push(offset);
            }
        }
    }

    fn create_frame_idx(&mut self, ty: TyIdx) -> mir::StackFrameIdx {
        let idx = self.mir_function.next_stack_frame_idx;

        self.mir_function.next_stack_frame_idx += 1;
        self.mir_function
            .stack_slots
            .insert(idx, self.abi.ty_size(self.ty_storage, ty));

        idx
    }

    fn get_basic_block(&mut self) -> &mut mir::BasicBlock<'hir> {
        &mut self.mir_function.blocks[self.current_bb_idx]
    }

    fn lower_instruction(&mut self, instruction: &hir::Instruction) {
        match instruction {
            hir::Instruction::Binary {
                kind,
                lhs,
                rhs,
                out,
            } => {
                let def = self.get_or_create_vreg(hir::Operand::Local(*out));
                let lhs = self.get_or_create_vreg(lhs.clone());
                let rhs = self.get_or_create_vreg(rhs.clone());
                let opcode = match kind {
                    BinOp::Add => GenericOpcode::Add,
                    BinOp::Sub => GenericOpcode::Sub,
                    BinOp::Mul => GenericOpcode::Mul,
                    BinOp::SDiv => GenericOpcode::SDiv,
                    BinOp::UDiv => GenericOpcode::UDiv,
                };

                self.get_basic_block()
                    .instructions
                    .push(mir::Instruction::new(
                        opcode as Opcode,
                        vec![
                            mir::Operand::Register(Register::Virtual(def), RegisterRole::Def),
                            mir::Operand::Register(Register::Virtual(lhs), RegisterRole::Use),
                            mir::Operand::Register(Register::Virtual(rhs), RegisterRole::Use),
                        ],
                    ));
            }
            hir::Instruction::Alloca { ty, out } => {
                let def = self.get_or_create_vreg(hir::Operand::Local(*out));
                let frame_idx = self.create_frame_idx(*ty);

                self.get_basic_block()
                    .instructions
                    .push(mir::Instruction::new(
                        GenericOpcode::FrameIndex as Opcode,
                        vec![
                            mir::Operand::Register(Register::Virtual(def), RegisterRole::Def),
                            mir::Operand::Frame(frame_idx),
                        ],
                    ));
            }
            hir::Instruction::Store { ptr, value } => {
                let base = self.get_or_create_vreg(ptr.clone());
                let vregs = self.get_or_create_vregs(value.clone()).to_vec();
                let offsets = self.ty_to_offsets[&value.ty(self)].to_vec();

                for (vreg_idx, offset) in vregs.into_iter().zip(offsets) {
                    let ptr_add = self.create_vreg(self.ty_storage.ptr_ty);
                    let bb = self.get_basic_block();

                    bb.instructions.push(mir::Instruction::new(
                        GenericOpcode::PtrAdd as Opcode,
                        vec![
                            mir::Operand::Register(Register::Virtual(ptr_add), RegisterRole::Def),
                            mir::Operand::Register(Register::Virtual(base), RegisterRole::Use),
                            mir::Operand::Immediate(offset as u64),
                        ],
                    ));
                    bb.instructions.push(mir::Instruction::new(
                        GenericOpcode::Store as Opcode,
                        vec![
                            mir::Operand::Register(Register::Virtual(vreg_idx), RegisterRole::Use),
                            mir::Operand::Register(Register::Virtual(ptr_add), RegisterRole::Use),
                        ],
                    ));
                }
            }
            hir::Instruction::Load { ptr, out } => {
                let base = self.get_or_create_vreg(ptr.clone());
                let vregs = self.get_or_create_vregs(hir::Operand::Local(*out)).to_vec();
                let offsets = self.ty_to_offsets[&self.hir_function.locals[*out]].to_vec();

                for (vreg_idx, offset) in vregs.into_iter().zip(offsets) {
                    let ptr_add = self.create_vreg(self.ty_storage.ptr_ty);
                    let bb = self.get_basic_block();

                    bb.instructions.push(mir::Instruction::new(
                        GenericOpcode::PtrAdd as Opcode,
                        vec![
                            mir::Operand::Register(Register::Virtual(ptr_add), RegisterRole::Def),
                            mir::Operand::Register(Register::Virtual(base), RegisterRole::Use),
                            mir::Operand::Immediate(offset as u64),
                        ],
                    ));
                    bb.instructions.push(mir::Instruction::new(
                        GenericOpcode::Load as Opcode,
                        vec![
                            mir::Operand::Register(Register::Virtual(vreg_idx), RegisterRole::Def),
                            mir::Operand::Register(Register::Virtual(ptr_add), RegisterRole::Use),
                        ],
                    ));
                }
            }
            _ => todo!(),
        }
    }

    fn lower_terminator(&mut self, terminator: &hir::Terminator) {
        match terminator {
            hir::Terminator::Return(_value) => unimplemented!(),
            hir::Terminator::Br(branch) => match branch {
                hir::Branch::Conditional {
                    condition: _,
                    iftrue: _,
                    iffalse: _,
                } => unimplemented!(),
                hir::Branch::Unconditional { block_idx } => {
                    self.get_basic_block()
                        .instructions
                        .push(mir::Instruction::new(
                            GenericOpcode::Br as Opcode,
                            vec![mir::Operand::Block(*block_idx)],
                        ));
                }
            },
        }
    }
}

impl<A: Abi> LocalStorage for FnLowering<'_, '_, A> {
    fn get_local_ty(&self, idx: hir::LocalIdx) -> TyIdx {
        self.hir_function.locals[idx]
    }
}

fn lower_fn<'hir, A: Abi>(
    func: &'hir hir::Function,
    ty_storage: &ty::Storage,
    abi: &A,
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
            blocks: Vec::new(),
        },
        operand_to_vreg_indices: HashMap::new(),
        ty_to_offsets: HashMap::new(),
        abi,
        current_bb_idx: 0,
    };

    for (idx, bb) in func.blocks.iter().enumerate() {
        lowering.current_bb_idx = idx;
        lowering.mir_function.blocks.push(mir::BasicBlock {
            name: &bb.name,
            instructions: Vec::new(),
        });

        for instr in &bb.instructions {
            lowering.lower_instruction(instr);
        }

        lowering.lower_terminator(&bb.terminator);
    }

    lowering.mir_function
}

/// Lowers hir into generic mir
pub fn lower<'hir, A: Abi>(hir: &'hir Hir, abi: &A) -> Mir<'hir> {
    Mir(hir
        .modules
        .iter()
        .map(|module| lower_module(module, &hir.ty_storage, abi))
        .collect())
}
