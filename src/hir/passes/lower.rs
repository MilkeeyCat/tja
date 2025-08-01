use crate::{
    hir::{
        self, Const, LocalStorage,
        op::BinOp,
        ty::{self, Ty, TyIdx},
    },
    mir::{self, BlockIdx, GenericOpcode, Opcode, Register, RegisterRole},
    pass::{Context, Pass},
    targets::{Abi, CallingConvention, Target},
};
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct Lower;

impl<'a, T: Target> Pass<'a, hir::Function, T> for Lower {
    fn run(&self, function: &mut hir::Function, ctx: &mut Context<'a, T>) {
        let mut lowering = FnLowering {
            ty_storage: ctx.ty_storage,
            hir_function: function,
            mir_function: mir::Function {
                name: function.name.clone(),
                next_vreg_idx: 0,
                vreg_classes: HashMap::new(),
                vreg_types: HashMap::new(),
                next_stack_frame_idx: 0,
                stack_slots: HashMap::new(),
                blocks: vec![mir::BasicBlock {
                    name: "entry".into(),
                    instructions: vec![],
                    successors: HashSet::from([1]),
                }],
            },
            operand_to_vreg_indices: HashMap::new(),
            ty_to_offsets: HashMap::new(),
            abi: ctx.target.abi(),
            current_bb_idx: 0,
        };

        let vreg_indices = (0..lowering.hir_function.params_count)
            .into_iter()
            .map(|local_idx| {
                lowering
                    .get_or_create_vregs(hir::Operand::Local(local_idx))
                    .to_vec()
            })
            .collect();
        let tys = lowering.hir_function.locals[..lowering.hir_function.params_count].to_vec();
        let ret_ty = lowering.hir_function.ret_ty;

        lowering
            .abi
            .calling_convention()
            .lower_params(&mut lowering, vreg_indices, tys, ret_ty);
        lowering
            .get_basic_block()
            .instructions
            .push(mir::Instruction::new(
                GenericOpcode::Br as mir::Opcode,
                vec![mir::Operand::Block(1)],
            ));

        for (idx, bb) in function.blocks.iter().enumerate() {
            lowering.current_bb_idx = idx + 1;
            lowering.mir_function.blocks.push(mir::BasicBlock {
                name: bb.name.clone(),
                instructions: Vec::new(),
                successors: HashSet::new(),
            });

            for instr in &bb.instructions {
                lowering.lower_instruction(instr);
            }

            lowering.lower_terminator(&bb.terminator);
        }

        for bb in lowering.mir_function.blocks.iter_mut().skip(1) {
            bb.successors = std::mem::take(&mut bb.successors)
                .into_iter()
                .map(|idx| idx + 1)
                .collect();

            for instr in &mut bb.instructions {
                for operand in &mut instr.operands {
                    if let mir::Operand::Block(idx) = operand {
                        *idx += 1;
                    }
                }
            }
        }

        ctx.mir_function = Some(lowering.mir_function);
    }
}

pub struct FnLowering<'a, A: Abi> {
    pub ty_storage: &'a ty::Storage,
    hir_function: &'a hir::Function,
    pub mir_function: mir::Function,
    operand_to_vreg_indices: HashMap<hir::Operand, Vec<mir::VregIdx>>,
    pub ty_to_offsets: HashMap<TyIdx, Vec<usize>>,
    pub abi: &'a A,
    current_bb_idx: BlockIdx,
}

impl<'a, A: Abi> FnLowering<'a, A> {
    pub fn create_vreg(&mut self, ty: TyIdx) -> mir::VregIdx {
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
                Const::Global(idx) => {
                    let vreg_idx = self.create_vreg(*ty);

                    self.get_basic_block()
                        .instructions
                        .push(mir::Instruction::new(
                            mir::GenericOpcode::GlobalValue as mir::Opcode,
                            vec![
                                mir::Operand::Register(
                                    Register::Virtual(vreg_idx),
                                    RegisterRole::Def,
                                ),
                                mir::Operand::Global(*idx),
                            ],
                        ));
                    vreg_indices.push(vreg_idx);
                }
                Const::Function(idx) => {
                    let vreg_idx = self.create_vreg(*ty);

                    self.get_basic_block()
                        .instructions
                        .push(mir::Instruction::new(
                            mir::GenericOpcode::GlobalValue as mir::Opcode,
                            vec![
                                mir::Operand::Register(
                                    Register::Virtual(vreg_idx),
                                    RegisterRole::Def,
                                ),
                                mir::Operand::Function(*idx),
                            ],
                        ));
                    vreg_indices.push(vreg_idx);
                }
                Const::Int(value) => {
                    let vreg_idx = self.create_vreg(*ty);

                    self.get_basic_block()
                        .instructions
                        .push(mir::Instruction::new(
                            mir::GenericOpcode::Copy as mir::Opcode,
                            vec![
                                mir::Operand::Register(
                                    Register::Virtual(vreg_idx),
                                    RegisterRole::Def,
                                ),
                                mir::Operand::Immediate(*value),
                            ],
                        ));
                    vreg_indices.push(vreg_idx);
                }
                Const::Aggregate(consts) => {
                    match self.ty_storage.get_ty(*ty) {
                        Ty::Struct(tys) => {
                            assert_eq!(consts.len(), tys.len());

                            for (c, ty) in consts.iter().zip(tys) {
                                let tmp =
                                    self.get_or_create_vregs(hir::Operand::Const(c.clone(), *ty));

                                vreg_indices.extend_from_slice(tmp);
                            }
                        }
                        Ty::Array { ty, len } => {
                            assert_eq!(consts.len(), *len);

                            for c in consts.iter() {
                                let tmp =
                                    self.get_or_create_vregs(hir::Operand::Const(c.clone(), *ty));

                                vreg_indices.extend_from_slice(tmp);
                            }
                        }
                        _ => unreachable!(),
                    };
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
            Ty::Array { ty, len } => {
                for i in 0..*len {
                    self.lower_ty(
                        *ty,
                        types,
                        offsets,
                        offset + (self.abi.ty_size(self.ty_storage, *ty) * i),
                    );
                }
            }
            _ => {
                types.push(ty);
                offsets.push(offset);
            }
        }
    }

    pub fn create_frame_idx(&mut self, ty: TyIdx) -> mir::StackFrameIdx {
        let idx = self.mir_function.next_stack_frame_idx;

        self.mir_function.next_stack_frame_idx += 1;
        self.mir_function
            .stack_slots
            .insert(idx, self.abi.ty_size(self.ty_storage, ty));

        idx
    }

    pub fn get_basic_block(&mut self) -> &mut mir::BasicBlock {
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
                    let ptr_add = self.ptr_add(
                        mir::Operand::Register(Register::Virtual(base), RegisterRole::Use),
                        mir::Operand::Immediate(offset as u64),
                    );
                    let bb = self.get_basic_block();

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
                    let ptr_add = self.ptr_add(
                        mir::Operand::Register(Register::Virtual(base), RegisterRole::Use),
                        mir::Operand::Immediate(offset as u64),
                    );
                    let bb = self.get_basic_block();

                    bb.instructions.push(mir::Instruction::new(
                        GenericOpcode::Load as Opcode,
                        vec![
                            mir::Operand::Register(Register::Virtual(vreg_idx), RegisterRole::Def),
                            mir::Operand::Register(Register::Virtual(ptr_add), RegisterRole::Use),
                        ],
                    ));
                }
            }
            hir::Instruction::GetElementPtr {
                ptr,
                ptr_ty,
                indices,
                out,
            } => {
                let mut ty_idx = *ptr_ty;
                let mut offset = 0;
                let mut base = self.get_or_create_vreg(ptr.clone());

                for (i, idx) in indices.iter().enumerate() {
                    match self.ty_storage.get_ty(ty_idx) {
                        Ty::Struct(fields) if i > 0 => {
                            let idx = match idx {
                                hir::Operand::Const(Const::Int(idx), _) => *idx as usize,
                                _ => unreachable!(),
                            };

                            ty_idx = fields[idx];
                            offset += self.abi.field_offset(self.ty_storage, fields, idx);
                        }
                        ty => {
                            let size = match ty {
                                Ty::Array { ty, .. } if i > 0 => {
                                    self.abi.ty_size(self.ty_storage, *ty)
                                }
                                _ => self.abi.ty_size(self.ty_storage, ty_idx),
                            };

                            match idx {
                                hir::Operand::Local(_) => {
                                    if offset != 0 {
                                        base = self.ptr_add(
                                            mir::Operand::Register(
                                                Register::Virtual(base),
                                                RegisterRole::Use,
                                            ),
                                            mir::Operand::Immediate(offset as u64),
                                        );
                                        offset = 0;
                                    }

                                    let vreg_idx = if size == 1 {
                                        self.get_or_create_vreg(idx.clone())
                                    } else {
                                        let def_vreg_idx = self.create_vreg(self.ty_storage.ptr_ty);
                                        let lhs_vreg_idx = self.get_or_create_vreg(idx.clone());

                                        self.get_basic_block().instructions.push(
                                            mir::Instruction::new(
                                                GenericOpcode::Mul as Opcode,
                                                vec![
                                                    mir::Operand::Register(
                                                        Register::Virtual(def_vreg_idx),
                                                        RegisterRole::Def,
                                                    ),
                                                    mir::Operand::Register(
                                                        Register::Virtual(lhs_vreg_idx),
                                                        RegisterRole::Use,
                                                    ),
                                                    mir::Operand::Immediate(size as u64),
                                                ],
                                            ),
                                        );

                                        def_vreg_idx
                                    };

                                    base = self.ptr_add(
                                        mir::Operand::Register(
                                            Register::Virtual(base),
                                            RegisterRole::Use,
                                        ),
                                        mir::Operand::Register(
                                            Register::Virtual(vreg_idx),
                                            RegisterRole::Use,
                                        ),
                                    );
                                }
                                hir::Operand::Const(hir::Const::Int(value), _) => {
                                    offset += size * (*value as usize);
                                }
                                _ => unreachable!(),
                            }
                        }
                    }
                }

                if offset != 0 {
                    base = self.ptr_add(
                        mir::Operand::Register(Register::Virtual(base), RegisterRole::Use),
                        mir::Operand::Immediate(offset as u64),
                    );
                }

                let def_vreg_idx = self.get_or_create_vreg(hir::Operand::Local(*out));

                self.get_basic_block()
                    .instructions
                    .push(mir::Instruction::new(
                        GenericOpcode::Copy as Opcode,
                        vec![
                            mir::Operand::Register(
                                Register::Virtual(def_vreg_idx),
                                RegisterRole::Def,
                            ),
                            mir::Operand::Register(Register::Virtual(base), RegisterRole::Use),
                        ],
                    ));
            }
            hir::Instruction::Call {
                operand,
                arguments,
                out,
            } => {
                let callee_vreg_idx = self.get_or_create_vreg(operand.clone());
                let arg_vreg_indices = arguments
                    .iter()
                    .map(|operand| self.get_or_create_vregs(operand.clone()).to_vec())
                    .collect();
                let arg_tys = arguments.iter().map(|operand| operand.ty(self)).collect();
                let ret = out.map(|out| {
                    (
                        self.get_or_create_vregs(hir::Operand::Local(out)).to_vec(),
                        self.hir_function.locals[out],
                    )
                });

                self.abi.calling_convention().lower_call(
                    self,
                    callee_vreg_idx,
                    arg_vreg_indices,
                    arg_tys,
                    ret,
                );
            }
            _ => todo!(),
        }
    }

    fn lower_terminator(&mut self, terminator: &hir::Terminator) {
        match terminator {
            hir::Terminator::Return(value) => {
                if let Some(value) = value {
                    let vreg_indices = self.get_or_create_vregs(value.clone()).to_vec();

                    self.abi
                        .calling_convention()
                        .lower_ret(self, vreg_indices, value.ty(self));
                }

                self.get_basic_block()
                    .instructions
                    .push(mir::Instruction::new(
                        GenericOpcode::Return as mir::Opcode,
                        vec![],
                    ));
            }
            hir::Terminator::Br(branch) => match branch {
                hir::Branch::Conditional {
                    condition: _,
                    iftrue: _,
                    iffalse: _,
                } => unimplemented!(),
                hir::Branch::Unconditional { block_idx } => {
                    let bb = self.get_basic_block();

                    bb.successors = HashSet::from([*block_idx]);
                    bb.instructions.push(mir::Instruction::new(
                        GenericOpcode::Br as Opcode,
                        vec![mir::Operand::Block(*block_idx)],
                    ));
                }
            },
        }
    }

    fn ptr_add(&mut self, base: mir::Operand, offset: mir::Operand) -> mir::VregIdx {
        assert!(matches!(
            offset,
            mir::Operand::Register(_, _) | mir::Operand::Immediate(_)
        ));
        let vreg_idx = self.create_vreg(self.ty_storage.ptr_ty);

        self.get_basic_block()
            .instructions
            .push(mir::Instruction::new(
                GenericOpcode::PtrAdd as Opcode,
                vec![
                    mir::Operand::Register(Register::Virtual(vreg_idx), RegisterRole::Def),
                    base,
                    offset,
                ],
            ));

        vreg_idx
    }
}

impl<A: Abi> LocalStorage for FnLowering<'_, A> {
    fn get_local_ty(&self, idx: hir::LocalIdx) -> TyIdx {
        self.hir_function.locals[idx]
    }
}

#[derive(Default)]
pub struct LowerFunctionToModuleAdaptor;

impl<'a, T: Target> Pass<'a, hir::Module, T> for LowerFunctionToModuleAdaptor {
    fn run(&self, module: &mut hir::Module, ctx: &mut Context<'a, T>) {
        ctx.mir_module = Some(mir::Module {
            name: module.name.clone(),
            globals: module.globals.clone(),
            functions: Vec::new(),
        });

        for function in &mut module.functions {
            Lower::default().run(function, ctx);

            ctx.mir_module
                .as_mut()
                .unwrap()
                .functions
                .push(ctx.mir_function.take().unwrap());
        }
    }
}
