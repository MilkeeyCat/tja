use crate::{
    hir::{
        self, Const, LocalIdx, LocalStorage,
        op::BinOp,
        ty::{self, Ty, TyIdx},
    },
    mir::{
        self, BlockIdx, GenericOpcode, InstrBuilder, Operand, Register, RegisterRole,
        function::{FrameInfo, FunctionPatch, VregInfo},
    },
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
                vreg_info: VregInfo::default(),
                frame_info: FrameInfo::default(),
                blocks: Vec::new(),
            },
            operand_to_vreg_indices: HashMap::new(),
            ty_to_offsets: HashMap::new(),
            abi: ctx.target.abi(),
            current_bb_idx: None,
            entry_bb: mir::BasicBlock {
                name: "entry".into(),
                instructions: vec![],
                successors: HashSet::from([BlockIdx(1)]),
            },
        };

        let vreg_indices = (0..lowering.hir_function.params_count)
            .into_iter()
            .map(|local_idx| {
                lowering
                    .get_or_create_vregs(LocalIdx(local_idx).into())
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
            .push(mir::Instruction::br(BlockIdx(1)));

        for (idx, bb) in function.blocks.iter().enumerate() {
            lowering.current_bb_idx = Some(BlockIdx(idx));
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

        let mut patch = FunctionPatch::new();

        patch.add_basic_block(BlockIdx(0), lowering.entry_bb);
        patch.apply(&mut lowering.mir_function);
        ctx.mir_function = Some(lowering.mir_function);
    }
}

pub struct FnLowering<'a, A: Abi> {
    pub ty_storage: &'a ty::Storage,
    pub hir_function: &'a hir::Function,
    pub mir_function: mir::Function,
    operand_to_vreg_indices: HashMap<hir::Operand, Vec<mir::VregIdx>>,
    pub ty_to_offsets: HashMap<TyIdx, Vec<usize>>,
    pub abi: &'a A,
    current_bb_idx: Option<BlockIdx>,
    entry_bb: mir::BasicBlock,
}

impl<'a, A: Abi> FnLowering<'a, A> {
    pub fn get_or_create_vregs(&mut self, operand: hir::Operand) -> &[mir::VregIdx] {
        if self.operand_to_vreg_indices.contains_key(&operand) {
            return &self.operand_to_vreg_indices[&operand];
        }

        let ty = match operand {
            hir::Operand::Local(idx) => self.hir_function.locals[*idx],
            hir::Operand::Const(_, ty) => ty,
        };

        let mut vreg_indices: Vec<mir::VregIdx> = Vec::new();
        let mut types = Vec::new();
        let mut offsets = Vec::new();

        self.lower_ty(ty, &mut types, &mut offsets, 0);

        match &operand {
            hir::Operand::Local(_) => {
                for ty in types {
                    vreg_indices.push(self.mir_function.vreg_info.create_vreg(ty));
                }
            }
            hir::Operand::Const(c, ty) => match c {
                Const::Global(idx) => {
                    let vreg_idx = self.mir_function.vreg_info.create_vreg(*ty);

                    self.get_basic_block()
                        .instructions
                        .push(mir::Instruction::global_value(
                            Register::Virtual(vreg_idx),
                            (*idx).into(),
                        ));
                    vreg_indices.push(vreg_idx);
                }
                Const::Function(idx) => {
                    let vreg_idx = self.mir_function.vreg_info.create_vreg(*ty);

                    self.get_basic_block()
                        .instructions
                        .push(mir::Instruction::global_value(
                            Register::Virtual(vreg_idx),
                            (*idx).into(),
                        ));
                    vreg_indices.push(vreg_idx);
                }
                Const::Int(value) => {
                    let vreg_idx = self.mir_function.vreg_info.create_vreg(*ty);

                    self.get_basic_block()
                        .instructions
                        .push(mir::Instruction::copy(
                            Register::Virtual(vreg_idx),
                            mir::Operand::Immediate(*value),
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

    pub fn get_basic_block(&mut self) -> &mut mir::BasicBlock {
        self.current_bb_idx
            .map(|idx| &mut self.mir_function.blocks[*idx])
            .unwrap_or_else(|| &mut self.entry_bb)
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

                self.get_basic_block().instructions.push(
                    InstrBuilder::new(opcode.into())
                        .add_def(Register::Virtual(def))
                        .add_use(Register::Virtual(lhs))
                        .add_use(Register::Virtual(rhs))
                        .into(),
                );
            }
            hir::Instruction::Alloca { ty, out } => {
                let def = self.get_or_create_vreg(hir::Operand::Local(*out));
                let frame_idx = self
                    .mir_function
                    .frame_info
                    .create_stack_object(self.abi.ty_size(self.ty_storage, *ty));

                self.get_basic_block()
                    .instructions
                    .push(mir::Instruction::frame_idx(
                        Register::Virtual(def),
                        frame_idx,
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

                    bb.instructions.push(mir::Instruction::store(
                        Register::Virtual(vreg_idx),
                        Operand::not_def(Register::Virtual(ptr_add)),
                    ));
                }
            }
            hir::Instruction::Load { ptr, out } => {
                let base = self.get_or_create_vreg(ptr.clone());
                let vregs = self.get_or_create_vregs(hir::Operand::Local(*out)).to_vec();
                let offsets = self.ty_to_offsets[&self.hir_function.locals[**out]].to_vec();

                for (vreg_idx, offset) in vregs.into_iter().zip(offsets) {
                    let ptr_add = self.ptr_add(
                        mir::Operand::Register(Register::Virtual(base), RegisterRole::Use),
                        mir::Operand::Immediate(offset as u64),
                    );
                    let bb = self.get_basic_block();

                    bb.instructions.push(mir::Instruction::load(
                        Register::Virtual(vreg_idx),
                        Operand::not_def(Register::Virtual(ptr_add)),
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
                                        let def_vreg_idx = self
                                            .mir_function
                                            .vreg_info
                                            .create_vreg(self.ty_storage.ptr_ty);
                                        let lhs_vreg_idx = self.get_or_create_vreg(idx.clone());

                                        self.get_basic_block().instructions.push(
                                            mir::Instruction::binary(
                                                GenericOpcode::Mul.into(),
                                                Register::Virtual(def_vreg_idx),
                                                Operand::not_def(Register::Virtual(lhs_vreg_idx)),
                                                Operand::Immediate(size as u64),
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
                    .push(mir::Instruction::copy(
                        Register::Virtual(def_vreg_idx),
                        Operand::not_def(Register::Virtual(base)),
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
                        self.hir_function.locals[*out],
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
                    .push(mir::Instruction::new(GenericOpcode::Return.into()));
            }
            hir::Terminator::Br(branch) => match branch {
                hir::Branch::Conditional {
                    condition: _,
                    iftrue: _,
                    iffalse: _,
                } => unimplemented!(),
                hir::Branch::Unconditional { block_idx } => {
                    let bb = self.get_basic_block();

                    bb.successors = HashSet::from([BlockIdx(**block_idx)]);
                    bb.instructions
                        .push(mir::Instruction::br(BlockIdx(**block_idx)));
                }
            },
        }
    }

    fn ptr_add(&mut self, base: mir::Operand, offset: mir::Operand) -> mir::VregIdx {
        let vreg_idx = self
            .mir_function
            .vreg_info
            .create_vreg(self.ty_storage.ptr_ty);

        self.get_basic_block()
            .instructions
            .push(mir::Instruction::ptr_add(
                Register::Virtual(vreg_idx),
                base,
                offset,
            ));

        vreg_idx
    }
}

impl<A: Abi> LocalStorage for FnLowering<'_, A> {
    fn get_local_ty(&self, idx: hir::LocalIdx) -> TyIdx {
        self.hir_function.locals[*idx]
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
