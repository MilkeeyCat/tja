use crate::{
    hir::{
        self, Const, LocalIdx, LocalStorage,
        op::BinOp,
        ty::{self, Ty, TyIdx},
    },
    mir::{
        self, BasicBlockCursor, BasicBlockCursorMut, BlockIdx, GenericOpcode, InstructionCursor,
        InstructionCursorMut, Register, RegisterRole,
    },
    pass::{Context, Pass},
    targets::{Abi, CallingConvention, Target},
};
use index_vec::IndexVec;
use std::collections::HashMap;

#[derive(Default)]
pub struct Lower;

impl<'a, T: Target> Pass<'a, hir::Function, T> for Lower {
    fn run(&self, function: &mut hir::Function, ctx: &mut Context<'a, T>) {
        let mut mir_function = mir::Function::new(function.name.clone());

        if function.is_declaration() {
            ctx.mir_function = Some(mir_function);

            return;
        }

        let entry_bb_idx = mir_function.create_block("entry".into());

        mir_function.block_cursor_mut().insert_after(entry_bb_idx);

        let mut lowering = FnLowering {
            ty_storage: ctx.ty_storage,
            hir_function: function,
            mir_function,
            operand_to_vreg_indices: HashMap::new(),
            hir_to_mir_bb: HashMap::new(),
            ty_to_offsets: HashMap::new(),
            abi: ctx.target.abi(),
            current_bb_idx: entry_bb_idx,
        };

        let vreg_indices = (0..lowering.hir_function.params_count)
            .into_iter()
            .map(|local_idx| {
                lowering
                    .get_or_create_vregs(LocalIdx::new(local_idx).into())
                    .to_vec()
            })
            .collect();
        let tys =
            lowering.hir_function.locals[..lowering.hir_function.params_count.into()].to_vec();
        let ret_ty = lowering.hir_function.ret_ty;

        lowering.abi.calling_convention().lower_params(
            &mut lowering,
            vreg_indices,
            tys.raw,
            ret_ty,
        );

        for (idx, bb) in function.blocks.iter_enumerated() {
            let bb_idx = lowering.mir_function.create_block(bb.name.clone());

            lowering.block_cursor_mut().insert_after(bb_idx);
            lowering.hir_to_mir_bb.insert(idx, bb_idx);
        }

        let instr_idx = {
            let bb_idx = lowering.mir_function.blocks[entry_bb_idx].next.unwrap();

            lowering.mir_function.create_instr().br(bb_idx)
        };

        lowering.instr_cursor_mut().insert_after(instr_idx);

        for bb in &function.blocks {
            lowering.current_bb_idx = lowering.mir_function.blocks[lowering.current_bb_idx]
                .next
                .unwrap();

            for instr in &bb.instructions {
                lowering.lower_instruction(instr);
            }

            lowering.lower_terminator(&bb.terminator);
        }

        ctx.mir_function = Some(lowering.mir_function);
    }
}

pub struct FnLowering<'a, A: Abi> {
    pub ty_storage: &'a ty::Storage,
    pub hir_function: &'a hir::Function,
    pub mir_function: mir::Function,
    operand_to_vreg_indices: HashMap<hir::Operand, Vec<mir::VregIdx>>,
    hir_to_mir_bb: HashMap<hir::BlockIdx, mir::BlockIdx>,
    pub ty_to_offsets: HashMap<TyIdx, Vec<usize>>,
    pub abi: &'a A,
    current_bb_idx: BlockIdx,
}

impl<'a, A: Abi> FnLowering<'a, A> {
    pub fn block_cursor(&self) -> BasicBlockCursor<'_> {
        let mut cursor = self.mir_function.block_cursor();

        cursor.set_idx(self.current_bb_idx);

        cursor
    }

    pub fn block_cursor_mut(&mut self) -> BasicBlockCursorMut<'_> {
        let mut cursor = self.mir_function.block_cursor_mut();

        cursor.set_idx(self.current_bb_idx);

        cursor
    }

    pub fn instr_cursor(&self) -> InstructionCursor<'_> {
        self.mir_function
            .instr_cursor(self.current_bb_idx)
            .at_tail()
    }

    pub fn instr_cursor_mut(&mut self) -> InstructionCursorMut<'_> {
        self.mir_function
            .instr_cursor_mut(self.current_bb_idx)
            .at_tail()
    }

    pub fn get_or_create_vregs(&mut self, operand: hir::Operand) -> &[mir::VregIdx] {
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
                    vreg_indices.push(self.mir_function.vreg_info.create_vreg(ty));
                }
            }
            hir::Operand::Const(c, ty) => match c {
                Const::Global(idx) => {
                    let vreg_idx = self.mir_function.vreg_info.create_vreg(*ty);
                    let instr_idx = self
                        .mir_function
                        .create_instr()
                        .global_value(Register::Virtual(vreg_idx), (*idx).into());

                    self.instr_cursor_mut().insert_after(instr_idx);
                    vreg_indices.push(vreg_idx);
                }
                Const::Function(idx) => {
                    let vreg_idx = self.mir_function.vreg_info.create_vreg(*ty);
                    let instr_idx = self
                        .mir_function
                        .create_instr()
                        .global_value(Register::Virtual(vreg_idx), (*idx).into());

                    self.instr_cursor_mut().insert_after(instr_idx);
                    vreg_indices.push(vreg_idx);
                }
                Const::Int(value) => {
                    let vreg_idx = self.mir_function.vreg_info.create_vreg(*ty);
                    let instr_idx = self
                        .mir_function
                        .create_instr()
                        .copy(Register::Virtual(vreg_idx), mir::Operand::Immediate(*value));

                    self.instr_cursor_mut().insert_after(instr_idx);
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
                let instr_idx = self
                    .mir_function
                    .create_instr()
                    .with_opcode(opcode.into())
                    .add_def(Register::Virtual(def))
                    .add_use(Register::Virtual(lhs))
                    .add_use(Register::Virtual(rhs))
                    .idx();

                self.instr_cursor_mut().insert_after(instr_idx);
            }
            hir::Instruction::Alloca { ty, out } => {
                let def = self.get_or_create_vreg(hir::Operand::Local(*out));
                let frame_idx = self
                    .mir_function
                    .frame_info
                    .create_stack_object(self.abi.ty_size(self.ty_storage, *ty));
                let instr_idx = self
                    .mir_function
                    .create_instr()
                    .frame_idx(Register::Virtual(def), frame_idx);

                self.instr_cursor_mut().insert_after(instr_idx);
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
                    let instr_idx = self.mir_function.create_instr().store(
                        Register::Virtual(vreg_idx),
                        mir::Operand::not_def(Register::Virtual(ptr_add)),
                    );

                    self.instr_cursor_mut().insert_after(instr_idx);
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
                    let instr_idx = self.mir_function.create_instr().load(
                        Register::Virtual(vreg_idx),
                        mir::Operand::not_def(Register::Virtual(ptr_add)),
                    );

                    self.instr_cursor_mut().insert_after(instr_idx);
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
                                        let instr_idx = self
                                            .mir_function
                                            .create_instr()
                                            .with_opcode(GenericOpcode::Mul.into())
                                            .add_def(Register::Virtual(def_vreg_idx))
                                            .add_use(Register::Virtual(lhs_vreg_idx))
                                            .add_operand(mir::Operand::Immediate(size as u64))
                                            .idx();

                                        self.instr_cursor_mut().insert_after(instr_idx);

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
                let instr_idx = self.mir_function.create_instr().copy(
                    Register::Virtual(def_vreg_idx),
                    mir::Operand::not_def(Register::Virtual(base)),
                );

                self.instr_cursor_mut().insert_after(instr_idx);
            }
            hir::Instruction::Icmp {
                cond_code,
                lhs,
                rhs,
                out,
            } => {
                let out = self.get_or_create_vreg(hir::Operand::Local(*out));
                let lhs = self.get_or_create_vreg(lhs.clone());
                let rhs = self.get_or_create_vreg(rhs.clone());
                let instr_idx = self.mir_function.create_instr().icmp(
                    Register::Virtual(out),
                    *cond_code,
                    Register::Virtual(lhs),
                    Register::Virtual(rhs),
                );

                self.instr_cursor_mut().insert_after(instr_idx);
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
                let operand = value.clone().map(|value| {
                    let ty = value.ty(self);

                    (self.get_or_create_vregs(value).to_vec(), ty)
                });

                self.abi.calling_convention().lower_ret(self, operand);
            }
            hir::Terminator::Br(branch) => match branch {
                hir::Branch::Conditional {
                    condition,
                    iftrue,
                    iffalse,
                } => {
                    let condition = self.get_or_create_vreg(condition.clone());
                    let iftrue = self.hir_to_mir_bb[iftrue];
                    let iffalse = self.hir_to_mir_bb[iffalse];

                    self.block_cursor_mut()
                        .current_mut()
                        .unwrap()
                        .successors
                        .extend([iftrue, iffalse]);

                    let br_cond_idx = self.mir_function.create_instr().br_cond(
                        mir::Operand::Register(Register::Virtual(condition), RegisterRole::Use),
                        iftrue,
                    );
                    let br_idx = self.mir_function.create_instr().br(iffalse);
                    let mut cursor = self.instr_cursor_mut();

                    cursor.insert_after(br_cond_idx);
                    cursor.insert_after(br_idx);
                }
                hir::Branch::Unconditional { block_idx } => {
                    let bb_idx = self.hir_to_mir_bb[block_idx];
                    let instr_idx = self.mir_function.create_instr().br(bb_idx);

                    self.block_cursor_mut()
                        .current_mut()
                        .unwrap()
                        .successors
                        .insert(bb_idx);
                    self.instr_cursor_mut().insert_after(instr_idx);
                }
            },
        }
    }

    fn ptr_add(&mut self, base: mir::Operand, offset: mir::Operand) -> mir::VregIdx {
        let vreg_idx = self
            .mir_function
            .vreg_info
            .create_vreg(self.ty_storage.ptr_ty);
        let instr_idx =
            self.mir_function
                .create_instr()
                .ptr_add(Register::Virtual(vreg_idx), base, offset);

        self.instr_cursor_mut().insert_after(instr_idx);

        vreg_idx
    }
}

impl<A: Abi> LocalStorage for FnLowering<'_, A> {
    fn get_local_ty(&self, idx: hir::LocalIdx) -> TyIdx {
        self.hir_function.locals[idx]
    }
}

#[derive(Default)]
pub struct LowerFunctionToModuleAdapter;

impl<'a, T: Target> Pass<'a, hir::Module, T> for LowerFunctionToModuleAdapter {
    fn run(&self, module: &mut hir::Module, ctx: &mut Context<'a, T>) {
        ctx.mir_module = Some(mir::Module {
            name: module.name.clone(),
            globals: module.globals.clone(),
            functions: IndexVec::new(),
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
