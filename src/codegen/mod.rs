pub mod abi;
mod allocator;
pub mod calling_convention;
mod condition;
mod operands;
mod register;

use crate::repr::{
    BlockIdx, Branch, Const, FunctionIdx, GlobalIdx, Instruction, InstructionIdx, LocalIdx,
    LocalStorage, Module, Operand, Patch, Terminator, Wrapper,
    op::BinOp,
    ty::{Storage, Ty, TyIdx},
};
use abi::Abi;
use allocator::Allocator;
use condition::Condition;
use operands::{
    Base, Destination, EffectiveAddress, Immediate, InvalidOperandSize, Memory, Offset,
    OperandSize, Source,
};
use register::Register;
use std::collections::HashMap;

impl Const {
    fn to_imm(&self, codegen: &CodeGen) -> Immediate {
        match self {
            Self::Global(idx) => Immediate::Label(codegen.module.globals[*idx].name.clone()),
            Self::Function(idx) => Immediate::Label(codegen.module.functions[*idx].name.clone()),
            Self::Int(value) => Immediate::Int(*value),
            Self::Aggregate(_) => unreachable!(),
        }
    }

    fn to_location(&self, codegen: &CodeGen) -> Location {
        match self {
            Self::Global(idx) => codegen.globals[idx].clone(),
            Self::Function(_) | Self::Int(_) | Self::Aggregate(_) => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Register(Register),
    Address {
        effective_address: EffectiveAddress,
        spilled: bool,
    },
}

impl Location {
    pub fn to_source(&self, size: OperandSize) -> Source {
        match self {
            Self::Register(r) => r.resize(size).into(),
            Self::Address {
                effective_address, ..
            } => Source::Memory(Memory {
                effective_address: effective_address.clone(),
                size,
            }),
        }
    }

    pub fn to_dest(&self, size: OperandSize) -> Destination {
        match self {
            Self::Register(r) => r.resize(size).into(),
            Self::Address {
                effective_address, ..
            } => Destination::Memory(Memory {
                effective_address: effective_address.clone(),
                size,
            }),
        }
    }
}

impl From<Register> for Location {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}

impl Operand {
    /// panics if a local has multiple locations
    fn get_location(&self, codegen: &CodeGen) -> Location {
        match self {
            Self::Local(idx) => codegen.locals[idx].get_single_location().clone(),
            Self::Const(c, _) => c.to_location(codegen),
        }
    }

    fn to_source(&self, codegen: &CodeGen, size: OperandSize) -> Source {
        match self {
            Self::Local(idx) => codegen.locals[idx].get_single_location().to_source(size),
            Self::Const(c, _) => c.to_imm(codegen).into(),
        }
    }

    fn to_dest(&self, codegen: &CodeGen, size: OperandSize) -> Destination {
        self.get_location(codegen).to_dest(size)
    }
}

pub type LocalLocation = Vec<Location>;

#[derive(Debug)]
struct Variable {
    ty: TyIdx,
    location: LocalLocation,
}

impl Variable {
    fn get_single_location(&self) -> &Location {
        assert!(
            self.location.len() == 1,
            "expected the variable to have one location, got {}",
            self.location.len()
        );

        &self.location[0]
    }
}

pub struct CodeGen<'a, 'ctx> {
    module: Wrapper<'ctx, &'ctx mut Module>,
    locals: HashMap<LocalIdx, Variable>,
    globals: HashMap<GlobalIdx, Location>,
    spill_register: Register,
    bss: String,
    data: String,
    text: String,
    abi: &'a dyn Abi,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub fn new(module: Wrapper<'ctx, &'ctx mut Module>, abi: &'a dyn Abi) -> Self {
        Self {
            module,
            locals: HashMap::new(),
            globals: HashMap::new(),
            spill_register: Register::R8,
            bss: String::new(),
            data: String::new(),
            text: String::new(),
            abi,
        }
    }

    fn canonicalize(&mut self) {
        for function in &mut self.module.functions {
            let mut patch = Patch::new(function);

            for (block_idx, block) in function.blocks.iter().enumerate() {
                for (instr_idx, instruction) in block.instructions.iter().enumerate() {
                    match instruction {
                        Instruction::Binary {
                            kind: kind @ (BinOp::SDiv | BinOp::UDiv),
                            lhs,
                            rhs,
                            out,
                        } => {
                            let rhs = if matches!(rhs, Operand::Const(..)) {
                                let idx = patch.add_local(function.locals[*out]);

                                patch.add_instruction(
                                    block_idx,
                                    instr_idx,
                                    Instruction::Copy {
                                        out: idx,
                                        operand: rhs.clone(),
                                    },
                                );

                                Operand::Local(idx)
                            } else {
                                rhs.clone()
                            };
                            let idx = patch.add_local(function.locals[*out]);

                            patch.add_instruction(
                                block_idx,
                                instr_idx + 1,
                                Instruction::Copy {
                                    out: *out,
                                    operand: Operand::Local(idx),
                                },
                            );
                            patch.patch_instruction(
                                block_idx,
                                instr_idx,
                                Instruction::Binary {
                                    kind: *kind,
                                    lhs: lhs.clone(),
                                    rhs,
                                    out: idx,
                                },
                            );
                        }
                        Instruction::Binary { .. }
                        | Instruction::Copy { .. }
                        | Instruction::Sext { .. }
                        | Instruction::Zext { .. }
                        | Instruction::Alloca { .. }
                        | Instruction::Store { .. }
                        | Instruction::Load { .. }
                        | Instruction::GetElementPtr { .. } => (),
                        Instruction::Icmp {
                            kind,
                            lhs: lhs @ Operand::Const(_, ty),
                            rhs,
                            out,
                            ..
                        } => {
                            let idx = patch.add_local(*ty);

                            patch.add_instruction(
                                block_idx,
                                instr_idx,
                                Instruction::Copy {
                                    out: idx,
                                    operand: lhs.clone(),
                                },
                            );
                            patch.patch_instruction(
                                block_idx,
                                instr_idx,
                                Instruction::Icmp {
                                    kind: *kind,
                                    lhs: Operand::Local(idx),
                                    rhs: rhs.clone(),
                                    out: *out,
                                },
                            )
                        }
                        Instruction::Icmp { .. } | Instruction::Call { .. } => (),
                    }
                }

                match &block.terminator {
                    Terminator::Return(Some(operand @ Operand::Const(_, ty))) => {
                        let idx = patch.add_local(*ty);

                        patch.add_instruction(
                            block_idx,
                            block.instructions.len(),
                            Instruction::Copy {
                                out: idx,
                                operand: operand.clone(),
                            },
                        );
                        patch.patch_terminator(
                            block_idx,
                            Terminator::Return(Some(Operand::Local(idx))),
                        );
                    }
                    Terminator::Return(_) => (),
                    Terminator::Br(Branch::Conditional {
                        condition: condition @ Operand::Const(_, ty),
                        iftrue,
                        iffalse,
                    }) => {
                        let idx = patch.add_local(*ty);

                        patch.add_instruction(
                            block_idx,
                            block.instructions.len(),
                            Instruction::Copy {
                                out: idx,
                                operand: condition.clone(),
                            },
                        );
                        patch.patch_terminator(
                            block_idx,
                            Terminator::Br(Branch::Conditional {
                                condition: Operand::Local(idx),
                                iftrue: *iftrue,
                                iffalse: *iffalse,
                            }),
                        );
                    }
                    Terminator::Br(_) => (),
                }
            }

            patch.apply(function);
        }
    }

    fn precolor(&self, allocator: &mut Allocator, idx: FunctionIdx) {
        for block in &self.module.functions[idx].blocks {
            for instruction in &block.instructions {
                match instruction {
                    Instruction::Binary {
                        kind: BinOp::SDiv | BinOp::UDiv,
                        lhs,
                        rhs,
                        out,
                    } => {
                        allocator.precolor(*out, vec![Register::Rax.into()]);

                        let indices: Vec<_> = vec![lhs.local_idx(), rhs.local_idx(), Some(*out)]
                            .into_iter()
                            .flatten()
                            .collect();

                        if !indices.is_empty() {
                            let ty = self.module.functions[idx].locals[*out];
                            let rdx = allocator.create_node(ty);
                            allocator.precolor(rdx, vec![Register::Rdx.into()]);

                            for idx in indices {
                                allocator.add_edge((rdx, idx));
                            }
                        }
                    }
                    Instruction::Binary {
                        kind: BinOp::Sub,
                        rhs: Operand::Local(rhs),
                        out,
                        ..
                    } => allocator.add_edge((*out, *rhs)),
                    Instruction::Alloca { ty, out } => {
                        allocator.stack_frame_size += self.ty_size(*ty);
                        allocator.precolor(
                            *out,
                            vec![Location::Address {
                                effective_address: EffectiveAddress {
                                    base: Base::Register(Register::Rbp),
                                    index: None,
                                    scale: None,
                                    displacement: Some(Offset(
                                        -(allocator.stack_frame_size as isize),
                                    )),
                                },
                                spilled: false,
                            }],
                        );
                    }
                    Instruction::Binary { .. }
                    | Instruction::Copy { .. }
                    | Instruction::Sext { .. }
                    | Instruction::Zext { .. }
                    | Instruction::Store { .. }
                    | Instruction::Load { .. }
                    | Instruction::GetElementPtr { .. }
                    | Instruction::Icmp { .. } => (),
                    Instruction::Call { arguments, .. } => {
                        // TODO: move this somewhere else, codegen shouldn't know about this
                        let registers: Vec<_> = vec![
                            Register::Rdi,
                            Register::Rsi,
                            Register::Rdx,
                            Register::Rcx,
                            Register::R8,
                            Register::R9,
                        ]
                        .into_iter()
                        .map(|r| {
                            let idx = allocator.create_node(self.module.ty_storage.void_ty);

                            allocator.precolor(idx, vec![r.into()]);

                            idx
                        })
                        .collect();

                        for operand in arguments {
                            if let Some(local_idx) = operand.local_idx() {
                                for &r in &registers {
                                    allocator.add_edge((r, local_idx));
                                }
                            }
                        }
                    }
                }
            }

            match &block.terminator {
                Terminator::Return(operand) => {
                    if let Some(Operand::Local(local_idx)) = operand {
                        allocator.precolor(
                            *local_idx,
                            self.abi.calling_convention().returned_value_location(
                                self,
                                self.module.functions[idx].locals[*local_idx],
                            ),
                        );
                    }
                }
                Terminator::Br(_) => (),
            };
        }
    }

    fn instruction(
        &mut self,
        fn_idx: FunctionIdx,
        block_idx: BlockIdx,
        instr_idx: InstructionIdx,
    ) -> Result<(), InvalidOperandSize> {
        match &self.module.functions[fn_idx].blocks[block_idx].instructions[instr_idx].clone() {
            Instruction::Binary {
                kind,
                lhs,
                rhs,
                out,
            } => {
                let ty = self.locals[out].ty;
                let ty_size: OperandSize = self.ty_size(ty).try_into()?;

                if kind != &BinOp::Sub {
                    self.copy(lhs, &self.locals[out].get_single_location().clone(), ty)?;
                }

                match kind {
                    BinOp::Add => {
                        self.add(
                            &self.locals[out].get_single_location().to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::Sub => {
                        if let Operand::Const(Const::Int(0), _) = lhs {
                            self.copy(rhs, &self.locals[out].get_single_location().clone(), ty)?;
                            self.neg(&self.locals[out].get_single_location().to_dest(ty_size));
                        } else {
                            self.copy(lhs, &self.locals[out].get_single_location().clone(), ty)?;
                            self.sub(
                                &self.locals[out].get_single_location().to_dest(ty_size),
                                &rhs.to_source(self, ty_size),
                            );
                        }
                    }
                    BinOp::Mul => {
                        self.mul(
                            &self.locals[out].get_single_location().to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::SDiv => {
                        assert!(
                            !matches!(rhs, Operand::Const(..)),
                            "rhs of sdiv can't be a const"
                        );

                        let dest = rhs.to_dest(self, ty_size);

                        match ty_size {
                            OperandSize::Byte => {
                                let src = self.locals[out].get_single_location();

                                self.sext(
                                    &src.to_source(OperandSize::Byte),
                                    &src.to_dest(OperandSize::Word),
                                );
                            }
                            OperandSize::Word => {
                                self.text.push_str("\tcwd\n");
                            }
                            OperandSize::Dword => {
                                self.text.push_str("\tcdq\n");
                            }
                            OperandSize::Qword => {
                                self.text.push_str("\tcqo\n");
                            }
                        };
                        self.div(&dest);
                    }
                    BinOp::UDiv => {
                        assert!(
                            !matches!(rhs, Operand::Const(..)),
                            "rhs of udiv can't be a const"
                        );

                        let dest = rhs.to_dest(self, ty_size);

                        match ty_size {
                            OperandSize::Byte => {
                                let src = self.locals[out].get_single_location();

                                self.zext(
                                    &src.to_source(OperandSize::Byte),
                                    &src.to_dest(OperandSize::Word),
                                );
                            }
                            size => {
                                let r = Register::Rdx.resize(size);

                                self.xor(&r.into(), &r.into());
                            }
                        };
                        self.div(&dest);
                    }
                };
            }
            Instruction::Copy { out, operand } => {
                self.explode_operand(operand.clone(), &self.locals[out].location.clone())?;
            }
            Instruction::Sext { operand, out } => {
                let out = &self.locals[out];

                self.sext(
                    &operand.to_source(self, self.ty_size(operand.ty(self)).try_into()?),
                    &out.get_single_location()
                        .to_dest(self.ty_size(out.ty).try_into()?),
                );
            }
            Instruction::Zext { operand, out } => {
                let out = &self.locals[out];

                self.zext(
                    &operand.to_source(self, self.ty_size(operand.ty(self)).try_into()?),
                    &out.get_single_location()
                        .to_dest(self.ty_size(out.ty).try_into()?),
                );
            }
            Instruction::Alloca { .. } => (),
            Instruction::Store { ptr, value } => {
                let loc = match ptr.get_location(self) {
                    Location::Register(r) => Location::Address {
                        effective_address: r.into(),
                        spilled: false,
                    },
                    loc => loc,
                };

                self.copy(value, &loc, value.ty(self))?;
            }
            Instruction::Load { ptr, out } => {
                let src = match ptr.get_location(self) {
                    Location::Address {
                        spilled: true,
                        effective_address,
                    } => {
                        self.mov(
                            &effective_address.src(OperandSize::Qword),
                            &self.spill_register.into(),
                        );

                        EffectiveAddress::from(self.spill_register)
                    }
                    Location::Address {
                        effective_address, ..
                    } => effective_address,
                    Location::Register(r) => EffectiveAddress::from(r),
                };

                let size = self.ty_size(self.locals[out].ty);

                match Operand::Local(*out).get_location(self) {
                    Location::Register(r) => {
                        let operand_size = size.try_into()?;

                        self.mov(&src.src(operand_size), &r.resize(operand_size).into());
                    }
                    Location::Address {
                        effective_address, ..
                    } => {
                        self.inline_memcpy(&src.clone(), &effective_address.clone(), size);
                    }
                }
            }
            Instruction::GetElementPtr {
                ptr,
                ptr_ty,
                indices,
                out,
            } => match indices.as_slice() {
                [operand, rest @ ..] => {
                    let mut ptr = match ptr.get_location(self) {
                        Location::Address {
                            effective_address, ..
                        } => effective_address,
                        Location::Register(r) => r.into(),
                    };
                    let index = match operand {
                        Operand::Const(c, _) => c.usize_unchecked(),
                        Operand::Local(_) => unreachable!(),
                    };

                    ptr = ptr + Offset((self.ty_size(*ptr_ty) * index) as isize);

                    self.get_element_ptr(*ptr_ty, &ptr, rest, *out);
                }
                [] => (),
            },
            Instruction::Icmp {
                kind,
                lhs,
                rhs,
                out,
            } => {
                let size = self.ty_size(lhs.ty(self)).try_into()?;

                self.cmp(&lhs.to_dest(self, size), &rhs.to_source(self, size));
                self.setcc(
                    &self.locals[out]
                        .get_single_location()
                        .to_dest(OperandSize::Byte),
                    kind.into(),
                );
            }
            Instruction::Call {
                operand,
                arguments,
                out,
            } => {
                let locations = self.abi.calling_convention().arguments(
                    self,
                    out.map(|idx| self.locals[&idx].ty)
                        .unwrap_or_else(|| self.module.ty_storage.void_ty),
                    &arguments
                        .iter()
                        .map(|operand| operand.ty(self))
                        .collect::<Vec<_>>(),
                );

                for (argument, locations) in arguments.iter().zip(locations) {
                    self.explode_operand(argument.clone(), &locations)?;
                }

                self.call(&operand.to_source(self, OperandSize::Qword));
            }
        };

        Ok(())
    }

    fn terminator(
        &mut self,
        fn_idx: FunctionIdx,
        block_idx: BlockIdx,
    ) -> Result<(), InvalidOperandSize> {
        match &self.module.functions[fn_idx].blocks[block_idx]
            .terminator
            .clone()
        {
            Terminator::Return(_) => {
                self.jmp(&format!(".L{}_ret", &self.module.functions[fn_idx].name));
            }
            Terminator::Br(branch) => match branch {
                Branch::Conditional {
                    condition,
                    iftrue,
                    iffalse,
                } => {
                    let size = self.ty_size(condition.ty(self)).try_into()?;
                    let loc = condition.get_location(self);

                    self.test(&loc.to_dest(size), &loc.to_source(size));
                    self.jcc(&self.block_label(fn_idx, *iftrue), Condition::NotEqual);
                    self.jmp(&self.block_label(fn_idx, *iffalse));
                }
                Branch::Unconditional { block_idx } => {
                    self.jmp(&self.block_label(fn_idx, *block_idx))
                }
            },
        };

        Ok(())
    }

    fn global(&mut self, idx: GlobalIdx) {
        let global = &self.module.globals[idx];

        if let Some(value) = &global.value {
            self.data.push_str(&format!(".global {}\n", global.name));
            self.data.push_str(&format!("{}:\n", global.name));
            self.emit_const(value.clone(), global.ty);
        } else {
            self.bss.push_str(&format!(".global {}\n", global.name));
            self.bss.push_str(&format!("{}:\n", global.name));
            self.bss
                .push_str(&format!("\t.zero {}\n", self.ty_size(global.ty)));
        }
    }

    fn function(&mut self, idx: FunctionIdx) {
        let mut allocator = Allocator::new(
            self.module.functions[idx].locals.clone(),
            self.module.functions[idx].interference(),
            vec![
                Register::R15,
                Register::R14,
                Register::R13,
                Register::R12,
                Register::R11,
                Register::R10,
                Register::R9,
                Register::Rcx,
                Register::Rdx,
                Register::Rsi,
                Register::Rdi,
                Register::Rax,
                self.spill_register,
            ],
            false,
        );
        self.abi.calling_convention().precolor_parameters(
            self,
            &mut allocator,
            self.module.functions[idx].ret_ty,
            &self.module.functions[idx].locals[..self.module.functions[idx].params_count],
        );
        self.precolor(&mut allocator, idx);

        let (locations, mut stack_frame_size) = allocator.allocate(self);
        stack_frame_size = stack_frame_size.next_multiple_of(16);
        self.locals = locations
            .into_iter()
            .zip(&self.module.functions[idx].locals)
            .enumerate()
            .map(|(i, (location, ty))| (i, Variable { ty: *ty, location }))
            .collect();
        self.text.push_str(&format!(
            ".global {0}\n{0}:\n",
            &self.module.functions[idx].name
        ));

        if stack_frame_size > 0 {
            self.push(&Register::Rbp.into());
            self.mov(&Register::Rsp.into(), &Register::Rbp.into());
            self.sub(
                &Register::Rsp.into(),
                &Source::Immediate(Immediate::Int(stack_frame_size.next_multiple_of(16) as u64)),
            );
        }

        for block_idx in 0..self.module.functions[idx].blocks.len() {
            let block = &self.module.functions[idx].blocks[block_idx];
            self.text
                .push_str(&format!("{}:\n", self.block_label(idx, block_idx)));

            for instr_idx in 0..block.instructions.len() {
                self.instruction(idx, block_idx, instr_idx).unwrap();
            }

            self.terminator(idx, block_idx).unwrap();
        }

        self.text
            .push_str(&format!(".L{}_ret:\n", &self.module.functions[idx].name));
        if stack_frame_size > 0 {
            self.text.push_str("\tleave\n");
        }
        self.text.push_str("\tret\n");
        self.locals.clear();
    }

    fn get_element_ptr(
        &mut self,
        ty: TyIdx,
        base: &EffectiveAddress,
        indices: &[Operand],
        out: LocalIdx,
    ) {
        match indices {
            [] => {
                self.lea(
                    &self.locals[&out]
                        .get_single_location()
                        .to_dest(OperandSize::Qword),
                    base,
                );
            }
            [operand, rest @ ..] => {
                let (ty, base) = match self.module.ty_storage.get_ty(ty) {
                    Ty::Struct(fields) => {
                        let index = match operand {
                            Operand::Const(c, _) => c.usize_unchecked(),
                            Operand::Local(_) => unreachable!(),
                        };
                        let base = base.clone() + Offset(self.field_offset(fields, index) as isize);

                        (&fields[index], base)
                    }
                    _ => unreachable!(),
                };

                self.get_element_ptr(*ty, &base, rest, out);
            }
        }
    }

    pub fn compile(mut self) -> Vec<u8> {
        self.canonicalize();

        self.globals = self
            .module
            .globals
            .iter()
            .enumerate()
            .map(|(idx, global)| {
                (
                    idx,
                    Location::Address {
                        effective_address: EffectiveAddress {
                            base: Base::Label(global.name.clone()),
                            index: None,
                            scale: None,
                            displacement: None,
                        },
                        spilled: false,
                    },
                )
            })
            .collect();

        for idx in 0..self.module.globals.len() {
            self.global(idx);
        }

        for idx in 0..self.module.functions.len() {
            self.function(idx);
        }

        let mut result = String::new();

        if !self.bss.is_empty() {
            result.push_str(".bss\n");
            result.push_str(&self.bss);
        }
        if !self.data.is_empty() {
            result.push_str(".data\n");
            result.push_str(&self.data);
        }
        if !self.text.is_empty() {
            result.push_str(".section .text\n");
            result.push_str(&self.text);
        }

        result.into_bytes()
    }

    fn emit_const(&mut self, c: Const, ty: TyIdx) {
        match c {
            Const::Global(idx) => self
                .data
                .push_str(&format!("\t.quad {}\n", self.module.globals[idx].name)),
            Const::Function(idx) => self
                .data
                .push_str(&format!("\t.quad {}\n", self.module.functions[idx].name)),
            Const::Int(value) => {
                let prefix = match self.module.ty_storage.get_ty(ty) {
                    Ty::I8 => ".byte",
                    Ty::I16 => ".short",
                    Ty::I32 => ".long",
                    Ty::I64 => ".quad",
                    _ => unreachable!(),
                };

                self.data.push_str(&format!("\t{prefix} {value}\n"));
            }
            Const::Aggregate(consts) => {
                let fields = match self.module.ty_storage.get_ty(ty) {
                    Ty::Struct(fields) => fields,
                    _ => unreachable!(),
                }
                .clone();

                let mut last_offset_plus_size = 0;

                for (i, (c, ty)) in consts.into_iter().zip(fields.iter().cloned()).enumerate() {
                    let field_offset = self.field_offset(&fields, i);

                    if field_offset > last_offset_plus_size {
                        self.data.push_str(&format!(
                            "\t.zero {}\n",
                            field_offset - last_offset_plus_size
                        ));
                    }

                    self.emit_const(c, ty);
                    last_offset_plus_size = field_offset + self.ty_size(ty);
                }
            }
        }
    }

    fn inline_memcpy(&mut self, src: &EffectiveAddress, dest: &EffectiveAddress, size: usize) {
        self.mov(
            &Source::Immediate(Immediate::Int(size as u64)),
            &Register::Rcx.into(),
        );
        self.lea(&Register::Rsi.into(), src);
        self.lea(&Register::Rdi.into(), dest);
        self.text.push_str("\trep movsb\n");
    }

    fn copy(
        &mut self,
        src: &Operand,
        dest: &Location,
        ty: TyIdx,
    ) -> Result<(), InvalidOperandSize> {
        match src {
            Operand::Local(idx) => match self.locals[idx].location.as_slice() {
                [location] => match location {
                    Location::Address {
                        effective_address,
                        spilled: false,
                    } if ty == self.module.ty_storage.ptr_ty => {
                        self.lea(
                            &dest.to_dest(OperandSize::Qword),
                            &effective_address.clone(),
                        );
                    }
                    location => self.mov_location(&location.clone(), dest, self.ty_size(ty))?,
                },
                location => self.implode_operand(&location.to_vec(), dest, self.ty_size(ty))?,
            },
            Operand::Const(c, _) => self.mov_const(dest, c, ty)?,
        };

        Ok(())
    }

    fn mov_const(
        &mut self,
        dest: &Location,
        src: &Const,
        ty: TyIdx,
    ) -> Result<(), InvalidOperandSize> {
        match src {
            Const::Aggregate(values) => match self.module.ty_storage.get_ty(ty) {
                Ty::Struct(fields) => {
                    let addr = match dest {
                        Location::Address {
                            effective_address, ..
                        } => effective_address,
                        Location::Register(_) => unreachable!(),
                    };

                    let offsets: Vec<_> = fields
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| (Offset(self.field_offset(fields, i) as isize), *ty))
                        .collect();

                    for (value, (offset, ty)) in values.iter().zip(offsets) {
                        self.copy(
                            &Operand::Const(value.clone(), ty),
                            &Location::Address {
                                effective_address: addr.clone() + offset,
                                spilled: false,
                            },
                            ty,
                        )?;
                    }
                }
                _ => unreachable!(),
            },
            c => self.mov(
                &c.to_imm(self).into(),
                &dest.to_dest(self.ty_size(ty).try_into()?),
            ),
        };

        Ok(())
    }

    fn mov_location(
        &mut self,
        src: &Location,
        dest: &Location,
        size: usize,
    ) -> Result<(), InvalidOperandSize> {
        let reg_resize_iter = |mut size: usize| {
            let mut chunks = Vec::new();

            while size > 0 {
                let chunk = if size.is_power_of_two() {
                    size
                } else {
                    size.next_power_of_two() >> 1
                };

                chunks.push(chunk);
                size -= chunk;
            }

            chunks.into_iter().peekable()
        };

        match (src, dest) {
            (
                Location::Address {
                    effective_address: src,
                    ..
                },
                Location::Address {
                    effective_address: dest,
                    ..
                },
            ) => {
                self.inline_memcpy(&src.clone(), &dest.clone(), size);
            }
            (
                Location::Address {
                    effective_address, ..
                },
                Location::Register(dest),
            ) => {
                let mut iter = reg_resize_iter(size);
                let mut offset = 0;

                while let Some(size) = iter.next() {
                    self.mov(
                        &(effective_address.clone() + Offset(offset as isize))
                            .src(size.try_into()?),
                        &dest.resize(size.try_into()?).into(),
                    );
                    if iter.peek().is_some() {
                        self.shl(
                            &(*dest).into(),
                            &Source::Immediate(
                                Immediate::Int((size as u32 * u8::BITS) as u64).into(),
                            ),
                        );
                    }

                    offset += size;
                }
            }
            (
                Location::Register(src),
                Location::Address {
                    effective_address, ..
                },
            ) => {
                let mut iter = reg_resize_iter(size);
                let mut offset = 0;

                while let Some(size) = iter.next() {
                    self.mov(
                        &src.resize(size.try_into()?).into(),
                        &(effective_address.clone() + Offset(offset as isize))
                            .dest(size.try_into()?),
                    );
                    if iter.peek().is_some() {
                        self.shr(
                            &(*src).into(),
                            &Source::Immediate(
                                Immediate::Int((size as u32 * u8::BITS) as u64).into(),
                            ),
                        );
                    }

                    offset += size;
                }
            }
            (Location::Register(src), Location::Register(dest)) => {
                let size = size.next_power_of_two();
                assert!(size <= 8);
                let size = OperandSize::try_from(size)?;

                self.mov(&src.resize(size).into(), &dest.resize(size).into());
            }
        };

        Ok(())
    }

    fn implode_operand(
        &mut self,
        location: &LocalLocation,
        dest: &Location,
        ty: TyIdx,
    ) -> Result<(), InvalidOperandSize> {
        match location.as_slice() {
            [location] => self.mov_location(location, dest, self.ty_size(ty))?,
            locations => {
                let dest = match dest {
                    Location::Address {
                        effective_address, ..
                    } => effective_address,
                    Location::Register(_) => unreachable!(),
                };
                let mut offset = 0;

                for location in locations {
                    let size = (self.ty_size(ty) - offset).min(8);
                    assert!(size > 0);

                    self.mov_location(
                        location,
                        &Location::Address {
                            effective_address: dest.clone() + Offset(offset as isize),
                            spilled: false,
                        },
                        size,
                    )?;

                    offset += size;
                }
            }
        }

        Ok(())
    }

    fn explode_operand(
        &mut self,
        operand: Operand,
        locations: &[Location],
    ) -> Result<(), InvalidOperandSize> {
        match operand {
            Operand::Const(c, ty) => {
                fn flatten(
                    storage: &Storage,
                    abi: &dyn Abi,
                    consts: &mut Vec<(Const, usize, TyIdx)>,
                    c: Const,
                    ty: TyIdx,
                    offset: usize,
                ) {
                    match c {
                        Const::Global(idx) | Const::Function(idx) => unimplemented!(),
                        Const::Int(_) => consts.push((c, offset, ty)),
                        Const::Aggregate(c) => {
                            let fields = match storage.get_ty(ty) {
                                Ty::Struct(fields) => fields,
                                _ => unreachable!(),
                            };

                            for (i, (ty, c)) in
                                fields.into_iter().cloned().zip(c.into_iter()).enumerate()
                            {
                                flatten(
                                    storage,
                                    abi,
                                    consts,
                                    c,
                                    ty,
                                    offset + abi.field_offset(storage, fields, i),
                                );
                            }
                        }
                    }
                }

                let mut consts = Vec::new();
                flatten(&self.module.ty_storage, self.abi, &mut consts, c, ty, 0);
                let mut splits: Vec<Vec<(Const, usize, TyIdx)>> = vec![Vec::new(); locations.len()];
                for (i, location) in locations.iter().enumerate() {
                    let split = &mut splits[i];

                    match location {
                        Location::Address { .. } => {
                            split.extend(consts.iter().cloned());
                        }
                        Location::Register(_) => {
                            let start_offset = consts[0].1;
                            while let Some((_, offset, _)) = consts.get(0) {
                                if (offset - start_offset) < 8 {
                                    let (c, offset, ty) = consts.remove(0);

                                    split.push((c, offset - i * 8, ty));
                                } else {
                                    break;
                                }
                            }
                        }
                    }
                }

                for (consts, location) in splits.into_iter().zip(locations.iter()) {
                    let mut result: u64 = 0;
                    let mut size = 0;

                    for (c, offset, ty) in consts.into_iter().rev() {
                        match c {
                            Const::Global(idx) | Const::Function(idx) => unimplemented!(),
                            Const::Int(value) => {
                                result |= value;
                                result <<= offset * u8::BITS as usize;
                            }
                            Const::Aggregate(_) => unreachable!(),
                        };

                        size += self.ty_size(ty);
                    }

                    self.mov(
                        &Source::Immediate(Immediate::Int(result)),
                        &location.to_dest(size.try_into()?),
                    )
                }
            }
            Operand::Local(idx) => {
                let src = self.locals[&idx].location.clone();

                match (src.as_slice(), locations) {
                    (src, dest) if src.len() == dest.len() => {
                        let mut offset = 0;

                        for (src, dest) in src.iter().zip(dest.iter()) {
                            let size = (self.ty_size(self.locals[&idx].ty) - offset).min(8);

                            self.mov_location(src, dest, size)?;
                            offset += size;
                        }
                    }
                    (
                        [
                            Location::Address {
                                effective_address, ..
                            },
                        ],
                        dest,
                    ) => {
                        let mut offset = 0;

                        for location in dest {
                            let size = (self.ty_size(self.locals[&idx].ty) - offset).min(8);

                            self.mov_location(
                                &Location::Address {
                                    effective_address: effective_address.clone()
                                        + Offset(offset as isize),
                                    spilled: false,
                                },
                                location,
                                size,
                            )?;
                            offset += size;
                        }
                    }
                    (
                        src,
                        [
                            Location::Address {
                                effective_address, ..
                            },
                        ],
                    ) => {
                        let mut offset = 0;

                        for location in src {
                            let size = (self.ty_size(self.locals[&idx].ty) - offset).min(8);

                            self.mov_location(
                                location,
                                &Location::Address {
                                    effective_address: effective_address.clone()
                                        + Offset(offset as isize),
                                    spilled: false,
                                },
                                size,
                            )?;
                            offset += size;
                        }
                    }
                    _ => (),
                }
            }
        }

        Ok(())
    }

    fn mov(&mut self, src: &Source, dest: &Destination) {
        assert!(
            !(matches!(src, Source::Memory(..)) && matches!(dest, Destination::Memory(..))),
            "can't move memory to memory"
        );
        self.text.push_str(&format!("\tmov {dest}, {src}\n"));
    }

    fn sext(&mut self, src: &Source, dest: &Destination) {
        if let Source::Immediate(_) = src {
            self.mov(src, dest);
        } else {
            self.text.push_str(&format!("\tmovsx {dest}, {src}\n"));
        }
    }

    fn zext(&mut self, src: &Source, dest: &Destination) {
        match (src.size(), dest.size()) {
            // on x86_64 you can move 32 bit value in 64 bit register, and upper 32 bits of the register will be zeroed
            (Some(OperandSize::Dword), OperandSize::Qword) => {
                self.mov(src, &dest.clone().resize(OperandSize::Dword));
            }
            _ => {
                if let Source::Immediate(_) = src {
                    self.mov(src, dest);
                } else {
                    self.text.push_str(&format!("\tmovzx {dest}, {src}\n"));
                }
            }
        }
    }

    fn push(&mut self, dest: &Destination) {
        self.text.push_str(&format!("\tpush {dest}\n"));
    }

    fn add(&mut self, lhs: &Destination, rhs: &Source) {
        self.text.push_str(&format!("\tadd {lhs}, {rhs}\n"));
    }

    fn sub(&mut self, lhs: &Destination, rhs: &Source) {
        self.text.push_str(&format!("\tsub {lhs}, {rhs}\n"));
    }

    fn mul(&mut self, lhs: &Destination, rhs: &Source) {
        self.text.push_str(&format!("\timul {lhs}, {rhs}\n"));
    }

    fn div(&mut self, op: &Destination) {
        self.text.push_str(&format!("\tidiv {op}\n"));
    }

    fn lea(&mut self, dest: &Destination, address: &EffectiveAddress) {
        self.text.push_str(&format!("\tlea {dest}, {address}\n"));
    }

    fn xor(&mut self, lhs: &Destination, rhs: &Source) {
        self.text.push_str(&format!("\txor {lhs}, {rhs}\n"));
    }

    fn cmp(&mut self, lhs: &Destination, rhs: &Source) {
        self.text.push_str(&format!("\tcmp {lhs}, {rhs}\n"));
    }

    fn setcc(&mut self, dest: &Destination, cond: Condition) {
        self.text.push_str(&format!("\tset{cond} {dest}\n"));
    }

    fn jmp(&mut self, label: &str) {
        self.text.push_str(&format!("\tjmp {label}\n"));
    }

    fn jcc(&mut self, label: &str, cond: Condition) {
        self.text.push_str(&format!("\tj{cond} {label}\n"));
    }

    fn test(&mut self, lhs: &Destination, rhs: &Source) {
        self.text.push_str(&format!("\ttest {lhs}, {rhs}\n"))
    }

    fn neg(&mut self, op: &Destination) {
        self.text.push_str(&format!("\tneg {op}\n"));
    }

    fn shl(&mut self, lhs: &Destination, rhs: &Source) {
        assert!(
            rhs == &Source::Register(Register::Cl)
                || matches!(rhs, Source::Immediate(Immediate::Int(..)))
        );

        self.text.push_str(&format!("\tshl {lhs}, {rhs}\n"));
    }

    fn shr(&mut self, lhs: &Destination, rhs: &Source) {
        assert!(
            rhs == &Source::Register(Register::Cl)
                || matches!(rhs, Source::Immediate(Immediate::Int(..)))
        );

        self.text.push_str(&format!("\tshr {lhs}, {rhs}\n"));
    }

    fn call(&mut self, target: &Source) {
        self.text.push_str(&format!("\tcall {target}\n"));
    }

    #[inline]
    fn field_offset(&self, fields: &[TyIdx], i: usize) -> usize {
        self.abi.field_offset(self.module.ty_storage, fields, i)
    }

    #[inline]
    fn ty_size(&self, ty: TyIdx) -> usize {
        self.abi.ty_size(self.module.ty_storage, ty)
    }

    #[inline]
    pub fn alignment(&self, ty: TyIdx) -> usize {
        self.abi.alignment(self.module.ty_storage, ty)
    }

    #[inline]
    pub fn block_label(&self, fn_idx: FunctionIdx, block_idx: BlockIdx) -> String {
        format!(
            ".L_{fn_idx}_{}",
            self.module.functions[fn_idx].blocks[block_idx].name
        )
    }
}

impl LocalStorage for CodeGen<'_, '_> {
    fn get_local_ty(&self, idx: LocalIdx) -> TyIdx {
        self.locals[&idx].ty
    }
}

#[cfg(test)]
mod tests {
    use super::{
        CodeGen, Location,
        operands::{Base, EffectiveAddress, InvalidOperandSize, Offset},
        register::Register,
    };
    use crate::{
        codegen::abi::sysv_amd64,
        repr::{Context, Operand, basic_block, op::BinOp, ty::Ty},
    };

    fn assert_generated_basic_block<F: Fn(&mut basic_block::Wrapper)>(f: F, expected: &str) {
        let mut ctx = Context::new();
        let void_ty = ctx.ty_storage.void_ty;
        let module_idx = ctx.create_module("test".into());
        let mut module = ctx.get_module(module_idx);
        let fn_idx = module.create_fn("test".into(), vec![], void_ty);
        let mut func = module.get_fn(fn_idx);
        let block_idx = func.create_block(None);

        f(&mut func.get_block(block_idx));
        let abi = sysv_amd64::SysVAmd64::new();
        let output = CodeGen::new(module, &abi).compile();
        let preamble = concat!(".section .text\n", ".global test\n", "test:\n", ".L_0_0:\n");
        let postamble = concat!("\tjmp .Ltest_ret\n", ".Ltest_ret:\n", "\tret\n");

        assert_eq!(
            std::str::from_utf8(&output).unwrap(),
            format!("{preamble}{expected}{postamble}")
        );
    }

    #[test]
    fn mov_location() -> Result<(), InvalidOperandSize> {
        let mut ctx = Context::new();
        let struct_ty = {
            let ty = ctx.ty_storage.add_ty(Ty::Struct(vec![
                ctx.ty_storage.i8_ty,
                ctx.ty_storage.i32_ty,
            ]));

            ctx.ty_storage
                .add_ty(Ty::Struct(vec![ctx.ty_storage.i8_ty, ty]))
        };

        let cases = [
            (
                (
                    &Location::Register(Register::Rdi),
                    &Location::Register(Register::Rsi),
                    ctx.ty_storage.i64_ty,
                ),
                "\tmov rsi, rdi\n",
            ),
            (
                (
                    &Location::Address {
                        effective_address: EffectiveAddress {
                            base: Base::Register(Register::Rbp),
                            index: None,
                            scale: None,
                            displacement: Some(Offset(-12)),
                        },
                        spilled: false,
                    },
                    &Location::Address {
                        effective_address: EffectiveAddress {
                            base: Base::Register(Register::Rbp),
                            index: None,
                            scale: None,
                            displacement: Some(Offset(-24)),
                        },
                        spilled: false,
                    },
                    struct_ty,
                ),
                "\tmov rcx, 12\n\tlea rsi, [rbp - 12]\n\tlea rdi, [rbp - 24]\n\trep movsb\n",
            ),
            (
                (
                    &Location::Address {
                        effective_address: EffectiveAddress {
                            base: Base::Register(Register::Rbp),
                            index: None,
                            scale: None,
                            displacement: Some(Offset(-8)),
                        },
                        spilled: true,
                    },
                    &Location::Register(Register::Rax),
                    ctx.ty_storage.ptr_ty,
                ),
                "\tmov rax, qword ptr [rbp - 8]\n",
            ),
        ];

        let module_idx = ctx.create_module("test".into());

        for ((src, dest, ty), expected) in cases {
            let abi = sysv_amd64::SysVAmd64::new();
            let mut codegen = CodeGen::new(ctx.get_module(module_idx), &abi);
            codegen.mov_location(src, dest, codegen.ty_size(ty))?;
            assert_eq!(codegen.text, expected);
        }

        Ok(())
    }

    #[test]
    fn sdiv() {
        let cases: &[(fn(&mut basic_block::Wrapper), &'static str)] = &[
            (
                |block: &mut basic_block::Wrapper| {
                    block.create_bin(
                        Operand::const_i8(-1i8 as u8, &block.ty_storage),
                        Operand::const_i8(8, &block.ty_storage),
                        BinOp::SDiv,
                    );
                },
                concat!(
                    "\tmov r15b, 8\n",
                    "\tmov al, 255\n",
                    "\tmovsx ax, al\n",
                    "\tidiv r15b\n",
                    "\tmov r15b, al\n",
                ),
            ),
            (
                |block: &mut basic_block::Wrapper| {
                    block.create_bin(
                        Operand::const_i16(i16::MIN as u16, &block.ty_storage),
                        Operand::const_i16(16, &block.ty_storage),
                        BinOp::SDiv,
                    );
                },
                concat!(
                    "\tmov r15w, 16\n",
                    "\tmov ax, 32768\n",
                    "\tcwd\n",
                    "\tidiv r15w\n",
                    "\tmov r15w, ax\n",
                ),
            ),
            (
                |block: &mut basic_block::Wrapper| {
                    block.create_bin(
                        Operand::const_i32(i32::MIN as u32, &block.ty_storage),
                        Operand::const_i32(32, &block.ty_storage),
                        BinOp::SDiv,
                    );
                },
                concat!(
                    "\tmov r15d, 32\n",
                    "\tmov eax, 2147483648\n",
                    "\tcdq\n",
                    "\tidiv r15d\n",
                    "\tmov r15d, eax\n",
                ),
            ),
            (
                |block: &mut basic_block::Wrapper| {
                    block.create_bin(
                        Operand::const_i64(i64::MIN as u64, &block.ty_storage),
                        Operand::const_i64(64, &block.ty_storage),
                        BinOp::SDiv,
                    );
                },
                concat!(
                    "\tmov r15, 64\n",
                    "\tmov rax, 9223372036854775808\n",
                    "\tcqo\n",
                    "\tidiv r15\n",
                    "\tmov r15, rax\n",
                ),
            ),
        ];

        for (f, expected) in cases {
            assert_generated_basic_block(f, expected);
        }
    }

    #[test]
    fn udiv() {
        let cases: &[(fn(&mut basic_block::Wrapper), &'static str)] = &[
            (
                |block: &mut basic_block::Wrapper| {
                    block.create_bin(
                        Operand::const_i8(u8::MAX, &block.ty_storage),
                        Operand::const_i8(8, &block.ty_storage),
                        BinOp::UDiv,
                    );
                },
                concat!(
                    "\tmov r15b, 8\n",
                    "\tmov al, 255\n",
                    "\tmovzx ax, al\n",
                    "\tidiv r15b\n",
                    "\tmov r15b, al\n",
                ),
            ),
            (
                |block: &mut basic_block::Wrapper| {
                    block.create_bin(
                        Operand::const_i16(u16::MAX, &block.ty_storage),
                        Operand::const_i16(16, &block.ty_storage),
                        BinOp::UDiv,
                    );
                },
                concat!(
                    "\tmov r15w, 16\n",
                    "\tmov ax, 65535\n",
                    "\txor dx, dx\n",
                    "\tidiv r15w\n",
                    "\tmov r15w, ax\n",
                ),
            ),
            (
                |block: &mut basic_block::Wrapper| {
                    block.create_bin(
                        Operand::const_i32(u32::MAX, &block.ty_storage),
                        Operand::const_i32(32, &block.ty_storage),
                        BinOp::UDiv,
                    );
                },
                concat!(
                    "\tmov r15d, 32\n",
                    "\tmov eax, 4294967295\n",
                    "\txor edx, edx\n",
                    "\tidiv r15d\n",
                    "\tmov r15d, eax\n",
                ),
            ),
            (
                |block: &mut basic_block::Wrapper| {
                    block.create_bin(
                        Operand::const_i64(u64::MAX, &block.ty_storage),
                        Operand::const_i64(64, &block.ty_storage),
                        BinOp::UDiv,
                    );
                },
                concat!(
                    "\tmov r15, 64\n",
                    "\tmov rax, 18446744073709551615\n",
                    "\txor rdx, rdx\n",
                    "\tidiv r15\n",
                    "\tmov r15, rax\n",
                ),
            ),
        ];

        for (f, expected) in cases {
            assert_generated_basic_block(f, expected);
        }
    }
}
