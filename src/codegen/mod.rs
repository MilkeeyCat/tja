pub mod abi;
mod allocator;
pub mod calling_convention;
mod condition;
mod operands;
mod register;

use crate::repr::{
    BlockIdx, Branch, Const, FunctionIdx, Global, Instruction, InstructionIdx, LocalIdx,
    LocalStorage, Module, Operand, Patch, Terminator, Wrapper,
    op::BinOp,
    ty::{Ty, TyIdx},
};
use abi::Abi;
use allocator::{Allocator, Location};
use condition::Condition;
use operands::{
    Base, Destination, EffectiveAddress, Immediate, InvalidOperandSize, Offset, OperandSize, Source,
};
use register::Register;
use std::{collections::HashMap, rc::Rc};

impl Operand {
    fn get_location<T: LocationStorage>(&self, storage: &T) -> Option<Location> {
        match self {
            Self::Global(global) => Some(storage.get_global(global)),
            Self::Local(idx) => Some(storage.get_local(*idx)),
            Self::Const(_, _) => None,
        }
    }

    fn to_source<T: LocationStorage>(&self, storage: &T, size: OperandSize) -> Source {
        match self {
            Self::Global(global) => storage.get_global(&global).to_source(size),
            Self::Local(idx) => storage.get_local(*idx).to_source(size),
            Self::Const(c, _) => c.clone().try_into().unwrap(),
        }
    }

    fn to_dest<T: LocationStorage>(&self, storage: &T, size: OperandSize) -> Destination {
        match self {
            Self::Global(global) => storage.get_global(&global).to_dest(size),
            Self::Local(idx) => storage.get_local(*idx).to_dest(size),
            Self::Const(_, _) => unreachable!("can't turn const into Destination"),
        }
    }
}

#[derive(Debug)]
struct Variable {
    ty: TyIdx,
    location: Location,
}

#[derive(Debug)]
struct Argument {
    ty: TyIdx,
    locations: Vec<Location>,
}

pub struct CodeGen<'a, 'ctx> {
    module: Wrapper<'ctx, &'ctx mut Module>,
    locals: HashMap<LocalIdx, Variable>,
    arguments: HashMap<LocalIdx, Argument>,
    globals: HashMap<Rc<Global>, Location>,
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
            arguments: HashMap::new(),
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
                        Instruction::Icmp { .. } => (),
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
                        allocator.precolor(*out, Register::Rax.into());

                        let indices: Vec<_> = vec![lhs.local_idx(), rhs.local_idx(), Some(*out)]
                            .into_iter()
                            .flatten()
                            .collect();

                        if !indices.is_empty() {
                            let ty = self.module.functions[idx].locals[*out];
                            let rdx = allocator.create_node(ty);
                            allocator.precolor(rdx, Register::Rdx.into());

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
                            Location::Address {
                                effective_address: EffectiveAddress {
                                    base: Base::Register(Register::Rbp),
                                    index: None,
                                    scale: None,
                                    displacement: Some(Offset(
                                        -(allocator.stack_frame_size as isize),
                                    )),
                                },
                                spilled: false,
                            },
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
                }
            }

            match &block.terminator {
                Terminator::Return(operand) => {
                    if let Some(Operand::Local(idx)) = operand {
                        allocator.precolor(*idx, Register::Rax.into());
                    }
                }
                Terminator::Br(_) => (),
            };
        }
    }

    fn instruction(&mut self, fn_idx: FunctionIdx, block_idx: BlockIdx, instr_idx: InstructionIdx) {
        match &self.module.functions[fn_idx].blocks[block_idx].instructions[instr_idx].clone() {
            Instruction::Binary {
                kind,
                lhs,
                rhs,
                out,
            } => {
                let ty = self.locals[out].ty;
                let ty_size: OperandSize = self.ty_size(ty).try_into().unwrap();

                if kind != &BinOp::Sub {
                    self.copy(lhs, &self.locals[out].location.clone(), ty)
                        .unwrap();
                }

                match kind {
                    BinOp::Add => {
                        self.add(
                            &self.locals[out].location.to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::Sub => {
                        if let Operand::Const(Const::Int(0), _) = lhs {
                            self.copy(rhs, &self.locals[out].location.clone(), ty)
                                .unwrap();
                            self.neg(&self.locals[out].location.to_dest(ty_size));
                        } else {
                            self.copy(lhs, &self.locals[out].location.clone(), ty)
                                .unwrap();
                            self.sub(
                                &self.locals[out].location.to_dest(ty_size),
                                &rhs.to_source(self, ty_size),
                            );
                        }
                    }
                    BinOp::Mul => {
                        self.mul(
                            &self.locals[out].location.to_dest(ty_size),
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
                                let src = &self.locals[out].location;

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
                                let src = &self.locals[out].location;

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
                self.copy(
                    operand,
                    &self.locals[out].location.clone(),
                    self.locals[out].ty,
                )
                .unwrap();
            }
            Instruction::Sext { operand, out } => {
                let out = &self.locals[out];

                self.sext(
                    &operand.to_source(self, self.ty_size(operand.ty(self)).try_into().unwrap()),
                    &out.location
                        .to_dest(self.ty_size(out.ty).try_into().unwrap()),
                );
            }
            Instruction::Zext { operand, out } => {
                let out = &self.locals[out];

                self.zext(
                    &operand.to_source(self, self.ty_size(operand.ty(self)).try_into().unwrap()),
                    &out.location
                        .to_dest(self.ty_size(out.ty).try_into().unwrap()),
                );
            }
            Instruction::Alloca { .. } => (),
            Instruction::Store { ptr, value } => {
                let loc = match ptr.get_location(self).unwrap() {
                    Location::Register(r) => Location::Address {
                        effective_address: r.into(),
                        spilled: false,
                    },
                    loc => loc,
                };

                self.copy(value, &loc, value.ty(self)).unwrap();
            }
            Instruction::Load { ptr, out } => {
                let src = match ptr.get_location(self).unwrap() {
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

                match Operand::Local(*out).get_location(self).unwrap() {
                    Location::Register(r) => {
                        let operand_size = size.try_into().unwrap();

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
                    let mut ptr = match ptr.get_location(self).unwrap() {
                        Location::Address {
                            effective_address, ..
                        } => effective_address,
                        Location::Register(r) => r.into(),
                    };
                    let index = match operand {
                        Operand::Const(c, _) => c.usize_unchecked(),
                        Operand::Local(_) => unreachable!(),
                        Operand::Global(_) => unreachable!(),
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
                let size = self.ty_size(lhs.ty(self)).try_into().unwrap();

                self.cmp(&lhs.to_dest(self, size), &rhs.to_source(self, size));
                self.setcc(
                    &self.locals[out].location.to_dest(OperandSize::Byte),
                    kind.into(),
                );
            }
        }
    }

    fn terminator(&mut self, fn_idx: FunctionIdx, block_idx: BlockIdx) {
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
                    let size = self.ty_size(condition.ty(self)).try_into().unwrap();
                    let loc = condition.get_location(self).unwrap();

                    self.test(&loc.to_dest(size), &loc.to_source(size));
                    self.jcc(&self.block_label(fn_idx, *iftrue), Condition::NotEqual);
                    self.jmp(&self.block_label(fn_idx, *iffalse));
                }
                Branch::Unconditional { block_idx } => {
                    self.jmp(&self.block_label(fn_idx, *block_idx))
                }
            },
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
        self.arguments = self
            .abi
            .calling_convention()
            .compute(
                self,
                &mut allocator,
                &self.module.functions[idx].locals[self.module.functions[idx].params_count..],
            )
            .into_iter()
            .zip(&self.module.functions[idx].locals)
            .enumerate()
            .map(|(idx, (locations, ty))| (idx, Argument { ty: *ty, locations }))
            .collect();
        self.precolor(&mut allocator, idx);

        let (locations, mut stack_frame_size) = allocator.allocate(self);
        stack_frame_size = stack_frame_size.next_multiple_of(16);
        self.locals.extend(
            locations
                .into_iter()
                .zip(&self.module.functions[idx].locals)
                .enumerate()
                .map(|(i, (location, ty))| (i, Variable { ty: *ty, location })),
        );
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
                self.instruction(idx, block_idx, instr_idx);
            }

            self.terminator(idx, block_idx);
        }

        self.text
            .push_str(&format!(".L{}_ret:\n", &self.module.functions[idx].name));
        if stack_frame_size > 0 {
            self.text.push_str("\tleave\n");
        }
        self.text.push_str("\tret\n");
        self.locals
            .retain(|_, variable| matches!(variable.location, Location::Address { .. }));
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
                    &self.locals[&out].location.to_dest(OperandSize::Qword),
                    base,
                );
            }
            [operand, rest @ ..] => {
                let (ty, base) = match self.module.ty_storage.get_ty(ty) {
                    Ty::Struct(fields) => {
                        let index = match operand {
                            Operand::Const(c, _) => c.usize_unchecked(),
                            Operand::Global(_) => unreachable!(),
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
            .map(|global| {
                (
                    Rc::clone(global),
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

        for idx in 0..self.module.functions.len() {
            self.function(idx);
        }

        let mut result = String::new();

        if !self.bss.is_empty() {
            result.push_str(".section .bss\n");
            result.push_str(&self.bss);
        }
        if !self.data.is_empty() {
            result.push_str(".section .data\n");
            result.push_str(&self.data);
        }
        if !self.text.is_empty() {
            result.push_str(".section .text\n");
            result.push_str(&self.text);
        }

        result.into_bytes()
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
            Operand::Local(idx) if *idx < self.arguments.len() => {
                unimplemented!("argument copy");
            }
            Operand::Global(_) | Operand::Local(_) => match &src.get_location(self).unwrap() {
                Location::Address {
                    effective_address,
                    spilled: false,
                } if ty == self.module.ty_storage.ptr_ty => {
                    self.lea(
                        &dest.to_dest(OperandSize::Qword),
                        &effective_address.clone(),
                    );
                }
                location => self.mov_location(location, dest, self.ty_size(ty))?,
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
                &c.clone().try_into().unwrap(),
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

trait LocationStorage {
    fn get_global(&self, global: &Global) -> Location;
    fn get_local(&self, idx: LocalIdx) -> Location;
}

impl LocationStorage for CodeGen<'_, '_> {
    fn get_local(&self, idx: LocalIdx) -> Location {
        self.locals[&idx].location.clone()
    }

    fn get_global(&self, global: &Global) -> Location {
        self.globals[global].clone()
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
        CodeGen,
        allocator::Location,
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
