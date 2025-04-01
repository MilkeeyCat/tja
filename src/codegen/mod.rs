mod abi;
mod allocator;
mod operands;
mod register;

use crate::repr::{
    BlockIdx, Const, FunctionIdx, Global, Instruction, InstructionIdx, LocalIdx, LocalStorage,
    Module, Operand, Patch, Terminator, Wrapper,
    op::BinOp,
    ty::{Ty, TyIdx},
};
use abi::Abi;
use allocator::{Allocator, Location};
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
            Self::Const(c, _) => c.clone().into(),
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

pub struct CodeGen<'ctx> {
    module: Wrapper<'ctx, &'ctx mut Module>,
    locals: HashMap<LocalIdx, Variable>,
    globals: HashMap<Rc<Global>, Location>,
    spill_register: Register,
    bss: String,
    data: String,
    text: String,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(module: Wrapper<'ctx, &'ctx mut Module>) -> Self {
        Self {
            module,
            locals: HashMap::new(),
            globals: HashMap::new(),
            spill_register: Register::R8,
            bss: String::new(),
            data: String::new(),
            text: String::new(),
        }
    }

    fn canonicalize(&mut self) {
        for function in &mut self.module.functions {
            let mut patch = Patch::new(function);

            for (block_idx, block) in &mut function.blocks.iter_mut().enumerate() {
                for (instr_idx, instruction) in block.instructions.iter_mut().enumerate() {
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
                    }
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
                    | Instruction::GetElementPtr { .. } => (),
                }
            }

            match &block.terminator {
                Terminator::Return(operand) => {
                    if let Some(Operand::Local(idx)) = operand {
                        allocator.precolor(*idx, Register::Rax.into());
                    }
                }
                Terminator::Goto(_) => (),
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
                let ty = &self.locals[out].ty;
                let ty_size: OperandSize = self.ty_size(*ty).try_into().unwrap();

                self.copy(lhs, &self.locals[out].location.clone(), *ty)
                    .unwrap();

                match kind {
                    BinOp::Add => {
                        self.add(
                            &self.locals[out].location.to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::Sub => {
                        self.sub(
                            &self.locals[out].location.to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
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
                                self.sext(
                                    &dest.clone().into(),
                                    &dest.clone().resize(OperandSize::Word),
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
                                self.zext(
                                    &dest.clone().into(),
                                    &dest.clone().resize(OperandSize::Word),
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
                self.copy(value, &ptr.get_location(self).unwrap(), value.ty(self))
                    .unwrap();
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
                        Location::Register(_) => unreachable!(),
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
        }
    }

    fn terminator(&mut self, fn_idx: FunctionIdx, block_idx: BlockIdx, ret_label: &str) {
        match &self.module.functions[fn_idx].blocks[block_idx].terminator {
            Terminator::Goto(block_idx) => self.text.push_str(&format!(
                "\tjmp .L{}\n",
                self.module.functions[fn_idx].blocks[*block_idx].name
            )),
            Terminator::Return(_) => self.text.push_str(&format!("\tjmp {ret_label}\n")),
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
        self.precolor(&mut allocator, idx);

        let ret_label = format!(".L{}_ret", &self.module.functions[idx].name);
        let (locations, mut stack_frame_size) = allocator.allocate(self.module.ty_storage);
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
                &Source::Immediate(Immediate::Uint(stack_frame_size.next_multiple_of(16) as u64)),
            );
        }

        for block_idx in 0..self.module.functions[idx].blocks.len() {
            let block = &self.module.functions[idx].blocks[block_idx];
            self.text.push_str(&format!(".L{}:\n", &block.name));

            for instr_idx in 0..block.instructions.len() {
                self.instruction(idx, block_idx, instr_idx);
            }

            self.terminator(idx, block_idx, &ret_label);
        }

        self.text.push_str(&format!("{ret_label}:\n"));
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
            &Source::Immediate(Immediate::Uint(size as u64)),
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
            Operand::Global(_) | Operand::Local(_) => {
                self.mov_location(&src.get_location(self).unwrap(), dest, ty)?
            }
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
                &c.clone().into(),
                &dest.to_dest(self.ty_size(ty).try_into()?),
            ),
        };

        Ok(())
    }

    fn mov_location(
        &mut self,
        src: &Location,
        dest: &Location,
        ty: TyIdx,
    ) -> Result<(), InvalidOperandSize> {
        match (src, dest) {
            (
                Location::Address {
                    effective_address,
                    spilled: false,
                },
                dest,
            ) if ty == self.module.ty_storage.ptr_ty => {
                self.lea(
                    &dest.to_dest(OperandSize::Qword),
                    &effective_address.clone(),
                );
            }
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
                self.inline_memcpy(&src.clone(), &dest.clone(), self.ty_size(ty));
            }
            (lhs, rhs) => {
                let size = self.ty_size(ty).try_into()?;

                self.mov(&lhs.to_source(size), &rhs.to_dest(size));
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

    #[inline]
    fn field_offset(&self, fields: &[TyIdx], i: usize) -> usize {
        Abi::field_offset(self.module.ty_storage, fields, i)
    }

    #[inline]
    fn ty_size(&self, ty: TyIdx) -> usize {
        Abi::ty_size(self.module.ty_storage, ty)
    }

    #[inline]
    pub fn alignment(&self, ty: TyIdx) -> usize {
        Abi::alignment(self.module.ty_storage, ty)
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
    use crate::repr::{Context, ty::Ty};

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
                            displacement: Some(Offset(-6)),
                        },
                        spilled: false,
                    },
                    &Location::Register(Register::Rax),
                    ctx.ty_storage.ptr_ty,
                ),
                "\tlea rax, [rbp - 6]\n",
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
            let mut codegen = CodeGen::new(ctx.get_module(module_idx));
            codegen.mov_location(src, dest, ty)?;
            assert_eq!(codegen.text, expected);
        }

        Ok(())
    }
}

trait LocationStorage {
    fn get_global(&self, global: &Global) -> Location;
    fn get_local(&self, idx: LocalIdx) -> Location;
}

impl LocationStorage for CodeGen<'_> {
    fn get_local(&self, idx: LocalIdx) -> Location {
        self.locals[&idx].location.clone()
    }

    fn get_global(&self, global: &Global) -> Location {
        self.globals[global].clone()
    }
}

impl LocalStorage for CodeGen<'_> {
    fn get_local_ty(&self, idx: LocalIdx) -> TyIdx {
        self.locals[&idx].ty
    }
}
