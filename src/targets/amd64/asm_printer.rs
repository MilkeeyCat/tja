use crate::{
    Const, FunctionIdx, Global,
    mir::{self, Function, Instruction, Module, Operand},
    targets::{Abi, RegisterInfo, Target, amd64::Opcode},
    ty::{self, Ty, TyIdx},
};
use std::fmt::Write;

pub struct AsmPrinter<'a, T: Target, W: Write> {
    pub target: &'a T,
    pub ty_storage: &'a ty::Storage,
    pub buf: &'a mut W,
}

include!(concat!(env!("OUT_DIR"), "/amd64/asm_printer.rs"));

macro_rules! emit_memory {
    ($size: ident) => {
        paste::item! {
            fn [< emit_ $size _memory >](&mut self, module: &Module, operands: &[mir::Operand]) -> std::fmt::Result {
                write!(self.buf, stringify!([< $size >] ptr))?;

                self.emit_address(module, operands)
            }
        }
    };
}

impl<'a, T: Target, W: Write> AsmPrinter<'a, T, W> {
    pub fn new(target: &'a T, ty_storage: &'a ty::Storage, buf: &'a mut W) -> Self {
        Self {
            target,
            ty_storage,
            buf,
        }
    }

    pub fn emit(mut self, module: &Module) -> std::fmt::Result {
        for global in &module.globals {
            self.emit_global(global, module)?;
        }

        write!(self.buf, ".section .text\n")?;

        for (idx, func) in module.functions.iter().enumerate() {
            self.emit_fn(func, idx.into(), module)?;
        }

        Ok(())
    }

    fn emit_operand(&mut self, _module: &Module, operands: &[mir::Operand]) -> std::fmt::Result {
        match operands {
            [Operand::Register(mir::Register::Physical(reg), _)] => {
                write!(self.buf, "{}", self.target.register_info().get_name(reg))
            }
            [Operand::Immediate(value)] => {
                write!(self.buf, "{value}")
            }
            _ => unreachable!(),
        }
    }

    fn emit_address(&mut self, module: &Module, operands: &[mir::Operand]) -> std::fmt::Result {
        match operands {
            [
                base,
                index,
                Operand::Immediate(scale),
                Operand::Immediate(displacement),
            ] => {
                match base {
                    Operand::Register(_, _) => self.emit_operand(module, &[base.clone()])?,
                    Operand::Global(idx) => write!(&mut self.buf, "{}", module.globals[*idx].name)?,
                    Operand::Function(idx) => {
                        write!(&mut self.buf, "{}", module.functions[*idx].name)?
                    }
                    _ => unreachable!(),
                };

                match index {
                    Operand::Register(_, _) => {
                        write!(&mut self.buf, "+ ")?;

                        self.emit_operand(module, &[index.clone()])?;
                    }
                    Operand::Immediate(0) => (),
                    _ => unreachable!(),
                };

                if *scale > 1 {
                    write!(&mut self.buf, "* {}", *scale)?;
                }

                let displacement = *displacement as i64;
                if displacement != 0 {
                    if displacement > 0 {
                        write!(&mut self.buf, "+")?;
                    } else {
                        write!(&mut self.buf, "-")?;
                    }

                    write!(&mut self.buf, "{}", displacement.abs())?;
                }

                write!(&mut self.buf, "]")
            }
            _ => unreachable!(),
        }
    }

    emit_memory! {byte}
    emit_memory! {word}
    emit_memory! {dword}
    emit_memory! {qword}

    fn emit_const(&mut self, c: Const, ty: TyIdx, module: &Module) -> std::fmt::Result {
        match c {
            Const::Global(idx) => write!(self.buf, "\t.quad {}\n", module.globals[idx].name)?,
            Const::Function(idx) => write!(self.buf, "\t.quad {}\n", module.functions[idx].name)?,
            Const::Int(value) => {
                let prefix = match self.ty_storage.get_ty(ty) {
                    Ty::I8 => ".byte",
                    Ty::I16 => ".short",
                    Ty::I32 => ".long",
                    Ty::I64 => ".quad",
                    _ => unreachable!(),
                };

                write!(self.buf, "\t{prefix} {value}\n")?;
            }
            Const::Aggregate(consts) => match self.ty_storage.get_ty(ty).clone() {
                Ty::Struct(fields) => {
                    let mut last_offset_plus_size = 0;

                    for (i, (c, ty)) in consts.into_iter().zip(fields.iter().cloned()).enumerate() {
                        let field_offset =
                            self.target.abi().field_offset(self.ty_storage, &fields, i);

                        if field_offset > last_offset_plus_size {
                            write!(
                                self.buf,
                                "\t.zero {}\n",
                                field_offset - last_offset_plus_size
                            )?;
                        }

                        self.emit_const(c, ty, module)?;
                        last_offset_plus_size =
                            field_offset + self.target.abi().ty_size(self.ty_storage, ty);
                    }
                }
                Ty::Array { ty, len } => {
                    assert_eq!(consts.len(), len);

                    for c in consts {
                        self.emit_const(c, ty, module)?;
                    }
                }
                _ => unreachable!(),
            },
        };

        Ok(())
    }

    fn emit_global(&mut self, global: &Global, module: &Module) -> std::fmt::Result {
        if let Some(value) = &global.value {
            write!(self.buf, ".section .data\n")?;
            write!(self.buf, ".global {}\n", global.name)?;
            write!(self.buf, "{}:\n", global.name)?;

            self.emit_const(value.clone(), global.ty, module)?;
        } else {
            write!(self.buf, ".section .bss\n")?;
            write!(self.buf, ".global {}\n", global.name)?;
            write!(self.buf, "{}:\n", global.name)?;
            write!(
                self.buf,
                "\t.zero {}\n",
                self.target.abi().ty_size(self.ty_storage, global.ty)
            )?;
        }

        Ok(())
    }

    fn emit_fn(
        &mut self,
        func: &Function,
        fn_idx: FunctionIdx,
        module: &Module,
    ) -> std::fmt::Result {
        if func.is_declaration() {
            return Ok(());
        }

        write!(self.buf, ".global {}\n", func.name)?;
        write!(self.buf, "{}:\n", func.name)?;

        for (bb_idx, bb) in func.blocks.iter().enumerate() {
            write!(self.buf, ".L{}_{bb_idx}:\n", fn_idx.raw())?;

            for instr in &bb.instructions {
                write!(self.buf, "\t")?;

                Opcode::from(instr.opcode).write_instruction(
                    self,
                    module,
                    fn_idx,
                    instr.operands.as_raw_slice(),
                )?;

                write!(self.buf, "\n")?;
            }
        }

        Ok(())
    }
}
