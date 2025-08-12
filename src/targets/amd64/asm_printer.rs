use crate::{
    Const, FunctionIdx, Global,
    mir::{Function, Module},
    targets::{Abi, Target, amd64::Opcode},
    ty::{self, Ty, TyIdx},
};
use std::fmt::Write;

pub struct AsmPrinter<'a, T: Target, W: Write> {
    pub target: &'a T,
    pub ty_storage: &'a ty::Storage,
    pub buf: &'a mut W,
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
            self.emit_fn(func, FunctionIdx(idx), module)?;
        }

        Ok(())
    }

    fn emit_const(&mut self, c: Const, ty: TyIdx, module: &Module) -> std::fmt::Result {
        match c {
            Const::Global(idx) => write!(self.buf, "\t.quad {}\n", module.globals[*idx].name)?,
            Const::Function(idx) => write!(self.buf, "\t.quad {}\n", module.functions[*idx].name)?,
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
            Const::Aggregate(consts) => {
                let fields = match self.ty_storage.get_ty(ty) {
                    Ty::Struct(fields) => fields,
                    _ => unreachable!(),
                }
                .clone();

                let mut last_offset_plus_size = 0;

                for (i, (c, ty)) in consts.into_iter().zip(fields.iter().cloned()).enumerate() {
                    let field_offset = self.target.abi().field_offset(self.ty_storage, &fields, i);

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
        write!(self.buf, ".global {}\n", func.name)?;
        write!(self.buf, "{}:\n", func.name)?;

        for (bb_idx, bb) in func.blocks.iter().enumerate() {
            write!(self.buf, ".L{fn_idx}_{bb_idx}:\n")?;

            for instr in &bb.instructions {
                write!(self.buf, "\t")?;

                Opcode::from(instr.opcode).write_instruction(
                    self,
                    module,
                    fn_idx,
                    &instr.operands,
                )?;

                write!(self.buf, "\n")?;
            }
        }

        Ok(())
    }
}
