use crate::{
    hir::Global,
    mir::{Function, Module},
    targets::{Target, amd64::Opcode},
};
use std::fmt::Write;

pub struct AsmPrinter<'a, W: Write> {
    pub target: &'a dyn Target,
    pub buf: &'a mut W,
}

impl<'a, W: Write> AsmPrinter<'a, W> {
    pub fn new(target: &'a dyn Target, buf: &'a mut W) -> Self {
        Self { target, buf }
    }

    pub fn emit(mut self, module: &Module) -> Result<(), std::fmt::Error> {
        for func in &module.functions {
            self.emit_fn(func, module.globals)?;
        }

        Ok(())
    }

    fn emit_fn(&mut self, func: &Function, globals: &[Global]) -> Result<(), std::fmt::Error> {
        write!(self.buf, ".global {}\n", func.name)?;
        write!(self.buf, "{}:\n", func.name)?;

        for bb in &func.blocks {
            for instr in &bb.instructions {
                write!(self.buf, "\t")?;

                Opcode::from(instr.opcode)
                    .write_instruction(self, globals, &instr.operands)
                    .unwrap();

                write!(self.buf, "\n")?;
            }
        }

        Ok(())
    }
}
