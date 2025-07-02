use crate::{
    hir::FunctionIdx,
    mir::{Function, Module},
    targets::{Target, amd64::Opcode},
};
use std::fmt::Write;

pub struct AsmPrinter<'a, T: Target, W: Write> {
    pub target: &'a T,
    pub buf: &'a mut W,
}

impl<'a, T: Target, W: Write> AsmPrinter<'a, T, W> {
    pub fn new(target: &'a T, buf: &'a mut W) -> Self {
        Self { target, buf }
    }

    pub fn emit(mut self, module: &Module) -> Result<(), std::fmt::Error> {
        for (idx, func) in module.functions.iter().enumerate() {
            self.emit_fn(func, idx, module)?;
        }

        Ok(())
    }

    fn emit_fn(
        &mut self,
        func: &Function,
        fn_idx: FunctionIdx,
        module: &Module,
    ) -> Result<(), std::fmt::Error> {
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
