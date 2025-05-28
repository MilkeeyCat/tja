mod allocator;
//pub mod calling_convention;
mod condition;
mod operands;
mod register;

use crate::{
    hir,
    mir::{Function, Instruction, Module, Operand, Register, StackFrameIdx, VregIdx},
    targets::{
        Target,
        amd64::{self, Opcode},
    },
};
use allocator::Allocator;
use derive_more::Display;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Copy, Clone, Debug, Display)]
pub enum OperandSize {
    #[display("byte ptr")]
    Byte = 1,
    #[display("word ptr")]
    Word = 2,
    #[display("dword ptr")]
    Dword = 4,
    #[display("qword ptr")]
    Qword = 8,
}

fn lower_stack_slots(func: &Function) -> HashMap<StackFrameIdx, Vec<Operand>> {
    let mut locations = HashMap::new();
    let mut stack_offset: isize = 0;

    for bb in &func.blocks {
        for instr in &bb.instructions {
            for idx in instr.operands.iter().filter_map(|operand| {
                if let Operand::Frame(idx) = operand {
                    Some(idx)
                } else {
                    None
                }
            }) {
                stack_offset += func.stack_slots[idx] as isize;

                locations.insert(
                    *idx,
                    vec![
                        Operand::Reg(amd64::Register::Rbp as Register),
                        Operand::Immediate(0),
                        Operand::Immediate(0),
                        Operand::Immediate(-stack_offset as u64),
                    ],
                );
            }
        }
    }

    locations
}

struct ModuleCodeGen<'a> {
    target: &'a dyn Target,
    globals: &'a [hir::Global],
    text: String,
}

impl<'a> ModuleCodeGen<'a> {
    fn new(target: &'a dyn Target, globals: &'a [hir::Global]) -> Self {
        Self {
            target,
            globals,
            text: String::new(),
        }
    }

    fn emit_function(&mut self, func: &mut Function) -> Result<(), std::fmt::Error> {
        let vregs =
            Allocator::new(self.target.register_info(), HashMap::new(), false, func).allocate();
        lower_stack_slots(func);
        let stack_slots = lower_stack_slots(func);

        write!(self.text, ".global {0}\n{0}:\n", func.name)?;

        //TODO: allocate the stack frame

        let mut fn_codegen = FunctionCodeGen {
            vregs,
            stack_slots,
            target: self.target,
            text: &mut self.text,
            globals: self.globals,
        };

        for bb in &func.blocks {
            for instr in &bb.instructions {
                fn_codegen.emit_instruction(instr)?;
            }
        }

        //TODO: epilogue

        Ok(())
    }
}

pub struct FunctionCodeGen<'a> {
    vregs: HashMap<VregIdx, allocator::Location>,
    stack_slots: HashMap<StackFrameIdx, Vec<Operand>>,
    globals: &'a [hir::Global],
    target: &'a dyn Target,
    pub text: &'a mut String,
}

impl<'a> FunctionCodeGen<'a> {
    fn emit_instruction(&mut self, instr: &Instruction) -> Result<(), std::fmt::Error> {
        write!(self.text, "\t")?;

        amd64::Opcode::from(instr.opcode).write_instruction(self, &instr.operands)?;

        write!(self.text, "\n")
    }

    pub fn stringify_operand(&mut self, operands: &[Operand]) -> Result<String, std::fmt::Error> {
        match operands {
            [Operand::Vreg(idx, _)] => match self.vregs[idx] {
                allocator::Location::Register(r) => {
                    Ok(self.target.register_info().get_name(&r).to_string())
                }
                allocator::Location::Spill(idx) => {
                    self.stringify_operand(&self.stack_slots[&idx].clone())
                }
            },
            [Operand::Reg(r)] => Ok(self.target.register_info().get_name(r).to_string()),
            [Operand::Frame(idx)] => self.stringify_operand(&self.stack_slots[idx].clone()),
            [
                base,
                index,
                Operand::Immediate(scale),
                Operand::Immediate(displacement),
            ] => {
                let mut result = String::from("[");

                match base {
                    Operand::Reg(r) => {
                        result.push_str(self.target.register_info().get_name(r));
                    }
                    Operand::Vreg(idx, _) => match self.vregs[idx] {
                        allocator::Location::Register(r) => {
                            write!(&mut result, "{}", self.target.register_info().get_name(&r))?
                        }
                        allocator::Location::Spill(_) => unreachable!(),
                    },
                    Operand::Global(idx) => write!(&mut result, "{}", self.globals[*idx].name)?,
                    _ => unreachable!(),
                };

                match index {
                    Operand::Reg(r) => {
                        write!(
                            &mut result,
                            " + {}",
                            self.target.register_info().get_name(r)
                        )?;
                    }
                    Operand::Vreg(idx, _) => match self.vregs[idx] {
                        allocator::Location::Register(r) => write!(
                            &mut result,
                            " + {}",
                            self.target.register_info().get_name(&r)
                        )?,
                        allocator::Location::Spill(_) => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                if *scale > 0 {
                    write!(&mut result, "* {}", *scale)?;
                }

                let displacement = *displacement as i64;
                if displacement != 0 {
                    if displacement > 0 {
                        write!(&mut result, "+")?;
                    } else {
                        write!(&mut result, "-")?;
                    }

                    write!(&mut result, "{displacement}")?;
                }

                write!(&mut result, "]")?;

                Ok(result)
            }
            [Operand::Immediate(imm)] => Ok(imm.to_string()),
            operands => unreachable!("{:?}", operands),
        }
    }
}

pub fn generate(
    module: &mut Module,
    target: &dyn Target,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let mut codegen = ModuleCodeGen::new(target, module.globals);

    //TODO: globals

    for func in &mut module.functions {
        codegen.emit_function(func)?;
    }

    let mut result = String::new();

    if !codegen.text.is_empty() {
        result.push_str(".text\n");
        result.push_str(&codegen.text);
    }

    Ok(result.into_bytes())
}
