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

struct FunctionCodeGen<'a> {
    vregs: HashMap<VregIdx, allocator::Location>,
    stack_slots: HashMap<StackFrameIdx, Vec<Operand>>,
    globals: &'a [hir::Global],
    target: &'a dyn Target,
    text: &'a mut String,
}

impl<'a> FunctionCodeGen<'a> {
    fn emit_instruction(&mut self, instr: &Instruction) -> Result<(), std::fmt::Error> {
        match Opcode::from(instr.opcode) {
            opcode @ (Opcode::Add8rr
            | Opcode::Add8rm
            | Opcode::Add8mr
            | Opcode::Add8mi
            | Opcode::Add8ri
            | Opcode::Add16rr
            | Opcode::Add16rm
            | Opcode::Add16mr
            | Opcode::Add16mi
            | Opcode::Add16ri
            | Opcode::Add32rr
            | Opcode::Add32rm
            | Opcode::Add32mr
            | Opcode::Add32mi
            | Opcode::Add32ri
            | Opcode::Add64rr
            | Opcode::Add64rm
            | Opcode::Add64mr
            | Opcode::Add64mi
            | Opcode::Add64ri) => {
                write!(self.text, "\tadd ")?;

                let (src, dest) = match opcode {
                    Opcode::Add8rr
                    | Opcode::Add8rm
                    | Opcode::Add8ri
                    | Opcode::Add16rr
                    | Opcode::Add16rm
                    | Opcode::Add16ri
                    | Opcode::Add32rr
                    | Opcode::Add32rm
                    | Opcode::Add32ri
                    | Opcode::Add64rr
                    | Opcode::Add64rm
                    | Opcode::Add64ri => instr.operands.split_at(1),
                    Opcode::Add8mr
                    | Opcode::Add8mi
                    | Opcode::Add16mr
                    | Opcode::Add16mi
                    | Opcode::Add32mr
                    | Opcode::Add32mi
                    | Opcode::Add64mr
                    | Opcode::Add64mi => instr.operands.split_at(instr.operands.len() - 1),
                    _ => unreachable!(),
                };
                let size = match opcode {
                    Opcode::Add8rr
                    | Opcode::Add8rm
                    | Opcode::Add8mr
                    | Opcode::Add8mi
                    | Opcode::Add8ri => OperandSize::Byte,
                    Opcode::Add16rr
                    | Opcode::Add16rm
                    | Opcode::Add16mr
                    | Opcode::Add16mi
                    | Opcode::Add16ri => OperandSize::Word,
                    Opcode::Add32rr
                    | Opcode::Add32rm
                    | Opcode::Add32mr
                    | Opcode::Add32mi
                    | Opcode::Add32ri => OperandSize::Dword,
                    Opcode::Add64rr
                    | Opcode::Add64rm
                    | Opcode::Add64mr
                    | Opcode::Add64mi
                    | Opcode::Add64ri => OperandSize::Qword,
                    _ => unreachable!(),
                };

                self.write_operand(src, size)?;
                write!(self.text, ", ")?;
                self.write_operand(dest, size)?;
                write!(self.text, "\n")
            }
            Opcode::Sub => unimplemented!(),
            Opcode::Imul => unimplemented!(),
            Opcode::Idiv => unimplemented!(),
            opcode @ (Opcode::Mov8rr
            | Opcode::Mov8rm
            | Opcode::Mov8mr
            | Opcode::Mov8mi
            | Opcode::Mov8ri
            | Opcode::Mov16rr
            | Opcode::Mov16rm
            | Opcode::Mov16mr
            | Opcode::Mov16mi
            | Opcode::Mov16ri
            | Opcode::Mov32rr
            | Opcode::Mov32rm
            | Opcode::Mov32mr
            | Opcode::Mov32mi
            | Opcode::Mov32ri
            | Opcode::Mov64rr
            | Opcode::Mov64rm
            | Opcode::Mov64mr
            | Opcode::Mov64mi
            | Opcode::Mov64ri) => {
                write!(self.text, "\tmov ")?;

                let (src, dest) = match opcode {
                    Opcode::Mov8rr
                    | Opcode::Mov8rm
                    | Opcode::Mov8ri
                    | Opcode::Mov16rr
                    | Opcode::Mov16rm
                    | Opcode::Mov16ri
                    | Opcode::Mov32rr
                    | Opcode::Mov32rm
                    | Opcode::Mov32ri
                    | Opcode::Mov64rr
                    | Opcode::Mov64rm
                    | Opcode::Mov64ri => instr.operands.split_at(1),
                    Opcode::Mov8mr
                    | Opcode::Mov8mi
                    | Opcode::Mov16mr
                    | Opcode::Mov16mi
                    | Opcode::Mov32mr
                    | Opcode::Mov32mi
                    | Opcode::Mov64mr
                    | Opcode::Mov64mi => instr.operands.split_at(instr.operands.len() - 1),
                    _ => unreachable!(),
                };
                let size = match opcode {
                    Opcode::Mov8rr
                    | Opcode::Mov8rm
                    | Opcode::Mov8mr
                    | Opcode::Mov8mi
                    | Opcode::Mov8ri => OperandSize::Byte,
                    Opcode::Mov16rr
                    | Opcode::Mov16rm
                    | Opcode::Mov16mr
                    | Opcode::Mov16mi
                    | Opcode::Mov16ri => OperandSize::Word,
                    Opcode::Mov32rr
                    | Opcode::Mov32rm
                    | Opcode::Mov32mr
                    | Opcode::Mov32mi
                    | Opcode::Mov32ri => OperandSize::Dword,
                    Opcode::Mov64rr
                    | Opcode::Mov64rm
                    | Opcode::Mov64mr
                    | Opcode::Mov64mi
                    | Opcode::Mov64ri => OperandSize::Qword,
                    _ => unreachable!(),
                };

                self.write_operand(src, size)?;
                write!(self.text, ", ")?;
                self.write_operand(dest, size)?;
                write!(self.text, "\n")
            }
            Opcode::Lea => unimplemented!(),
            Opcode::Jmp => unimplemented!(),
            Opcode::Test => unimplemented!(),
            Opcode::Jcc => unimplemented!(),
            Opcode::Num => unreachable!(),
        }
    }

    fn write_operand(
        &mut self,
        operands: &[Operand],
        size: OperandSize,
    ) -> Result<(), std::fmt::Error> {
        match operands {
            [Operand::Vreg(idx, _)] => match self.vregs[idx] {
                allocator::Location::Register(r) => {
                    write!(self.text, "{}", self.target.register_info().get_name(&r))
                }
                allocator::Location::Spill(idx) => {
                    self.write_operand(&self.stack_slots[&idx].clone(), size)
                }
            },
            [Operand::Reg(r)] => {
                write!(self.text, "{}", self.target.register_info().get_name(r))
            }
            [Operand::Frame(idx)] => self.write_operand(&self.stack_slots[idx].clone(), size),
            [
                base,
                index,
                Operand::Immediate(scale),
                Operand::Immediate(displacement),
            ] => {
                write!(self.text, "{size} [")?;

                match base {
                    Operand::Reg(r) => {
                        write!(self.text, "{}", self.target.register_info().get_name(r))?;
                    }
                    Operand::Vreg(idx, _) => match self.vregs[idx] {
                        allocator::Location::Register(r) => {
                            write!(self.text, "{}", self.target.register_info().get_name(&r))?
                        }
                        allocator::Location::Spill(_) => unreachable!(),
                    },
                    Operand::Global(idx) => write!(self.text, "{}", self.globals[*idx].name)?,
                    _ => unreachable!(),
                };

                match index {
                    Operand::Reg(r) => {
                        write!(self.text, "{}", self.target.register_info().get_name(r))?;
                    }
                    Operand::Vreg(idx, _) => match self.vregs[idx] {
                        allocator::Location::Register(r) => {
                            write!(self.text, "{}", self.target.register_info().get_name(&r))?
                        }
                        allocator::Location::Spill(_) => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                if *scale > 0 {
                    write!(self.text, "* {}", *scale)?;
                }

                let displacement = *displacement as i64;
                if displacement != 0 {
                    if displacement > 0 {
                        write!(self.text, "+")?;
                    } else {
                        write!(self.text, "-")?;
                    }

                    write!(self.text, "{displacement}")?;
                }

                write!(self.text, "]")
            }
            [Operand::Immediate(imm)] => {
                write!(self.text, "{imm}")
            }
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
