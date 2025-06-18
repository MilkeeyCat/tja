pub mod allocator;
//pub mod calling_convention;
mod condition;
mod operands;
mod register;

use crate::{
    hir,
    mir::{
        Function, Instruction, Module, Operand, PhysicalRegister, Register, RegisterRole,
        StackFrameIdx, VregIdx,
    },
    targets::{Target, amd64},
};
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
                        Operand::Register(
                            Register::Physical(amd64::Register::Rbp as PhysicalRegister),
                            RegisterRole::Use,
                        ),
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
        //let vregs = allocate(self.target.register_info(), false, func);
        lower_stack_slots(func);
        let stack_slots = lower_stack_slots(func);

        write!(self.text, ".global {0}\n{0}:\n", func.name)?;

        //TODO: allocate the stack frame

        let mut fn_codegen = FunctionCodeGen {
            vregs: HashMap::new(),
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
    pub vregs: HashMap<VregIdx, allocator::Location>,
    pub stack_slots: HashMap<StackFrameIdx, Vec<Operand>>,
    pub globals: &'a [hir::Global],
    pub target: &'a dyn Target,
    pub text: &'a mut String,
}

impl<'a> FunctionCodeGen<'a> {
    fn emit_instruction(&mut self, instr: &Instruction) -> Result<(), std::fmt::Error> {
        write!(self.text, "\t")?;

        amd64::Opcode::from(instr.opcode).write_instruction(self, &instr.operands)?;

        write!(self.text, "\n")
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
