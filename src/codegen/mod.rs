mod allocator;
//pub mod calling_convention;
mod condition;
mod operands;
mod register;

use crate::{
    mir::{Function, Instruction, Module, Operand, Register, StackFrameIdx, VregIdx},
    targets::{
        Target,
        amd64::{self, Opcode},
    },
};
use allocator::Allocator;
use std::collections::HashMap;

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
    text: String,
}

impl<'a> ModuleCodeGen<'a> {
    fn new(target: &'a dyn Target) -> Self {
        Self {
            target,
            text: String::new(),
        }
    }

    fn emit_function(&mut self, func: &mut Function) {
        let vregs =
            Allocator::new(self.target.register_info(), HashMap::new(), false, func).allocate();
        lower_stack_slots(func);
        let stack_slots = lower_stack_slots(func);
        let mut fn_codegen = FunctionCodeGen {
            vregs,
            stack_slots,
            text: &mut self.text,
        };

        //TODO: prologue

        for bb in &func.blocks {
            for instr in &bb.instructions {
                fn_codegen.emit_instruction(instr);
            }
        }

        //TODO: epilogue
    }
}

#[derive(Debug)]
struct FunctionCodeGen<'a> {
    vregs: HashMap<VregIdx, allocator::Location>,
    stack_slots: HashMap<StackFrameIdx, Vec<Operand>>,
    text: &'a mut String,
}

impl<'a> FunctionCodeGen<'a> {
    fn emit_instruction(&mut self, instr: &Instruction) {
        match Opcode::from(instr.opcode) {
            Opcode::Add => unimplemented!(),
            Opcode::Sub => unimplemented!(),
            Opcode::Imul => unimplemented!(),
            Opcode::Idiv => unimplemented!(),
            Opcode::Mov => unimplemented!(),
            Opcode::Lea => unimplemented!(),
            Opcode::Jmp => unimplemented!(),
            Opcode::Test => unimplemented!(),
            Opcode::Jcc => unimplemented!(),
            Opcode::Num => unreachable!(),
        }
    }
}

pub fn generate(module: &mut Module, target: &dyn Target) -> Vec<u8> {
    let mut codegen = ModuleCodeGen::new(target);

    //TODO: globals

    for func in &mut module.functions {
        codegen.emit_function(func);
    }

    let mut result = String::new();

    if !codegen.text.is_empty() {
        result.push_str(".text\n");
        result.push_str(&codegen.text);
    }

    result.into_bytes()
}
