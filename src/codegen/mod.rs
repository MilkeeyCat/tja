mod allocator;
mod operands;
mod register;

use crate::repr::{
    op::BinOp, Function, Instruction, Operand, Place, Program, Terminator, ValueTree,
};
use allocator::Allocator;
use operands::{Destination, OperandSize, Source};
use register::Register;

impl Place {
    fn to_source(&self, codegen: &CodeGen) -> Source {
        match self {
            Place::Register(r) => codegen.locations[*r].clone().into(),
            Place::Global(_) => unimplemented!(),
        }
    }

    fn to_dest(&self, codegen: &CodeGen) -> Destination {
        match self {
            Place::Register(r) => codegen.locations[*r].clone().into(),
            Place::Global(_) => unimplemented!(),
        }
    }
}

impl Operand {
    fn to_source(&self, codegen: &CodeGen) -> Source {
        match self {
            Self::Place(place) => place.to_source(codegen),
            Self::Const(val) => match val {
                ValueTree::Leaf(leaf) => leaf.clone().into(),
                ValueTree::Branch(_) => unimplemented!(),
            },
        }
    }

    fn to_dest(&self, codegen: &CodeGen) -> Destination {
        match self {
            Self::Place(place) => place.to_dest(codegen),
            Self::Const(_) => unreachable!(),
        }
    }
}

pub struct CodeGen<'a> {
    program: &'a Program,
    locations: Vec<Destination>,
    bss: String,
    data: String,
    text: String,
}

impl<'a> CodeGen<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            locations: Vec::new(),
            bss: String::new(),
            data: String::new(),
            text: String::new(),
        }
    }

    fn precolor(allocator: &mut Allocator, function: &Function) {
        for block in &function.blocks {
            match &block.terminator {
                Terminator::Return(operand) => {
                    if let Some(Operand::Place(Place::Register(r))) = operand {
                        allocator.precolor(*r, Register::Rax.into());
                    }
                }
                Terminator::Goto(_) => (),
            };
        }
    }

    fn instruction(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Binary {
                kind,
                lhs,
                rhs,
                place,
            } => {
                //self.mov(&lhs.into(), &place.into(), false);

                match kind {
                    BinOp::Add => {}
                    BinOp::Sub => {}
                };
            }
            Instruction::Copy { place, operand } => {
                match place {
                    Place::Register(r) => {
                        self.mov(
                            &operand.to_source(self),
                            &self.locations[*r].clone().into(),
                            false,
                        );
                    }
                    Place::Global(_) => todo!(),
                };
            }
        }
    }

    fn terminator(&mut self, terminator: &Terminator) {
        match terminator {
            Terminator::Goto(block_id) => {}
            Terminator::Return(_) => self.text.push_str("\tret"),
        }
    }

    fn function(&mut self, function: &Function) {
        let mut allocator = Allocator::new(
            function.registers.len(),
            function.interference(),
            vec![
                Register::R15,
                Register::R14,
                Register::R13,
                Register::R12,
                Register::R11,
                Register::R10,
                Register::R9,
                Register::R8,
                Register::Rcx,
                Register::Rdx,
                Register::Rsi,
                Register::Rdi,
            ],
        );
        Self::precolor(&mut allocator, function);

        self.locations = allocator.allocate();
        self.text
            .push_str(&format!(".global {0}\n{0}:\n", &function.name));

        for block in &function.blocks {
            for instruction in &block.instructions {
                self.instruction(instruction);
            }

            self.terminator(&block.terminator);
        }
    }

    pub fn compile(mut self) -> Vec<u8> {
        for function in &self.program.functions {
            self.function(function)
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

    fn mov(&mut self, src: &Source, dest: &Destination, signed: bool) {
        match (dest, src) {
            (Destination::Memory(_), Source::Memory(_)) => {
                unreachable!("Can't move memory to memory")
            }
            (dest, src) => {
                let dest_size = dest.size();
                let src_size = src.size().unwrap_or(OperandSize::Qword);

                if dest_size == OperandSize::Qword && src_size == OperandSize::Dword {
                    // on x86_64 you can move 32 bit value in 64 bit register, and upper 32 bits of the register will be zeroed
                    self.mov(src, &Register::Eax.into(), false);

                    if signed {
                        self.text.push_str("\tcdqe\n");
                    }

                    self.mov(&Register::Rax.into(), dest, false);
                } else if dest_size > src_size {
                    if signed {
                        self.text.push_str(&format!("\tmovsx {dest}, {src}\n"));
                    } else {
                        self.text.push_str(&format!("\tmovzx {dest}, {src}\n"));
                    }
                } else {
                    self.text.push_str(&format!("\tmov {dest}, {src}\n"));
                }
            }
        }
    }
}
