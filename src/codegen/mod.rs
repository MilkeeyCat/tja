mod allocator;
mod operands;
mod register;

use crate::repr::{
    self, BasicBlock, Function, Instruction, Operand, Place, Program, Terminator, ValueTree,
    op::BinOp,
};
use allocator::{Allocator, Location};
use operands::{Destination, OperandSize, Source};
use register::Register;

impl Place {
    fn location<'c>(&self, codegen: &'c CodeGen) -> &'c Location {
        match self {
            Place::Register(r) => &codegen.locations[*r],
            Place::Global(_) => unimplemented!(),
        }
    }
}

impl Operand {
    fn to_source(&self, codegen: &CodeGen, size: OperandSize) -> Source {
        match self {
            Self::Place(place) => place.location(codegen).to_source(size),
            Self::Const(val) => match val {
                ValueTree::Leaf(leaf) => leaf.clone().into(),
                ValueTree::Branch(_) => unimplemented!(),
            },
        }
    }

    fn to_dest(&self, codegen: &CodeGen, size: OperandSize) -> Destination {
        match self {
            Self::Place(place) => place.location(codegen).to_dest(size),
            Self::Const(_) => unreachable!(),
        }
    }
}

pub struct CodeGen {
    locations: Vec<Location>,
    bss: String,
    data: String,
    text: String,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            locations: Vec::new(),
            bss: String::new(),
            data: String::new(),
            text: String::new(),
        }
    }

    fn canonicalize(program: &mut Program) {
        for function in &mut program.functions {
            for block in &mut function.blocks {
                let mut instructions = Vec::new();

                for (i, instruction) in block.instructions.iter_mut().enumerate() {
                    match instruction {
                        Instruction::Binary {
                            kind: BinOp::Div,
                            rhs,
                            place: Place::Register(r),
                            ty,
                            ..
                        } => {
                            if matches!(rhs, Operand::Const(..)) {
                                let r = function.registers.len();
                                function.registers.push(repr::Register {
                                    name: r.to_string(),
                                    ty: ty.clone(),
                                });
                                let place = Place::Register(r);
                                let mut operand = Operand::Place(place.clone());

                                std::mem::swap(rhs, &mut operand);
                                instructions.push((
                                    i,
                                    Instruction::Copy {
                                        ty: ty.clone(),
                                        place,
                                        operand,
                                    },
                                ));
                            }

                            let mut new_r = function.registers.len();
                            function.registers.push(repr::Register {
                                name: new_r.to_string(),
                                ty: ty.clone(),
                            });
                            std::mem::swap(r, &mut new_r);
                            instructions.push((
                                i + 1,
                                Instruction::Copy {
                                    ty: ty.clone(),
                                    place: Place::Register(new_r),
                                    operand: Operand::Place(Place::Register(*r)),
                                },
                            ));
                        }
                        Instruction::Binary { .. } | Instruction::Copy { .. } => (),
                    }
                }

                instructions
                    .into_iter()
                    .enumerate()
                    .for_each(|(i, (at, instruction))| {
                        block.instructions.insert(at + i, instruction)
                    });
            }
        }
    }

    fn precolor(allocator: &mut Allocator, function: &Function) {
        for block in &function.blocks {
            for instruction in &block.instructions {
                match instruction {
                    Instruction::Binary {
                        kind: BinOp::Div,
                        lhs,
                        rhs,
                        place,
                        ty,
                    } => {
                        if let Some(r) = place.register_id() {
                            allocator.precolor(r, Register::Rax.into());
                        }

                        let regs: Vec<_> =
                            vec![lhs.register_id(), rhs.register_id(), place.register_id()]
                                .into_iter()
                                .flatten()
                                .collect();

                        if !regs.is_empty() {
                            let rdx = allocator.create_node(ty.clone());
                            allocator.precolor(rdx, Register::Rdx.into());

                            for r in regs {
                                allocator.add_edge((rdx, r));
                            }
                        }
                    }
                    Instruction::Binary {
                        kind: BinOp::Sub,
                        rhs: Operand::Place(Place::Register(rhs)),
                        place: Place::Register(lhs),
                        ..
                    } => allocator.add_edge((*lhs, *rhs)),
                    Instruction::Binary { .. } | Instruction::Copy { .. } => (),
                }
            }

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
                ty,
            } => {
                let ty_size: OperandSize = ty.size().try_into().unwrap();

                self.mov(
                    &lhs.to_source(self, ty_size),
                    &place.location(self).to_dest(ty_size),
                    false,
                );

                match kind {
                    BinOp::Add => {
                        self.add(
                            &place.location(self).to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::Sub => {
                        self.sub(
                            &place.location(self).to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::Mul => {
                        self.mul(
                            &place.location(self).to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::Div => {
                        assert!(
                            !matches!(rhs, Operand::Const(..)),
                            "rhs of div can't be a const"
                        );

                        self.div(&rhs.to_dest(self, ty_size));
                    }
                };
            }
            Instruction::Copy { ty, place, operand } => {
                let ty_size = ty.size().try_into().unwrap();

                match place {
                    Place::Register(r) => {
                        self.mov(
                            &operand.to_source(self, ty_size),
                            &self.locations[*r].to_dest(ty_size),
                            ty.signed(),
                        );
                    }
                    Place::Global(_) => todo!(),
                };
            }
        }
    }

    fn terminator(&mut self, basic_blocks: &[BasicBlock], terminator: &Terminator) {
        match terminator {
            Terminator::Goto(block_id) => self
                .text
                .push_str(&format!("\tjmp .L{}\n", basic_blocks[*block_id].name)),
            Terminator::Return(_) => self.text.push_str("\tret\n"),
        }
    }

    fn function(&mut self, function: Function) {
        let mut allocator = Allocator::new(
            function
                .registers
                .iter()
                .map(|reg| reg.ty.clone())
                .collect(),
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
        Self::precolor(&mut allocator, &function);

        self.locations = allocator.allocate();
        self.text
            .push_str(&format!(".global {0}\n{0}:\n", &function.name));

        for block in &function.blocks {
            self.text.push_str(&format!(".L{}:\n", &block.name));
            for instruction in &block.instructions {
                self.instruction(instruction);
            }

            self.terminator(&function.blocks, &block.terminator);
        }
    }

    pub fn compile(mut self, mut program: Program) -> Vec<u8> {
        Self::canonicalize(&mut program);

        for function in program.functions {
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
            (dest, src) if dest != src => {
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
            _ => (),
        }
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
        self.text.push_str(&format!("\tcqo\n\tidiv {op}\n"));
    }
}
