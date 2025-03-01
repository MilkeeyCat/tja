mod allocator;
mod operands;
mod register;

use crate::repr::{
    self, BasicBlock, Const, Function, Instruction, Operand, Place, Program, Terminator, ValueTree,
    op::BinOp, ty::Ty,
};
use allocator::{Allocator, Location};
use operands::{Base, Destination, EffectiveAddress, Offset, OperandSize, Source};
use register::Register;
use std::collections::HashMap;

impl Operand {
    fn to_source(&self, codegen: &CodeGen, size: OperandSize) -> Source {
        match self {
            Self::Place(place) => codegen.variables[place].location.to_source(size),
            Self::Const(val) => match val {
                ValueTree::Leaf(leaf) => leaf.clone().into(),
                ValueTree::Branch(_) => unimplemented!(),
            },
        }
    }

    fn to_dest(&self, codegen: &CodeGen, size: OperandSize) -> Destination {
        match self {
            Self::Place(place) => codegen.variables[place].location.to_dest(size),
            Self::Const(_) => unreachable!(),
        }
    }
}

struct Variable {
    ty: Ty,
    location: Location,
}

pub struct CodeGen {
    variables: HashMap<Place, Variable>,
    bss: String,
    data: String,
    text: String,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
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
                            out,
                            ..
                        } => {
                            if matches!(rhs, Operand::Const(..)) {
                                let r = function.registers.len();
                                function.registers.push(repr::Register {
                                    name: r.to_string(),
                                    ty: function.registers[*out].ty.clone(),
                                });
                                let mut operand = Operand::Place(Place::Register(r));

                                std::mem::swap(rhs, &mut operand);
                                instructions.push((i, Instruction::Copy { out: r, operand }));
                            }

                            let mut r = function.registers.len();
                            function.registers.push(repr::Register {
                                name: r.to_string(),
                                ty: function.registers[*out].ty.clone(),
                            });
                            std::mem::swap(out, &mut r);
                            instructions.push((
                                i + 1,
                                Instruction::Copy {
                                    out: r,
                                    operand: Operand::Place(Place::Register(*out)),
                                },
                            ));
                        }
                        Instruction::Binary { .. }
                        | Instruction::Copy { .. }
                        | Instruction::Alloca { .. }
                        | Instruction::Store { .. }
                        | Instruction::Load { .. } => (),
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

    fn precolor(&self, allocator: &mut Allocator, function: &Function) {
        for block in &function.blocks {
            for instruction in &block.instructions {
                match instruction {
                    Instruction::Binary {
                        kind: BinOp::Div,
                        lhs,
                        rhs,
                        out,
                    } => {
                        allocator.precolor(*out, Register::Rax.into());

                        let regs: Vec<_> = vec![lhs.register_id(), rhs.register_id(), Some(*out)]
                            .into_iter()
                            .flatten()
                            .collect();

                        if !regs.is_empty() {
                            let ty = &function.registers[*out].ty;
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
                        out,
                        ..
                    } => allocator.add_edge((*out, *rhs)),
                    Instruction::Alloca { ty, out } => {
                        allocator.stack_frame_size += ty.size();
                        allocator.precolor(
                            *out,
                            Location::Address(EffectiveAddress {
                                base: Base::Register(Register::Rbp),
                                index: None,
                                scale: None,
                                displacement: Some(Offset(-(allocator.stack_frame_size as isize))),
                            }),
                        );
                    }
                    Instruction::Binary { .. }
                    | Instruction::Copy { .. }
                    | Instruction::Store { .. }
                    | Instruction::Load { .. } => (),
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
                out,
            } => {
                let place = Place::Register(*out);
                let ty = &self.variables[&place].ty;
                let ty_size: OperandSize = ty.size().try_into().unwrap();
                let dest_size = if kind == &BinOp::Div {
                    OperandSize::Qword
                } else {
                    ty_size
                };

                self.mov(
                    &lhs.to_source(self, ty_size),
                    &self.variables[&place].location.to_dest(dest_size),
                    ty.signed(),
                );

                match kind {
                    BinOp::Add => {
                        self.add(
                            &self.variables[&place].location.to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::Sub => {
                        self.sub(
                            &self.variables[&place].location.to_dest(ty_size),
                            &rhs.to_source(self, ty_size),
                        );
                    }
                    BinOp::Mul => {
                        self.mul(
                            &self.variables[&place].location.to_dest(ty_size),
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
            Instruction::Copy { out, operand } => {
                let place = Place::Register(*out);
                let ty = &self.variables[&place].ty;
                let ty_size = ty.size().try_into().unwrap();

                if ty == &Ty::Ptr {
                    match operand {
                        Operand::Const(_) => unreachable!(),
                        Operand::Place(place) => match &self.variables[&place].location {
                            Location::Register(r) => {
                                self.mov(
                                    &(*r).into(),
                                    &self.variables[&place].location.to_dest(ty_size),
                                    false,
                                );
                            }
                            Location::Address(addr) => {
                                self.lea(
                                    &self.variables[&place].location.to_dest(ty_size),
                                    &addr.clone(),
                                );
                            }
                        },
                    };
                } else {
                    self.mov(
                        &operand.to_source(self, ty_size),
                        &self.variables[&place].location.to_dest(ty_size),
                        ty.signed(),
                    );
                }
            }
            Instruction::Alloca { .. } => (),
            Instruction::Store { place, value } => {
                let ty = match value {
                    Operand::Place(place) => &self.variables[place].ty,
                    Operand::Const(ValueTree::Leaf(c)) => &c.ty(),
                    Operand::Const(ValueTree::Branch(_)) => {
                        unimplemented!("Can't do anything about `ValueTree::Branch`")
                    }
                };
                let ty_size = ty.size().try_into().unwrap();

                self.mov(
                    &value.to_source(self, ty_size),
                    &self.variables[place].location.to_dest(ty_size),
                    ty.signed(),
                )
            }
            Instruction::Load { place, out } => {
                let out = Place::Register(*out);
                let ty = &self.variables[&out].ty;
                let ty_size = ty.size().try_into().unwrap();

                self.mov(
                    &self.variables[place].location.to_source(ty_size),
                    &self.variables[&out].location.to_dest(ty_size),
                    ty.signed(),
                )
            }
        }
    }

    fn terminator(
        &mut self,
        basic_blocks: &[BasicBlock],
        terminator: &Terminator,
        ret_label: &str,
    ) {
        match terminator {
            Terminator::Goto(block_id) => self
                .text
                .push_str(&format!("\tjmp .L{}\n", basic_blocks[*block_id].name)),
            Terminator::Return(_) => self.text.push_str(&format!("\tjmp {ret_label}\n")),
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
                Register::Rax,
            ],
            false,
        );
        self.precolor(&mut allocator, &function);

        let ret_label = format!(".L{}_ret", &function.name);
        let (locations, stack_frame_size) = allocator.allocate();
        self.variables.extend(
            locations
                .into_iter()
                .zip(function.registers)
                .enumerate()
                .map(|(i, (location, vreg))| {
                    (
                        Place::Register(i),
                        Variable {
                            ty: vreg.ty,
                            location,
                        },
                    )
                }),
        );
        self.text
            .push_str(&format!(".global {0}\n{0}:\n", &function.name));

        if stack_frame_size > 0 {
            self.push(&Register::Rbp.into());
            self.mov(&Register::Rsp.into(), &Register::Rbp.into(), false);
            self.sub(
                &Register::Rsp.into(),
                //TODO: refactor dis
                &Source::Immediate(Const::U8(stack_frame_size as u8)),
            );
        }

        for block in &function.blocks {
            self.text.push_str(&format!(".L{}:\n", &block.name));
            for instruction in &block.instructions {
                self.instruction(instruction);
            }

            self.terminator(&function.blocks, &block.terminator, &ret_label);
        }

        self.text.push_str(&format!("{ret_label}:\n"));
        if stack_frame_size > 0 {
            self.text.push_str("\tleave\n");
        }
        self.text.push_str("\tret\n");
        self.variables
            .retain(|_, variable| matches!(variable.location, Location::Address(..)));
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

    fn push(&mut self, dest: &Destination) {
        self.text.push_str(&format!("\tpush {dest}\n"));
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

    fn lea(&mut self, dest: &Destination, address: &EffectiveAddress) {
        self.text.push_str(&format!("\tlea {dest}, {address}\n"));
    }
}
