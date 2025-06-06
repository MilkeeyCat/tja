use super::Condition;
use crate::{
    codegen::{FunctionCodeGen, allocator::Location},
    mir::Operand,
};
use std::fmt::Write;

fn register(codegen: &FunctionCodeGen, operands: &[Operand]) -> Result<String, std::fmt::Error> {
    match operands {
        [Operand::Vreg(idx, _)] => match codegen.vregs[idx] {
            Location::Register(r) => Ok(codegen.target.register_info().get_name(&r).to_string()),
            Location::Spill(_) => unreachable!(),
        },
        [Operand::Reg(r)] => Ok(codegen.target.register_info().get_name(r).to_string()),
        _ => unreachable!(),
    }
}

fn memory(codegen: &FunctionCodeGen, operands: &[Operand]) -> Result<String, std::fmt::Error> {
    match operands {
        [Operand::Frame(idx)] => memory(&codegen, &codegen.stack_slots[idx]),
        [
            base,
            index,
            Operand::Immediate(scale),
            Operand::Immediate(displacement),
        ] => {
            let mut result = String::from("[");

            match base {
                Operand::Reg(r) => {
                    result.push_str(codegen.target.register_info().get_name(r));
                }
                Operand::Vreg(idx, _) => match codegen.vregs[idx] {
                    Location::Register(r) => write!(
                        &mut result,
                        "{}",
                        codegen.target.register_info().get_name(&r)
                    )?,
                    Location::Spill(_) => unreachable!(),
                },
                Operand::Global(idx) => write!(&mut result, "{}", codegen.globals[*idx].name)?,
                _ => unreachable!(),
            };

            match index {
                Operand::Reg(r) => {
                    write!(
                        &mut result,
                        " + {}",
                        codegen.target.register_info().get_name(r)
                    )?;
                }
                Operand::Vreg(idx, _) => match codegen.vregs[idx] {
                    Location::Register(r) => write!(
                        &mut result,
                        " + {}",
                        codegen.target.register_info().get_name(&r)
                    )?,
                    Location::Spill(_) => unreachable!(),
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
        _ => unreachable!(),
    }
}

#[allow(non_camel_case_types)]
struct r8;

impl r8 {
    const MIR_LENGTH: usize = 1;

    fn from_operands(
        codegen: &FunctionCodeGen,
        operands: &[Operand],
    ) -> Result<String, std::fmt::Error> {
        register(codegen, operands)
    }
}

#[allow(non_camel_case_types)]
type r16 = r8;

#[allow(non_camel_case_types)]
type r32 = r8;

#[allow(non_camel_case_types)]
type r64 = r8;

macro_rules! mem_operand {
    ($name: ident, $prefix: literal) => {
        #[allow(non_camel_case_types)]
        struct $name;

        impl $name {
            const MIR_LENGTH: usize = 4;

            fn from_operands(
                codegen: &FunctionCodeGen,
                operands: &[Operand],
            ) -> Result<String, std::fmt::Error> {
                Ok(format!("{} ptr {}", $prefix, memory(codegen, operands)?))
            }
        }
    };
}

mem_operand! {m8, "byte"}
mem_operand! {m16, "word"}
mem_operand! {m32, "dword"}
mem_operand! {m64, "qword"}

#[allow(non_camel_case_types)]
struct ccode;

impl ccode {
    const MIR_LENGTH: usize = 1;

    fn from_operands(_: &FunctionCodeGen, operands: &[Operand]) -> Result<String, std::fmt::Error> {
        match operands {
            [Operand::Immediate(value)] => Ok(Condition::from(*value as usize).to_string()),
            _ => unreachable!(),
        }
    }
}

#[allow(non_camel_case_types)]
struct imm;

impl imm {
    const MIR_LENGTH: usize = 1;

    fn from_operands(_: &FunctionCodeGen, operands: &[Operand]) -> Result<String, std::fmt::Error> {
        match operands {
            [Operand::Immediate(value)] => Ok(value.to_string()),
            _ => unreachable!(),
        }
    }
}

#[allow(non_camel_case_types)]
struct label;

impl label {
    const MIR_LENGTH: usize = 1;

    fn from_operands(_: &FunctionCodeGen, operands: &[Operand]) -> Result<String, std::fmt::Error> {
        match operands {
            [Operand::Block(idx)] => Ok(format!(".L{idx}")),
            _ => unreachable!(),
        }
    }
}

macro_rules! opcodes {
    ($($variant: ident = $asm: literal, ($($var: ident = $class: ty),+);)+) => {
        #[repr(usize)]
        pub enum Opcode {
            $(
                $variant,
            )+

            Num
        }

        impl Opcode {
            pub fn write_instruction(&self, codegen: &mut FunctionCodeGen, operands: &[Operand]) -> std::fmt::Result {
                match self {
                    $(
                        Self::$variant => {
                            let mut start = 0;
                            $(
                                let $var = <$class>::from_operands(codegen, &operands[start..start+<$class>::MIR_LENGTH])?;
                                #[allow(unused_assignments)]
                                {
                                    start += <$class>::MIR_LENGTH;
                                }
                            )+

                            write!(codegen.text, $asm)
                        }
                    )+,

                    Self::Num => unreachable!(),
                }
            }
        }

        impl From<usize> for Opcode {
            fn from(value: usize) -> Self {
                assert!(value < Self::Num as usize);

                unsafe { std::mem::transmute::<_, Self>(value) }
            }
        }
    };
}

opcodes! {
    Mov8rr = "mov {dest}, {src}", (dest = r8, src = r8);
    Mov8rm = "mov {dest}, {src}", (dest = r8, src = m8);
    Mov8mr = "mov {dest}, {src}", (dest = m8, src = r8);
    Mov8mi = "mov {dest}, {src}", (dest = m8, src = imm);
    Mov8ri = "mov {dest}, {src}", (dest = r8, src = imm);

    Mov16rr = "mov {dest}, {src}", (dest = r16, src = r16);
    Mov16rm = "mov {dest}, {src}", (dest = r16, src = m16);
    Mov16mr = "mov {dest}, {src}", (dest = m16, src = r16);
    Mov16mi = "mov {dest}, {src}", (dest = m16, src = imm);
    Mov16ri = "mov {dest}, {src}", (dest = r16, src = imm);

    Mov32rr = "mov {dest}, {src}", (dest = r32, src = r32);
    Mov32rm = "mov {dest}, {src}", (dest = r32, src = m32);
    Mov32mr = "mov {dest}, {src}", (dest = m32, src = r32);
    Mov32mi = "mov {dest}, {src}", (dest = m32, src = imm);
    Mov32ri = "mov {dest}, {src}", (dest = r32, src = imm);

    Mov64rr = "mov {dest}, {src}", (dest = r64, src = r64);
    Mov64rm = "mov {dest}, {src}", (dest = r64, src = m64);
    Mov64mr = "mov {dest}, {src}", (dest = m64, src = r64);
    Mov64mi = "mov {dest}, {src}", (dest = m64, src = imm);
    Mov64ri = "mov {dest}, {src}", (dest = r64, src = imm);

    Add8rr = "add {dest}, {src}", (dest = r8, src = r8);
    Add8rm = "add {dest}, {src}", (dest = r8, src = m8);
    Add8mr = "add {dest}, {src}", (dest = m8, src = r8);
    Add8mi = "add {dest}, {src}", (dest = m8, src = imm);
    Add8ri = "add {dest}, {src}", (dest = r8, src = imm);

    Add16rr = "add {dest}, {src}", (dest = r16, src = r16);
    Add16rm = "add {dest}, {src}", (dest = r16, src = m16);
    Add16mr = "add {dest}, {src}", (dest = m16, src = r16);
    Add16mi = "add {dest}, {src}", (dest = m16, src = imm);
    Add16ri = "add {dest}, {src}", (dest = r16, src = imm);

    Add32rr = "add {dest}, {src}", (dest = r32, src = r32);
    Add32rm = "add {dest}, {src}", (dest = r32, src = m32);
    Add32mr = "add {dest}, {src}", (dest = m32, src = r32);
    Add32mi = "add {dest}, {src}", (dest = m32, src = imm);
    Add32ri = "add {dest}, {src}", (dest = r32, src = imm);

    Add64rr = "add {dest}, {src}", (dest = r64, src = r64);
    Add64rm = "add {dest}, {src}", (dest = r64, src = m64);
    Add64mr = "add {dest}, {src}", (dest = m64, src = r64);
    Add64mi = "add {dest}, {src}", (dest = m64, src = imm);
    Add64ri = "add {dest}, {src}", (dest = r64, src = imm);

    Test8ri = "test {lhs}, {rhs}", (lhs = r8, rhs = imm);
    Test8mi = "test {lhs}, {rhs}", (lhs = m8, rhs = imm);
    Test8rr = "test {lhs}, {rhs}", (lhs = r8, rhs = r8);
    Test8mr = "test {lhs}, {rhs}", (lhs = m8, rhs = r8);

    Test16ri = "test {lhs}, {rhs}", (lhs = r16, rhs = imm);
    Test16mi = "test {lhs}, {rhs}", (lhs = m16, rhs = imm);
    Test16rr = "test {lhs}, {rhs}", (lhs = r16, rhs = r16);
    Test16mr = "test {lhs}, {rhs}", (lhs = m16, rhs = r16);

    Test32ri = "test {lhs}, {rhs}", (lhs = r32, rhs = imm);
    Test32mi = "test {lhs}, {rhs}", (lhs = m32, rhs = imm);
    Test32rr = "test {lhs}, {rhs}", (lhs = r32, rhs = r32);
    Test32mr = "test {lhs}, {rhs}", (lhs = m32, rhs = r32);

    Test64ri = "test {lhs}, {rhs}", (lhs = r64, rhs = imm);
    Test64mi = "test {lhs}, {rhs}", (lhs = m64, rhs = imm);
    Test64rr = "test {lhs}, {rhs}", (lhs = r64, rhs = r64);
    Test64mr = "test {lhs}, {rhs}", (lhs = m64, rhs = r64);

    Jmp = "jmp {dest}", (dest = label);

    Jcc = "j{cc} {dest}", (dest = label, cc = ccode);
}
