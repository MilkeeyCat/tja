use crate::{codegen::FunctionCodeGen, mir::Operand};
use std::fmt::{Result, Write};

macro_rules! define_operand_class {
    ($name: ident, $mir_length: literal) => {
        define_operand_class! {$name, $mir_length, ""}
    };

    ($name: ident, $mir_length: literal, $prefix: literal) => {
        #[allow(non_camel_case_types)]
        struct $name;

        impl $name {
            const MIR_LENGTH: usize = $mir_length;

            fn prefix() -> &'static str {
                $prefix
            }
        }
    };
}

define_operand_class! {r8, 1}
define_operand_class! {r16, 1}
define_operand_class! {r32, 1}
define_operand_class! {r64, 1}

define_operand_class! {imm, 1}

define_operand_class! {m8, 4, "byte ptr "}
define_operand_class! {m16, 4, "word ptr "}
define_operand_class! {m32, 4, "dword ptr "}
define_operand_class! {m64, 4, "qword ptr "}

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
            pub fn write_instruction(&self, codegen: &mut FunctionCodeGen, operands: &[Operand]) -> Result {
                match self {
                    $(
                        Self::$variant => {
                            let mut start = 0;
                            $(
                                let $var = format!("{}{}", <$class>::prefix(), codegen.stringify_operand(&operands[start..start+<$class>::MIR_LENGTH])?);
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
}
