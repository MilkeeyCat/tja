use super::{AsmPrinter, Condition};
use crate::{
    FunctionIdx,
    mir::{self, GenericOpcode, Module, Operand, Register},
    targets::{RegisterInfo, Target, amd64::OperandKind},
};
use std::fmt::Write;

fn register<T: Target, W: Write>(
    printer: &AsmPrinter<T, W>,
    operands: &[Operand],
) -> Result<String, std::fmt::Error> {
    match operands {
        [Operand::Register(Register::Physical(reg), _)] => {
            Ok(printer.target.register_info().get_name(reg).to_string())
        }
        _ => unreachable!(),
    }
}

fn memory<T: Target, W: Write>(
    printer: &AsmPrinter<T, W>,
    module: &Module,
    operands: &[Operand],
) -> Result<String, std::fmt::Error> {
    match operands {
        [
            base,
            index,
            Operand::Immediate(scale),
            Operand::Immediate(displacement),
        ] => {
            let mut result = String::from("[");

            match base {
                Operand::Register(_, _) => {
                    write!(&mut result, "{}", register(printer, &[base.clone()])?)?
                }
                Operand::Global(idx) => write!(&mut result, "{}", module.globals[*idx].name)?,
                Operand::Function(idx) => write!(&mut result, "{}", module.functions[*idx].name)?,
                _ => unreachable!(),
            };

            match index {
                Operand::Register(_, _) => {
                    write!(&mut result, "+ {}", register(printer, &[index.clone()])?)?
                }
                Operand::Immediate(0) => (),
                _ => unreachable!(),
            };

            if *scale > 1 {
                write!(&mut result, "* {}", *scale)?;
            }

            let displacement = *displacement as i64;
            if displacement != 0 {
                if displacement > 0 {
                    write!(&mut result, "+")?;
                } else {
                    write!(&mut result, "-")?;
                }

                write!(&mut result, "{}", displacement.abs())?;
            }

            write!(&mut result, "]")?;

            Ok(result)
        }
        _ => unreachable!(),
    }
}

#[allow(non_camel_case_types)]
struct r;

impl r {
    const MIR_LENGTH: usize = 1;

    fn from_operands<T: Target, W: Write>(
        printer: &AsmPrinter<T, W>,
        _module: &Module,
        _fn_idx: FunctionIdx,
        operands: &[Operand],
    ) -> Result<String, std::fmt::Error> {
        register(printer, operands)
    }
}

#[allow(non_camel_case_types)]
type r8 = r;

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

            fn from_operands<T: Target, W: Write>(
                printer: &AsmPrinter<T, W>,
                module: &Module,
                _fn_idx: FunctionIdx,
                operands: &[Operand],
            ) -> Result<String, std::fmt::Error> {
                Ok(format!(
                    "{} ptr {}",
                    $prefix,
                    memory(printer, module, operands)?
                ))
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

    fn from_operands<T: Target, W: Write>(
        _printer: &AsmPrinter<T, W>,
        _module: &Module,
        _fn_idx: FunctionIdx,
        operands: &[Operand],
    ) -> Result<String, std::fmt::Error> {
        match operands {
            [Operand::Immediate(value)] => Ok(Condition::from(*value as usize).to_string()),
            _ => unreachable!(),
        }
    }
}

macro_rules! imm_operand {
    ($name: ident, $ty: ty) => {
        #[allow(non_camel_case_types)]
        struct $name;

        impl $name {
            const MIR_LENGTH: usize = 1;

            fn from_operands<T: Target, W: Write>(
                _printer: &AsmPrinter<T, W>,
                _module: &Module,
                _fn_idx: FunctionIdx,
                operands: &[Operand],
            ) -> Result<String, std::fmt::Error> {
                match operands {
                    [Operand::Immediate(value)] => {
                        assert!((<$ty>::MIN..<$ty>::MAX).contains(&(*value as $ty)));

                        Ok(value.to_string())
                    }
                    _ => unreachable!(),
                }
            }
        }
    };
}

imm_operand!(imm8, i8);
imm_operand!(imm16, i16);
imm_operand!(imm32, i32);
imm_operand!(imm64, i64);

#[allow(non_camel_case_types)]
struct label;

impl label {
    const MIR_LENGTH: usize = 1;

    fn from_operands<T: Target, W: Write>(
        _printer: &AsmPrinter<T, W>,
        _module: &Module,
        fn_idx: FunctionIdx,
        operands: &[Operand],
    ) -> Result<String, std::fmt::Error> {
        match operands {
            [Operand::Block(idx)] => Ok(format!(".L{}_{}", fn_idx.raw(), idx.raw())),
            _ => unreachable!(),
        }
    }
}

macro_rules! opcodes {
    ($($variant: ident = $asm: literal, ($($var: ident = $class: ty),*);)+) => {
        #[derive(Debug)]
        #[repr(usize)]
        pub enum Opcode {
            _Dummy = GenericOpcode::Num as usize - 1,
            $(
                $variant,
            )+

            Num
        }

        impl Opcode {
            pub fn write_instruction<T: Target, W: Write>(
                &self,
                printer: &mut AsmPrinter<T, W>,
                module: &Module,
                fn_idx: FunctionIdx,
                operands: &[Operand],
            ) -> Result<(), std::fmt::Error> {
                match self {
                    Self::_Dummy => unreachable!(),
                    $(
                        Self::$variant => {
                            #[allow(unused_mut)]
                            #[allow(unused_variables)]
                            let mut start = 0;
                            $(
                                let $var = <$class>::from_operands(
                                    printer,
                                    module,
                                    fn_idx,
                                    &operands[start..start+<$class>::MIR_LENGTH],
                                )?;
                                #[allow(unused_assignments)]
                                {
                                    start += <$class>::MIR_LENGTH;
                                }
                            )*

                            write!(printer.buf, $asm)
                        }
                    )+,

                    Self::Num => unreachable!(),
                }
            }
        }

        impl From<mir::Opcode> for Opcode {
            fn from(value: mir::Opcode) -> Self {
                assert!(*value >= GenericOpcode::Num as usize && *value < Self::Num as usize);

                unsafe { std::mem::transmute::<_, Self>(value) }
            }
        }

        impl Into<mir::Opcode> for Opcode {
            fn into(self) -> mir::Opcode {
                mir::Opcode(self as usize)
            }
        }
    };
}

opcodes! {
    Mov8rr = "mov {dest}, {src}", (dest = r8, src = r8);
    Mov8rm = "mov {dest}, {src}", (dest = r8, src = m8);
    Mov8mr = "mov {dest}, {src}", (dest = m8, src = r8);
    Mov8mi = "mov {dest}, {src}", (dest = m8, src = imm8);
    Mov8ri = "mov {dest}, {src}", (dest = r8, src = imm8);

    Mov16rr = "mov {dest}, {src}", (dest = r16, src = r16);
    Mov16rm = "mov {dest}, {src}", (dest = r16, src = m16);
    Mov16mr = "mov {dest}, {src}", (dest = m16, src = r16);
    Mov16mi = "mov {dest}, {src}", (dest = m16, src = imm16);
    Mov16ri = "mov {dest}, {src}", (dest = r16, src = imm16);

    Mov32rr = "mov {dest}, {src}", (dest = r32, src = r32);
    Mov32rm = "mov {dest}, {src}", (dest = r32, src = m32);
    Mov32mr = "mov {dest}, {src}", (dest = m32, src = r32);
    Mov32mi = "mov {dest}, {src}", (dest = m32, src = imm32);
    Mov32ri = "mov {dest}, {src}", (dest = r32, src = imm32);

    Mov64rr = "mov {dest}, {src}", (dest = r64, src = r64);
    Mov64rm = "mov {dest}, {src}", (dest = r64, src = m64);
    Mov64mr = "mov {dest}, {src}", (dest = m64, src = r64);
    Mov64ri = "mov {dest}, {src}", (dest = r64, src = imm64);

    Add8rr = "add {dest}, {src}", (dest = r8, src = r8);
    Add8rm = "add {dest}, {src}", (dest = r8, src = m8);
    Add8mr = "add {dest}, {src}", (dest = m8, src = r8);
    Add8mi = "add {dest}, {src}", (dest = m8, src = imm8);
    Add8ri = "add {dest}, {src}", (dest = r8, src = imm8);

    Add16rr = "add {dest}, {src}", (dest = r16, src = r16);
    Add16rm = "add {dest}, {src}", (dest = r16, src = m16);
    Add16mr = "add {dest}, {src}", (dest = m16, src = r16);
    Add16mi = "add {dest}, {src}", (dest = m16, src = imm16);
    Add16ri = "add {dest}, {src}", (dest = r16, src = imm16);

    Add32rr = "add {dest}, {src}", (dest = r32, src = r32);
    Add32rm = "add {dest}, {src}", (dest = r32, src = m32);
    Add32mr = "add {dest}, {src}", (dest = m32, src = r32);
    Add32mi = "add {dest}, {src}", (dest = m32, src = imm32);
    Add32ri = "add {dest}, {src}", (dest = r32, src = imm32);

    Add64rr = "add {dest}, {src}", (dest = r64, src = r64);
    Add64rm = "add {dest}, {src}", (dest = r64, src = m64);
    Add64mr = "add {dest}, {src}", (dest = m64, src = r64);
    Add64mi = "add {dest}, {src}", (dest = m64, src = imm64);
    Add64ri = "add {dest}, {src}", (dest = r64, src = imm64);

    Sub8rr = "sub {dest}, {src}", (dest = r8, src = r8);
    Sub8rm = "sub {dest}, {src}", (dest = r8, src = m8);
    Sub8mr = "sub {dest}, {src}", (dest = m8, src = r8);
    Sub8mi = "sub {dest}, {src}", (dest = m8, src = imm8);
    Sub8ri = "sub {dest}, {src}", (dest = r8, src = imm8);

    Sub16rr = "sub {dest}, {src}", (dest = r16, src = r16);
    Sub16rm = "sub {dest}, {src}", (dest = r16, src = m16);
    Sub16mr = "sub {dest}, {src}", (dest = m16, src = r16);
    Sub16mi = "sub {dest}, {src}", (dest = m16, src = imm16);
    Sub16ri = "sub {dest}, {src}", (dest = r16, src = imm16);

    Sub32rr = "sub {dest}, {src}", (dest = r32, src = r32);
    Sub32rm = "sub {dest}, {src}", (dest = r32, src = m32);
    Sub32mr = "sub {dest}, {src}", (dest = m32, src = r32);
    Sub32mi = "sub {dest}, {src}", (dest = m32, src = imm32);
    Sub32ri = "sub {dest}, {src}", (dest = r32, src = imm32);

    Sub64rr = "sub {dest}, {src}", (dest = r64, src = r64);
    Sub64rm = "sub {dest}, {src}", (dest = r64, src = m64);
    Sub64mr = "sub {dest}, {src}", (dest = m64, src = r64);
    Sub64mi = "sub {dest}, {src}", (dest = m64, src = imm64);
    Sub64ri = "sub {dest}, {src}", (dest = r64, src = imm64);

    Test8ri = "test {lhs}, {rhs}", (lhs = r8, rhs = imm8);
    Test8mi = "test {lhs}, {rhs}", (lhs = m8, rhs = imm8);
    Test8rr = "test {lhs}, {rhs}", (lhs = r8, rhs = r8);
    Test8mr = "test {lhs}, {rhs}", (lhs = m8, rhs = r8);

    Test16ri = "test {lhs}, {rhs}", (lhs = r16, rhs = imm16);
    Test16mi = "test {lhs}, {rhs}", (lhs = m16, rhs = imm16);
    Test16rr = "test {lhs}, {rhs}", (lhs = r16, rhs = r16);
    Test16mr = "test {lhs}, {rhs}", (lhs = m16, rhs = r16);

    Test32ri = "test {lhs}, {rhs}", (lhs = r32, rhs = imm32);
    Test32mi = "test {lhs}, {rhs}", (lhs = m32, rhs = imm32);
    Test32rr = "test {lhs}, {rhs}", (lhs = r32, rhs = r32);
    Test32mr = "test {lhs}, {rhs}", (lhs = m32, rhs = r32);

    Test64rr = "test {lhs}, {rhs}", (lhs = r64, rhs = r64);
    Test64mr = "test {lhs}, {rhs}", (lhs = m64, rhs = r64);

    Lea64 = "lea {lhs}, {rhs}", (lhs = r64, rhs = m64);

    Shl64r8i = "shl {lhs}, {rhs}", (lhs = r64, rhs = imm8);
    Shr64r8i = "shr {lhs}, {rhs}", (lhs = r64, rhs = imm8);

    Push64r = "push {src}", (src = r64);

    Pop64r = "pop {dest}", (dest = r64);

    Call64r = "call {src}", (src = r64);

    Jmp = "jmp {dest}", (dest = label);

    Jcc = "j{cc} {dest}", (dest = label, cc = ccode);

    Leave = "leave", ();
    Ret = "ret", ();
}

pub fn get_load_op(size: usize) -> Opcode {
    match size {
        1 => super::Opcode::Mov8rm,
        2 => super::Opcode::Mov16rm,
        4 => super::Opcode::Mov32rm,
        8 => super::Opcode::Mov64rm,
        _ => unreachable!(),
    }
}

pub fn get_store_op(size: usize) -> Opcode {
    match size {
        1 => super::Opcode::Mov8mr,
        2 => super::Opcode::Mov16mr,
        4 => super::Opcode::Mov32mr,
        8 => super::Opcode::Mov64mr,
        _ => unreachable!(),
    }
}

pub fn get_add_op(dest: OperandKind, src: OperandKind, size: usize) -> mir::Opcode {
    (match (dest, src, size) {
        (OperandKind::Register, OperandKind::Register, 1) => super::Opcode::Add8rr,
        (OperandKind::Register, OperandKind::Memory, 1) => super::Opcode::Add8rm,
        (OperandKind::Memory, OperandKind::Register, 1) => super::Opcode::Add8mr,
        (OperandKind::Memory, OperandKind::Immediate, 1) => super::Opcode::Add8mi,
        (OperandKind::Register, OperandKind::Immediate, 1) => super::Opcode::Add8ri,

        (OperandKind::Register, OperandKind::Register, 2) => super::Opcode::Add16rr,
        (OperandKind::Register, OperandKind::Memory, 2) => super::Opcode::Add16rm,
        (OperandKind::Memory, OperandKind::Register, 2) => super::Opcode::Add16mr,
        (OperandKind::Memory, OperandKind::Immediate, 2) => super::Opcode::Add16mi,
        (OperandKind::Register, OperandKind::Immediate, 2) => super::Opcode::Add16ri,

        (OperandKind::Register, OperandKind::Register, 4) => super::Opcode::Add32rr,
        (OperandKind::Register, OperandKind::Memory, 4) => super::Opcode::Add32rm,
        (OperandKind::Memory, OperandKind::Register, 4) => super::Opcode::Add32mr,
        (OperandKind::Memory, OperandKind::Immediate, 4) => super::Opcode::Add32mi,
        (OperandKind::Register, OperandKind::Immediate, 4) => super::Opcode::Add32ri,

        (OperandKind::Register, OperandKind::Register, 8) => super::Opcode::Add64rr,
        (OperandKind::Register, OperandKind::Memory, 8) => super::Opcode::Add64rm,
        (OperandKind::Memory, OperandKind::Register, 8) => super::Opcode::Add64mr,
        (OperandKind::Memory, OperandKind::Immediate, 8) => super::Opcode::Add64mi,
        (OperandKind::Register, OperandKind::Immediate, 8) => super::Opcode::Add64ri,

        _ => unreachable!(),
    })
    .into()
}
