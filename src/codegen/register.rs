macro_rules! define_registers {
    ($(($qword: ident, $dword: ident, $word: ident, $byte: ident)),+) => {
        use super::operands::OperandSize;

        #[derive(Debug, Copy, Clone, PartialEq)]
        pub enum Register {
            $($qword, $dword, $word, $byte,)+
        }

        impl Register {
            pub fn resize(self, size: OperandSize) -> Self {
                match (self, size) {
                    $(
                        (Self::$qword | Self::$dword | Self::$word | Self::$byte, OperandSize::Qword) => Self::$qword,
                        (Self::$qword | Self::$dword | Self::$word | Self::$byte, OperandSize::Dword) => Self::$dword,
                        (Self::$qword | Self::$dword | Self::$word | Self::$byte, OperandSize::Word) => Self::$word,
                        (Self::$qword | Self::$dword | Self::$word | Self::$byte, OperandSize::Byte) => Self::$byte,
                    )+
                }
            }

            pub fn size(&self) -> OperandSize {
                match self {
                    $(
                        Self::$qword => OperandSize::Qword,
                        Self::$dword => OperandSize::Dword,
                        Self::$word => OperandSize::Word,
                        Self::$byte => OperandSize::Byte,
                    )+
                }
            }
        }
    };
}

define_registers! {
    (Rax, Eax, Ax, Al),
    (Rbp, Ebp, Bpl, Bp),
    (Rcx, Ecx, Cx, Cl),
    (Rdx, Edx, Dx, Dl),
    (Rsi, Esi, Si, Sil),
    (Rdi, Edi, Di, Dil),
    (Rsp, Esp, Sp, Spl),
    (R15, R15d, R15w, R15b),
    (R14, R14d, R14w, R14b),
    (R13, R13d, R13w, R13b),
    (R12, R12d, R12w, R12b),
    (R11, R11d, R11w, R11b),
    (R10, R10d, R10w, R10b),
    (R9, R9d, R9w, R9b),
    (R8, R8d, R8w, R8b)
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Al => "al",
                Self::Ax => "ax",
                Self::Eax => "eax",
                Self::Rax => "rax",

                Self::Bpl => "bpl",
                Self::Bp => "bp",
                Self::Ebp => "ebp",
                Self::Rbp => "rbp",

                Self::Cl => "cl",
                Self::Cx => "cx",
                Self::Ecx => "ecx",
                Self::Rcx => "rcx",

                Self::Dl => "dl",
                Self::Dx => "dx",
                Self::Edx => "edx",
                Self::Rdx => "rdx",

                Self::Sil => "sil",
                Self::Si => "si",
                Self::Esi => "esi",
                Self::Rsi => "rsi",

                Self::Dil => "dil",
                Self::Di => "di",
                Self::Edi => "edi",
                Self::Rdi => "rdi",

                Self::Rsp => "rsp",
                Self::Esp => "esp",
                Self::Sp => "sp",
                Self::Spl => "spl",

                Self::R15b => "r15b",
                Self::R15w => "r15w",
                Self::R15d => "r15d",
                Self::R15 => "r15",

                Self::R14b => "r14b",
                Self::R14w => "r14w",
                Self::R14d => "r14d",
                Self::R14 => "r14",

                Self::R13b => "r13b",
                Self::R13w => "r13w",
                Self::R13d => "r13d",
                Self::R13 => "r13",

                Self::R12b => "r12b",
                Self::R12w => "r12w",
                Self::R12d => "r12d",
                Self::R12 => "r12",

                Self::R11b => "r11b",
                Self::R11w => "r11w",
                Self::R11d => "r11d",
                Self::R11 => "r11",

                Self::R10b => "r10b",
                Self::R10w => "r10w",
                Self::R10d => "r10d",
                Self::R10 => "r10",

                Self::R9b => "r9b",
                Self::R9w => "r9w",
                Self::R9d => "r9d",
                Self::R9 => "r9",

                Self::R8b => "r8b",
                Self::R8w => "r8w",
                Self::R8d => "r8d",
                Self::R8 => "r8",
            }
        )
    }
}
