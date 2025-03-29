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

        impl std::fmt::Display for Register {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$qword => write!(f, paste::paste!(stringify!([< $qword:lower >]))),
                        Self::$dword => write!(f, paste::paste!(stringify!([< $dword:lower >]))),
                        Self::$word => write!(f, paste::paste!(stringify!([< $word:lower >]))),
                        Self::$byte => write!(f, paste::paste!(stringify!([< $byte:lower >]))),
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
