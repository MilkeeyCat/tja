use super::OperandSize;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register {
    Al,
    Ax,
    Eax,
    Rax,

    Bpl,
    Bp,
    Ebp,
    Rbp,

    Cl,
    Cx,
    Ecx,
    Rcx,

    Dl,
    Dx,
    Edx,
    Rdx,

    Sil,
    Si,
    Esi,
    Rsi,

    Dil,
    Di,
    Edi,
    Rdi,

    Rsp,
    Esp,
    Sp,
    Spl,

    R15b,
    R15w,
    R15d,
    R15,

    R14b,
    R14w,
    R14d,
    R14,

    R13b,
    R13w,
    R13d,
    R13,

    R12b,
    R12w,
    R12d,
    R12,

    R11b,
    R11w,
    R11d,
    R11,

    R10b,
    R10w,
    R10d,
    R10,

    R9b,
    R9w,
    R9d,
    R9,

    R8b,
    R8w,
    R8d,
    R8,
}

impl Register {
    pub fn resize(self, size: OperandSize) -> Self {
        match (self, size) {
            (Self::Al | Self::Ax | Self::Eax | Self::Rax, OperandSize::Byte) => Self::Al,
            (Self::Al | Self::Ax | Self::Eax | Self::Rax, OperandSize::Word) => Self::Ax,
            (Self::Al | Self::Ax | Self::Eax | Self::Rax, OperandSize::Dword) => Self::Eax,
            (Self::Al | Self::Ax | Self::Eax | Self::Rax, OperandSize::Qword) => Self::Rax,

            (Self::Bpl | Self::Bp | Self::Ebp | Self::Rbp, OperandSize::Byte) => Self::Bpl,
            (Self::Bpl | Self::Bp | Self::Ebp | Self::Rbp, OperandSize::Word) => Self::Bp,
            (Self::Bpl | Self::Bp | Self::Ebp | Self::Rbp, OperandSize::Dword) => Self::Ebp,
            (Self::Bpl | Self::Bp | Self::Ebp | Self::Rbp, OperandSize::Qword) => Self::Rbp,

            (Self::Cl | Self::Cx | Self::Ecx | Self::Rcx, OperandSize::Byte) => Self::Cl,
            (Self::Cl | Self::Cx | Self::Ecx | Self::Rcx, OperandSize::Word) => Self::Cx,
            (Self::Cl | Self::Cx | Self::Ecx | Self::Rcx, OperandSize::Dword) => Self::Ecx,
            (Self::Cl | Self::Cx | Self::Ecx | Self::Rcx, OperandSize::Qword) => Self::Rcx,

            (Self::Dl | Self::Dx | Self::Edx | Self::Rdx, OperandSize::Byte) => Self::Dl,
            (Self::Dl | Self::Dx | Self::Edx | Self::Rdx, OperandSize::Word) => Self::Dx,
            (Self::Dl | Self::Dx | Self::Edx | Self::Rdx, OperandSize::Dword) => Self::Edx,
            (Self::Dl | Self::Dx | Self::Edx | Self::Rdx, OperandSize::Qword) => Self::Rdx,

            (Self::Sil | Self::Si | Self::Esi | Self::Rsi, OperandSize::Byte) => Self::Sil,
            (Self::Sil | Self::Si | Self::Esi | Self::Rsi, OperandSize::Word) => Self::Si,
            (Self::Sil | Self::Si | Self::Esi | Self::Rsi, OperandSize::Dword) => Self::Esi,
            (Self::Sil | Self::Si | Self::Esi | Self::Rsi, OperandSize::Qword) => Self::Rsi,

            (Self::Dil | Self::Di | Self::Edi | Self::Rdi, OperandSize::Byte) => Self::Dil,
            (Self::Dil | Self::Di | Self::Edi | Self::Rdi, OperandSize::Word) => Self::Di,
            (Self::Dil | Self::Di | Self::Edi | Self::Rdi, OperandSize::Dword) => Self::Edi,
            (Self::Dil | Self::Di | Self::Edi | Self::Rdi, OperandSize::Qword) => Self::Rdi,

            (Self::Spl | Self::Sp | Self::Esp | Self::Rsp, OperandSize::Byte) => Self::Spl,
            (Self::Spl | Self::Sp | Self::Esp | Self::Rsp, OperandSize::Word) => Self::Sp,
            (Self::Spl | Self::Sp | Self::Esp | Self::Rsp, OperandSize::Dword) => Self::Esp,
            (Self::Spl | Self::Sp | Self::Esp | Self::Rsp, OperandSize::Qword) => Self::Rsp,

            (Self::R15b | Self::R15w | Self::R15d | Self::R15, OperandSize::Byte) => Self::R15b,
            (Self::R15b | Self::R15w | Self::R15d | Self::R15, OperandSize::Word) => Self::R15w,
            (Self::R15b | Self::R15w | Self::R15d | Self::R15, OperandSize::Dword) => Self::R15d,
            (Self::R15b | Self::R15w | Self::R15d | Self::R15, OperandSize::Qword) => Self::R15,

            (Self::R14b | Self::R14w | Self::R14d | Self::R14, OperandSize::Byte) => Self::R14b,
            (Self::R14b | Self::R14w | Self::R14d | Self::R14, OperandSize::Word) => Self::R14w,
            (Self::R14b | Self::R14w | Self::R14d | Self::R14, OperandSize::Dword) => Self::R14d,
            (Self::R14b | Self::R14w | Self::R14d | Self::R14, OperandSize::Qword) => Self::R14,

            (Self::R13b | Self::R13w | Self::R13d | Self::R13, OperandSize::Byte) => Self::R13b,
            (Self::R13b | Self::R13w | Self::R13d | Self::R13, OperandSize::Word) => Self::R13w,
            (Self::R13b | Self::R13w | Self::R13d | Self::R13, OperandSize::Dword) => Self::R13d,
            (Self::R13b | Self::R13w | Self::R13d | Self::R13, OperandSize::Qword) => Self::R13,

            (Self::R12b | Self::R12w | Self::R12d | Self::R12, OperandSize::Byte) => Self::R12b,
            (Self::R12b | Self::R12w | Self::R12d | Self::R12, OperandSize::Word) => Self::R12w,
            (Self::R12b | Self::R12w | Self::R12d | Self::R12, OperandSize::Dword) => Self::R12d,
            (Self::R12b | Self::R12w | Self::R12d | Self::R12, OperandSize::Qword) => Self::R12,

            (Self::R11b | Self::R11w | Self::R11d | Self::R11, OperandSize::Byte) => Self::R11b,
            (Self::R11b | Self::R11w | Self::R11d | Self::R11, OperandSize::Word) => Self::R11w,
            (Self::R11b | Self::R11w | Self::R11d | Self::R11, OperandSize::Dword) => Self::R11d,
            (Self::R11b | Self::R11w | Self::R11d | Self::R11, OperandSize::Qword) => Self::R11,

            (Self::R10b | Self::R10w | Self::R10d | Self::R10, OperandSize::Byte) => Self::R10b,
            (Self::R10b | Self::R10w | Self::R10d | Self::R10, OperandSize::Word) => Self::R10w,
            (Self::R10b | Self::R10w | Self::R10d | Self::R10, OperandSize::Dword) => Self::R10d,
            (Self::R10b | Self::R10w | Self::R10d | Self::R10, OperandSize::Qword) => Self::R10,

            (Self::R9b | Self::R9w | Self::R9d | Self::R9, OperandSize::Byte) => Self::R9b,
            (Self::R9b | Self::R9w | Self::R9d | Self::R9, OperandSize::Word) => Self::R9w,
            (Self::R9b | Self::R9w | Self::R9d | Self::R9, OperandSize::Dword) => Self::R9d,
            (Self::R9b | Self::R9w | Self::R9d | Self::R9, OperandSize::Qword) => Self::R9,

            (Self::R8b | Self::R8w | Self::R8d | Self::R8, OperandSize::Byte) => Self::R8b,
            (Self::R8b | Self::R8w | Self::R8d | Self::R8, OperandSize::Word) => Self::R8w,
            (Self::R8b | Self::R8w | Self::R8d | Self::R8, OperandSize::Dword) => Self::R8d,
            (Self::R8b | Self::R8w | Self::R8d | Self::R8, OperandSize::Qword) => Self::R8,
        }
    }

    pub fn size(&self) -> OperandSize {
        match self {
            Self::Al => OperandSize::Byte,
            Self::Ax => OperandSize::Word,
            Self::Eax => OperandSize::Dword,
            Self::Rax => OperandSize::Qword,

            Self::Bpl => OperandSize::Byte,
            Self::Bp => OperandSize::Word,
            Self::Ebp => OperandSize::Dword,
            Self::Rbp => OperandSize::Qword,

            Self::Cl => OperandSize::Byte,
            Self::Cx => OperandSize::Word,
            Self::Ecx => OperandSize::Dword,
            Self::Rcx => OperandSize::Qword,

            Self::Dl => OperandSize::Byte,
            Self::Dx => OperandSize::Word,
            Self::Edx => OperandSize::Dword,
            Self::Rdx => OperandSize::Qword,

            Self::Sil => OperandSize::Byte,
            Self::Si => OperandSize::Word,
            Self::Esi => OperandSize::Dword,
            Self::Rsi => OperandSize::Qword,

            Self::Dil => OperandSize::Byte,
            Self::Di => OperandSize::Word,
            Self::Edi => OperandSize::Dword,
            Self::Rdi => OperandSize::Qword,

            Self::Spl => OperandSize::Byte,
            Self::Sp => OperandSize::Word,
            Self::Esp => OperandSize::Dword,
            Self::Rsp => OperandSize::Qword,

            Self::R15b => OperandSize::Byte,
            Self::R15w => OperandSize::Word,
            Self::R15d => OperandSize::Dword,
            Self::R15 => OperandSize::Qword,

            Self::R14b => OperandSize::Byte,
            Self::R14w => OperandSize::Word,
            Self::R14d => OperandSize::Dword,
            Self::R14 => OperandSize::Qword,

            Self::R13b => OperandSize::Byte,
            Self::R13w => OperandSize::Word,
            Self::R13d => OperandSize::Dword,
            Self::R13 => OperandSize::Qword,

            Self::R12b => OperandSize::Byte,
            Self::R12w => OperandSize::Word,
            Self::R12d => OperandSize::Dword,
            Self::R12 => OperandSize::Qword,

            Self::R11b => OperandSize::Byte,
            Self::R11w => OperandSize::Word,
            Self::R11d => OperandSize::Dword,
            Self::R11 => OperandSize::Qword,

            Self::R10b => OperandSize::Byte,
            Self::R10w => OperandSize::Word,
            Self::R10d => OperandSize::Dword,
            Self::R10 => OperandSize::Qword,

            Self::R9b => OperandSize::Byte,
            Self::R9w => OperandSize::Word,
            Self::R9d => OperandSize::Dword,
            Self::R9 => OperandSize::Qword,

            Self::R8b => OperandSize::Byte,
            Self::R8w => OperandSize::Word,
            Self::R8d => OperandSize::Dword,
            Self::R8 => OperandSize::Qword,
        }
    }
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
