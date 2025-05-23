#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(usize)]
pub enum Register {
    Rax,
    Eax,
    Ax,
    Ah,
    Al,

    Rbp,
    Ebp,
    Bpl,
    Bp,

    Rcx,
    Ecx,
    Cx,
    Cl,

    Rdx,
    Edx,
    Dx,
    Dl,

    Rsi,
    Esi,
    Si,
    Sil,

    Rdi,
    Edi,
    Di,
    Dil,

    Rsp,
    Esp,
    Sp,
    Spl,

    R15,
    R15d,
    R15w,
    R15b,

    R14,
    R14d,
    R14w,
    R14b,

    R13,
    R13d,
    R13w,
    R13b,

    R12,
    R12d,
    R12w,
    R12b,

    R11,
    R11d,
    R11w,
    R11b,

    R10,
    R10d,
    R10w,
    R10b,

    R9,
    R9d,
    R9w,
    R9b,

    R8,
    R8d,
    R8w,
    R8b,

    Num,
}

impl Register {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Rax => "rax",
            Self::Eax => "eax",
            Self::Ax => "ax",
            Self::Ah => "ah",
            Self::Al => "al",

            Self::Rbp => "rbp",
            Self::Ebp => "ebp",
            Self::Bpl => "bpl",
            Self::Bp => "bp",

            Self::Rcx => "rcx",
            Self::Ecx => "ecx",
            Self::Cx => "cx",
            Self::Cl => "cl",

            Self::Rdx => "rdx",
            Self::Edx => "edx",
            Self::Dx => "dx",
            Self::Dl => "dl",

            Self::Rsi => "rsi",
            Self::Esi => "esi",
            Self::Si => "si",
            Self::Sil => "sil",

            Self::Rdi => "rdi",
            Self::Edi => "edi",
            Self::Di => "di",
            Self::Dil => "dil",

            Self::Rsp => "rsp",
            Self::Esp => "esp",
            Self::Sp => "sp",
            Self::Spl => "spl",

            Self::R15 => "r15",
            Self::R15d => "r15d",
            Self::R15w => "r15w",
            Self::R15b => "r15b",

            Self::R14 => "r14",
            Self::R14d => "r14d",
            Self::R14w => "r14w",
            Self::R14b => "r14b",

            Self::R13 => "r13",
            Self::R13d => "r13d",
            Self::R13w => "r13w",
            Self::R13b => "r13b",

            Self::R12 => "r12",
            Self::R12d => "r12d",
            Self::R12w => "r12w",
            Self::R12b => "r12b",

            Self::R11 => "r11",
            Self::R11d => "r11d",
            Self::R11w => "r11w",
            Self::R11b => "r11b",

            Self::R10 => "r10",
            Self::R10d => "r10d",
            Self::R10w => "r10w",
            Self::R10b => "r10b",

            Self::R9 => "r9",
            Self::R9d => "r9d",
            Self::R9w => "r9w",
            Self::R9b => "r9b",

            Self::R8 => "r8",
            Self::R8d => "r8d",
            Self::R8w => "r8w",
            Self::R8b => "r8b",

            Self::Num => unreachable!(),
        }
    }
}

impl Register {
    fn subregs(&self) -> &[Self] {
        match self {
            Self::Rax => &[Self::Eax],
            Self::Eax => &[Self::Ax],
            Self::Ax => &[Self::Ah, Self::Al],
            Self::Ah | Self::Al => &[],

            Self::Rbp => &[Self::Ebp],
            Self::Ebp => &[Self::Bpl],
            Self::Bpl => &[Self::Bp],
            Self::Bp => &[],

            Self::Rcx => &[Self::Ecx],
            Self::Ecx => &[Self::Cx],
            Self::Cx => &[Self::Cl],
            Self::Cl => &[],

            Self::Rdx => &[Self::Edx],
            Self::Edx => &[Self::Dx],
            Self::Dx => &[Self::Dl],
            Self::Dl => &[],

            Self::Rsi => &[Self::Esi],
            Self::Esi => &[Self::Si],
            Self::Si => &[Self::Sil],
            Self::Sil => &[],

            Self::Rdi => &[Self::Edi],
            Self::Edi => &[Self::Di],
            Self::Di => &[Self::Dil],
            Self::Dil => &[],

            Self::Rsp => &[Self::Esp],
            Self::Esp => &[Self::Sp],
            Self::Sp => &[Self::Spl],
            Self::Spl => &[],

            Self::R15 => &[Self::R15d],
            Self::R15d => &[Self::R15w],
            Self::R15w => &[Self::R15b],
            Self::R15b => &[],

            Self::R14 => &[Self::R14d],
            Self::R14d => &[Self::R14w],
            Self::R14w => &[Self::R14b],
            Self::R14b => &[],

            Self::R13 => &[Self::R13d],
            Self::R13d => &[Self::R13w],
            Self::R13w => &[Self::R13b],
            Self::R13b => &[],

            Self::R12 => &[Self::R12d],
            Self::R12d => &[Self::R12w],
            Self::R12w => &[Self::R12b],
            Self::R12b => &[],

            Self::R11 => &[Self::R11d],
            Self::R11d => &[Self::R11w],
            Self::R11w => &[Self::R11b],
            Self::R11b => &[],

            Self::R10 => &[Self::R10d],
            Self::R10d => &[Self::R10w],
            Self::R10w => &[Self::R10b],
            Self::R10b => &[],

            Self::R9 => &[Self::R9d],
            Self::R9d => &[Self::R9w],
            Self::R9w => &[Self::R9b],
            Self::R9b => &[],

            Self::R8 => &[Self::R8d],
            Self::R8d => &[Self::R8w],
            Self::R8w => &[Self::R8b],
            Self::R8b => &[],

            Self::Num => unreachable!(),
        }
    }

    pub fn contains(&self, r: Register) -> bool {
        self.subregs()
            .iter()
            .any(|other| other == &r || other.contains(r))
    }
}

impl From<usize> for Register {
    fn from(value: usize) -> Self {
        assert!(value < Self::Num as usize);

        unsafe { std::mem::transmute::<_, Self>(value) }
    }
}
