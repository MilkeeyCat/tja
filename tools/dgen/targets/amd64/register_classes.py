from dgen.base.instruction import RegisterClass
from dgen.base.constraints import *

from .registers import *

GPR8 = RegisterClass(
    "Gpr8",
    i8,
    [
        R15B,
        R14B,
        R13B,
        R12B,
        R11B,
        R10B,
        R9B,
        R8B,
        BH,
        BL,
        CL,
        DL,
        SIL,
        DIL,
        AH,
        AL,
    ],
)

GPR16 = RegisterClass(
    "Gpr16",
    i16,
    [
        R15W,
        R14W,
        R13W,
        R12W,
        R11W,
        R10W,
        R9W,
        R8W,
        BX,
        CX,
        DX,
        SI,
        DI,
        AX,
    ],
)
GPR32 = RegisterClass(
    "Gpr32",
    i32,
    [
        R15D,
        R14D,
        R13D,
        R12D,
        R11D,
        R10D,
        R9D,
        R8D,
        EBX,
        ECX,
        EDX,
        ESI,
        EDI,
        EAX,
    ],
)

GPR64 = RegisterClass(
    "Gpr64",
    i64,
    [
        R15,
        R14,
        R13,
        R12,
        R11,
        R10,
        R9,
        R8,
        RBX,
        RCX,
        RDX,
        RSI,
        RDI,
        RAX,
    ],
)
