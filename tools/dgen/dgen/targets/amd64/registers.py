from dgen.base.register import Register

AL = Register("al", 8, [])
AH = Register("ah", 8, [])
AX = Register("ax", 16, [AL, AH])
EAX = Register("eax", 32, [AX])
RAX = Register("rax", 64, [EAX])

BL = Register("bl", 8, [])
BH = Register("bh", 8, [])
BX = Register("bx", 16, [BL, BH])
EBX = Register("ebx", 32, [BX])
RBX = Register("rbx", 64, [EBX])

BPL = Register("bpl", 8, [])
BP = Register("bp", 16, [BPL])
EBP = Register("ebp", 32, [BP])
RBP = Register("rbp", 64, [EBP])

CL = Register("cl", 8, [])
CH = Register("ch", 8, [])
CX = Register("cx", 16, [CL, CH])
ECX = Register("ecx", 32, [CX])
RCX = Register("rcx", 64, [ECX])

DL = Register("dl", 8, [])
DH = Register("dh", 8, [])
DX = Register("dx", 16, [DL, DH])
EDX = Register("edx", 32, [DX])
RDX = Register("rdx", 64, [EDX])

SIL = Register("sil", 8, [])
SI = Register("si", 16, [SIL])
ESI = Register("esi", 32, [SI])
RSI = Register("rsi", 64, [ESI])

DIL = Register("dil", 8, [])
DI = Register("di", 16, [DIL])
EDI = Register("edi", 32, [DI])
RDI = Register("rdi", 64, [EDI])

SPL = Register("spl", 8, [])
SP = Register("sp", 16, [SPL])
ESP = Register("esp", 32, [SP])
RSP = Register("rsp", 64, [ESP])


def define_extended_regs(n: int) -> list[Register]:
    byte = Register(f"r{n}b", 8, [])
    word = Register(f"r{n}w", 16, [byte])
    dword = Register(f"r{n}d", 32, [word])
    qword = Register(f"r{n}", 64, [dword])

    return [byte, word, dword, qword]


[R15B, R15W, R15D, R15] = define_extended_regs(15)

[R14B, R14W, R14D, R14] = define_extended_regs(14)

[R13B, R13W, R13D, R13] = define_extended_regs(13)

[R12B, R12W, R12D, R12] = define_extended_regs(12)

[R11B, R11W, R11D, R11] = define_extended_regs(11)

[R10B, R10W, R10D, R10] = define_extended_regs(10)

[R9B, R9W, R9D, R9] = define_extended_regs(9)

[R8B, R8W, R8D, R8] = define_extended_regs(8)
