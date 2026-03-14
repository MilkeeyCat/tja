from .registers import AH, AL, AX, DX, EAX, EDX, RAX, RBP, RDX
from .instruction import Instruction
from .operand import Value, r, w, rw, implicit
from .values import *


def get_value(variant: str, size: int) -> Value:
    name = {
        "r": f"R{size}",
        "m": f"MEM{size}",
        "i": f"IMM{size}",
    }[variant]

    value = globals()[name]

    if not isinstance(value, Value):
        assert False, "value not found"

    return value


# ==============================================================================
# MOV
# ==============================================================================


# rm, r
for size in [8, 16, 32, 64]:
    for variant in ["r", "m"]:
        value = get_value(variant, size)

        Instruction(f"Mov{size}{variant}r", "mov", [w(value), r(get_value("r", size))])

# r, m
for size in [8, 16, 32, 64]:
    value = get_value("r", size)

    Instruction(f"Mov{size}rm", "mov", [w(value), r(get_value("m", size))])

# rm, i
for size in [8, 16, 32]:
    for variant in ["r", "m"]:
        value = get_value(variant, size)

        Instruction(f"Mov{size}{variant}i", "mov", [w(value), r(get_value("i", size))])

Instruction(f"Mov64ri", "mov", [w(R64), r(IMM64)])


# ==============================================================================
# MOVSX
# ==============================================================================


# r, rm
for from_, to in [(8, 16), (8, 32), (8, 64), (16, 32), (16, 64)]:
    for variant in ["r", "m"]:
        Instruction(
            f"Movsx{to}r{variant}{from_}",
            "movsx",
            [w(get_value("r", to)), r(get_value(variant, from_))],
        )


# ==============================================================================
# ADD
# ==============================================================================


# rm, i
for size in [8, 16, 32]:
    for variant in ["r", "m"]:
        value = get_value(variant, size)

        Instruction(f"Add{size}{variant}i", "add", [rw(value), r(get_value("i", size))])

# rm, r
for size in [8, 16, 32, 64]:
    for variant in ["r", "m"]:
        value = get_value(variant, size)

        Instruction(f"Add{size}{variant}r", "add", [rw(value), r(get_value("r", size))])

Instruction(f"Add64ri32", "add", [rw(R64), r(IMM32)])


# ==============================================================================
# SUB
# ==============================================================================


# rm, i
for size in [8, 16, 32]:
    for variant in ["r", "m"]:
        value = get_value(variant, size)

        Instruction(f"Sub{size}{variant}i", "sub", [rw(value), r(get_value("i", size))])

# rm, r
for size in [8, 16, 32, 64]:
    for variant in ["r", "m"]:
        value = get_value(variant, size)

        Instruction(f"Sub{size}{variant}r", "sub", [rw(value), r(get_value("r", size))])

Instruction(f"Sub64ri32", "sub", [rw(R64), r(IMM32)])


# ==============================================================================
# IMUL
# ==============================================================================


Instruction("IMul8r", "imul", [w(implicit(AX)), r(implicit(AL)), r(R8)])
Instruction("IMul16r", "imul", [rw(implicit(AX)), w(implicit(DX)), r(R16)])
Instruction("IMul32r", "imul", [rw(implicit(EAX)), w(implicit(EDX)), r(R32)])
Instruction("IMul64r", "imul", [rw(implicit(RAX)), w(implicit(RDX)), r(R64)])


# ==============================================================================
# IDIV
# ==============================================================================


Instruction(
    "IDiv8r", "idiv", [w(implicit(AL)), w(implicit(AH)), r(implicit(AX)), r(R8)]
)
Instruction("IDiv16r", "idiv", [rw(implicit(AX)), rw(implicit(DX)), r(R16)])
Instruction("IDiv32r", "idiv", [rw(implicit(EAX)), rw(implicit(EDX)), r(R32)])
Instruction("IDiv64r", "idiv", [rw(implicit(RAX)), rw(implicit(RDX)), r(R64)])


# ==============================================================================
# CMP
# ==============================================================================


for size in [8, 16, 32, 64]:
    for variant in ["r", "m"]:
        value = get_value(variant, size)

        Instruction(f"Cmp{size}{variant}r", "cmp", [r(value), r(get_value("r", size))])

Instruction(f"Cmp8ri", "cmp", [r(R8), r(IMM8)])


# ==============================================================================
# XOR
# ==============================================================================


for size in [8, 16, 32, 64]:
    value = get_value("r", size)

    Instruction(f"Xor{size}rr", "xor", [rw(value), r(value)])


# ==============================================================================
# LEA
# ==============================================================================


Instruction("Lea64", "lea", [w(R64), r(ADDR)])


# ==============================================================================
# SHL
# ==============================================================================


Instruction("Shl64r8i", "shl", [rw(R64), r(IMM8)])


# ==============================================================================
# SHR
# ==============================================================================


Instruction("Shr64r8i", "shr", [rw(R64), r(IMM8)])


# ==============================================================================
# PUSH
# ==============================================================================


Instruction("Push64r", "push", [r(R64)])


# ==============================================================================
# POP
# ==============================================================================


Instruction("Pop64r", "pop", [w(R64)])


# ==============================================================================
# CALL
# ==============================================================================


Instruction("Call64r", "call", [r(R64)])


# ==============================================================================
# JMP
# ==============================================================================


Instruction("Jmp", "jmp", [r(BLOCK_IDX)])


# ==============================================================================
# JCC
# ==============================================================================


# TODO: add more variants
Instruction("Ja", "ja", [r(BLOCK_IDX)])


# ==============================================================================
# SETCC
# ==============================================================================


# TODO: add more variants
Instruction("Setar", "seta", [w(R8)])
Instruction("Setam", "seta", [w(MEM8)])


# ==============================================================================
# LEAVE
# ==============================================================================


Instruction("Leave64", "leave", [rw(implicit(RBP))])


# ==============================================================================
# RET
# ==============================================================================


Instruction("Ret", "ret", [])


# ==============================================================================
# CWD/CDQ/CQO
# ==============================================================================


Instruction("Cwd", "cwd", [w(implicit(DX)), r(implicit(AX))])
Instruction("Cdq", "cdq", [w(implicit(EDX)), r(implicit(EAX))])
Instruction("Cqo", "cqo", [w(implicit(RDX)), r(implicit(RAX))])
