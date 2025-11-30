from typing import overload

from dgen.base.instruction import *
from dgen.base.operands import *

from .register_classes import *
from .operands import *

O = TypeVar("O", bound=Operand)


@overload
def get_operand(name: str, type: type[O]) -> O: ...


@overload
def get_operand(variant: str, size: int) -> Operand: ...


def get_operand(name_or_variant: str, size_or_type: int | type[O]) -> O | Operand:
    if isinstance(size_or_type, int):
        return {
            "r": get_operand(f"GPR{size_or_type}", Register),
            "m": get_operand(f"I{size_or_type}MEM", Memory),
            "i": get_operand(f"I{size_or_type}IMM", Immediate),
        }[name_or_variant]
    else:
        operand = globals()[name_or_variant]

        if not isinstance(operand, size_or_type):
            assert False

        return operand


# ==============================================================================
# MOV
# ==============================================================================


def mov_rm_r(size: int) -> list[TargetInstruction]:
    instructions: list[TargetInstruction] = []

    for variant in ["r", "m"]:
        operand = get_operand(variant, size)

        instructions.append(
            TargetInstruction(
                f"Mov{size}{variant}r",
                [("dst", operand)],
                [("src1", operand), ("src2", get_operand("r", size))],
                "mov {dst}, {src2}",
                ("dst", "src1"),
            )
        )

    return instructions


[
    [MOV8RR, MOV8MR],
    [MOV16RR, MOV16MR],
    [MOV32RR, MOV32MR],
    [MOV64RR, MOV64MR],
] = [mov_rm_r(size) for size in [8, 16, 32, 64]]


def mov_r_m(size: int) -> TargetInstruction:
    operand = get_operand("r", size)

    return TargetInstruction(
        f"Mov{size}rm",
        [("dst", operand)],
        [("src1", operand), ("src2", get_operand("r", size))],
        "mov {dst}, {src2}",
        ("dst", "src1"),
    )


[
    MOV8RM,
    MOV16RM,
    MOV32RM,
    MOV64RM,
] = [mov_r_m(size) for size in [8, 16, 32, 64]]


def mov_rm_i(size: int) -> list[TargetInstruction]:
    instructions: list[TargetInstruction] = []

    for variant in ["r", "m"]:
        operand = get_operand(variant, size)

        instructions.append(
            TargetInstruction(
                f"Mov{size}{variant}i",
                [("dst", operand)],
                [("src1", operand), ("src2", get_operand("i", size))],
                "mov {dst}, {src2}",
                ("dst", "src1"),
            )
        )

    return instructions


[
    [MOV8RI, MOV8MI],
    [MOV16RI, MOV16MI],
    [MOV32RI, MOV32MI],
] = [mov_rm_i(size) for size in [8, 16, 32]]
MOV64RI = TargetInstruction(
    f"Mov64ri",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I64IMM)],
    "mov {dst}, {src2}",
    ("dst", "src1"),
)


# ==============================================================================
# ADD
# ==============================================================================


def add_rm_i(size: int) -> list[TargetInstruction]:
    instructions: list[TargetInstruction] = []

    for variant in ["r", "m"]:
        operand = get_operand(variant, size)

        instructions.append(
            TargetInstruction(
                f"Add{size}{variant}i",
                [("dst", operand)],
                [("src1", operand), ("src2", get_operand("i", size))],
                "add {dst}, {src2}",
                ("dst", "src1"),
            )
        )

    return instructions


[
    [ADD8RI, ADD8MI],
    [ADD16RI, ADD16MI],
    [ADD32RI, ADD32MI],
] = [add_rm_i(size) for size in [8, 16, 32]]


def add_rm_r(size: int) -> list[TargetInstruction]:
    instructions: list[TargetInstruction] = []

    for variant in ["r", "m"]:
        operand = get_operand(variant, size)

        instructions.append(
            TargetInstruction(
                f"Add{size}{variant}r",
                [("dst", operand)],
                [("src1", operand), ("src2", get_operand("r", size))],
                "add {dst}, {src2}",
                ("dst", "src1"),
            )
        )

    return instructions


[
    [ADD8RR, ADD8MR],
    [ADD16RR, ADD16MR],
    [ADD32RR, ADD32MR],
    [ADD64RR, ADD64MR],
] = [add_rm_r(size) for size in [8, 16, 32, 64]]
ADD64RI32 = TargetInstruction(
    f"Add64ri32",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I32IMM)],
    "add {dst}, {src2}",
    ("dst", "src1"),
)


# ==============================================================================
# SUB
# ==============================================================================


SUB64RI32 = TargetInstruction(
    f"Sub64ri32",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I32IMM)],
    "sub {dst}, {src2}",
    ("dst", "src1"),
)


# ==============================================================================
# LEA
# ==============================================================================


LEA64 = TargetInstruction(
    "Lea64",
    [("dst", GPR64)],
    [("src", ADDR)],
    "lea {dst}, {src}",
)


# ==============================================================================
# SHL
# ==============================================================================


SHL64R8I = TargetInstruction(
    "Shl64r8i",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I8IMM)],
    "shl {dst}, {src2}",
    ("dst", "src1"),
)


# ==============================================================================
# SHR
# ==============================================================================


SHR64R8I = TargetInstruction(
    "Shr64r8i",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I8IMM)],
    "shr {dst}, {src2}",
    ("dst", "src1"),
)


# ==============================================================================
# PUSH
# ==============================================================================


PUSH64R = TargetInstruction("Push64r", [], [("src", GPR64)], "push {src}")


# ==============================================================================
# POP
# ==============================================================================


POP64R = TargetInstruction("Pop64r", [("dst", GPR64)], [], "pop {dst}")


# ==============================================================================
# CALL
# ==============================================================================


CALL64R = TargetInstruction("Call64r", [], [("src", GPR64)], "call {src}")


# ==============================================================================
# JMP
# ==============================================================================


JMP = TargetInstruction("Jmp", [], [("dst", BLOCK)], "jmp {dst}")


# ==============================================================================
# JCC
# ==============================================================================


JCC = TargetInstruction("Jcc", [], [("dst", BLOCK), ("cc", CCODE)], "j{cc} {dst}")


# ==============================================================================
# LEAVE
# ==============================================================================


LEAVE = TargetInstruction("Leave", [], [], "leave")


# ==============================================================================
# RET
# ==============================================================================


RET = TargetInstruction("Ret", [], [], "ret")
