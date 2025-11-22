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


def mov() -> list[list[TargetInstruction]]:
    instructions: list[list[TargetInstruction]] = []
    sizes = [8, 16, 32, 64]
    variants = ["rr", "rm", "mr", "mi", "ri"]

    for size in sizes:
        variant_instructions: list[TargetInstruction] = []

        for variant in variants:
            if size == 64 and variant == "mi":
                continue

            lhs = get_operand(variant[0], size)
            rhs = get_operand(variant[1], size)

            variant_instructions.append(
                TargetInstruction(
                    f"Mov{size}{variant}",
                    [("dst", lhs)],
                    [("src1", lhs), ("src2", rhs)],
                    "mov {dst}, {src2}",
                    ("dst", "src1"),
                )
            )

        instructions.append(variant_instructions)

    return instructions


[
    [MOV8RR, MOV8RM, MOV8MR, MOV8MI, MOV8RI],
    [MOV16RR, MOV16RM, MOV16MR, MOV16MI, MOV16RI],
    [MOV32RR, MOV32RM, MOV32MR, MOV32MI, MOV32RI],
    [MOV64RR, MOV64RM, MOV64MR, MOV64RI],
] = mov()


def add() -> list[list[TargetInstruction]]:
    instructions: list[list[TargetInstruction]] = []
    sizes = [8, 16, 32, 64]
    variants = ["rr", "rm", "mr", "mi", "ri"]

    for size in sizes:
        variant_instructions: list[TargetInstruction] = []

        for variant in variants:
            lhs = get_operand(variant[0], size)
            rhs = get_operand(variant[1], size)

            variant_instructions.append(
                TargetInstruction(
                    f"Add{size}{variant}",
                    [("dst", lhs)],
                    [("src1", lhs), ("src2", rhs)],
                    "add {dst}, {src2}",
                    ("dst", "src1"),
                )
            )

        instructions.append(variant_instructions)

    return instructions


[
    [ADD8RR, ADD8RM, ADD8MR, ADD8MI, ADD8RI],
    [ADD16RR, ADD16RM, ADD16MR, ADD16MI, ADD16RI],
    [ADD32RR, ADD32RM, ADD32MR, ADD32MI, ADD32RI],
    [ADD64RR, ADD64RM, ADD64MR, ADD64MI, ADD64RI],
] = add()


def sub() -> list[list[TargetInstruction]]:
    instructions: list[list[TargetInstruction]] = []
    sizes = [8, 16, 32, 64]
    variants = ["rr", "rm", "mr", "mi", "ri"]

    for size in sizes:
        variant_instructions: list[TargetInstruction] = []

        for variant in variants:
            lhs = get_operand(variant[0], size)
            rhs = get_operand(variant[1], size)

            variant_instructions.append(
                TargetInstruction(
                    f"Sub{size}{variant}",
                    [("dst", lhs)],
                    [("src1", lhs), ("src2", rhs)],
                    "sub {dst}, {src2}",
                    ("dst", "src1"),
                )
            )

        instructions.append(variant_instructions)

    return instructions


[
    [SUB8RR, SUB8RM, SUB8MR, SUB8MI, SUB8RI],
    [SUB16RR, SUB16RM, SUB16MR, SUB16MI, SUB16RI],
    [SUB32RR, SUB32RM, SUB32MR, SUB32MI, SUB32RI],
    [SUB64RR, SUB64RM, SUB64MR, SUB64MI, SUB64RI],
] = sub()


def test() -> list[list[TargetInstruction]]:
    instructions: list[list[TargetInstruction]] = []
    sizes = [8, 16, 32, 64]
    variants = ["rr", "mr", "mi", "ri"]

    for size in sizes:
        variant_instructions: list[TargetInstruction] = []

        for variant in variants:
            if variant in ["mi", "ri"] and size == 64:
                continue

            lhs = get_operand(variant[0], size)
            rhs = get_operand(variant[1], size)

            variant_instructions.append(
                TargetInstruction(
                    f"Test{size}{variant}",
                    [("dst", lhs)],
                    [("src1", lhs), ("src2", rhs)],
                    "sub {dst}, {src2}",
                    ("dst", "src1"),
                )
            )

        instructions.append(variant_instructions)

    return instructions


[
    [TEST8RR, TEST8MR, TEST8MI, TEST8RI],
    [TEST16RR, TEST16MR, TEST16MI, TEST16RI],
    [TEST32RR, TEST32MR, TEST32MI, TEST32RI],
    [TEST64RR, TEST64MR],
] = test()

LEA64 = TargetInstruction(
    "Lea64",
    [("dst", GPR64)],
    [("src", ADDR)],
    "lea {dst}, {src}",
)

SHL64R8I = TargetInstruction(
    "Shl64r8i",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I8IMM)],
    "shl {dst}, {src2}",
    ("dst", "src1"),
)

SHR64R8I = TargetInstruction(
    "Shr64r8i",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I8IMM)],
    "shr {dst}, {src2}",
    ("dst", "src1"),
)

PUSH64R = TargetInstruction("Push64r", [], [("src", GPR64)], "push {src}")

POP64R = TargetInstruction("Pop64r", [("dst", GPR64)], [], "pop {dst}")

CALL64R = TargetInstruction("Call64r", [], [("src", GPR64)], "call {src}")

JMP = TargetInstruction("Jmp", [], [("dst", BLOCK)], "jmp {dst}")

JCC = TargetInstruction("Jcc", [], [("dst", BLOCK), ("cc", CCODE)], "j{cc} {dst}")

LEAVE = TargetInstruction("Leave", [], [], "leave")

RET = TargetInstruction("Ret", [], [], "ret")
