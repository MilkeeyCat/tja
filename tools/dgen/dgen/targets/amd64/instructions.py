from typing import overload
from dgen.base.instruction import *
from dgen.base.operands import *

from .register_classes import *
from .operands import *

O = TypeVar("O", bound=Operand)


@overload
def get_operand(name: str, type: type[O]) -> O:
    ...


@overload
def get_operand(variant: str, size: int) -> Operand:
    ...


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
    [MOV8rr, MOV8rm, MOV8mr, MOV8mi, MOV8ri],
    [MOV16rr, MOV16rm, MOV16mr, MOV16mi, MOV16ri],
    [MOV32rr, MOV32rm, MOV32mr, MOV32mi, MOV32ri],
    [MOV64rr, MOV64rm, MOV64mr, MOV64ri],
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
    [ADD8rr, ADD8rm, ADD8mr, ADD8mi, ADD8ri],
    [ADD16rr, ADD16rm, ADD16mr, ADD16mi, ADD16ri],
    [ADD32rr, ADD32rm, ADD32mr, ADD32mi, ADD32ri],
    [ADD64rr, ADD64rm, ADD64mr, ADD64mi, ADD64ri],
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
    [SUB8rr, SUB8rm, SUB8mr, SUB8mi, SUB8ri],
    [SUB16rr, SUB16rm, SUB16mr, SUB16mi, SUB16ri],
    [SUB32rr, SUB32rm, SUB32mr, SUB32mi, SUB32ri],
    [SUB64rr, SUB64rm, SUB64mr, SUB64mi, SUB64ri],
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
    [TEST8rr, TEST8mr, TEST8mi, TEST8ri],
    [TEST16rr, TEST16mr, TEST16mi, TEST16ri],
    [TEST32rr, TEST32mr, TEST32mi, TEST32ri],
    [TEST32rr, TEST32mr],
] = test()

LEA64 = TargetInstruction(
    "Lea64",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", ADDR)],
    "lea {dst}, {src2}",
    ("dst", "src1"),
)

SHL64r8i = TargetInstruction(
    "Shl64r8i",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I8IMM)],
    "shl {dst}, {src2}",
    ("dst", "src1"),
)

SHR64r8i = TargetInstruction(
    "Shr64r8i",
    [("dst", GPR64)],
    [("src1", GPR64), ("src2", I8IMM)],
    "shr {dst}, {src2}",
    ("dst", "src1"),
)

PUSH64r = TargetInstruction("Push64r", [], [("src", GPR64)], "push {src}")

POP64r = TargetInstruction("Pop64r", [("dst", GPR64)], [], "pop {dst}")

CALL64r = TargetInstruction("Call64r", [], [("src", GPR64)], "call {src}")

JMP = TargetInstruction("Jmp", [], [("dst", BLOCK)], "jmp {dst}")

JCC = TargetInstruction("Jcc", [], [("dst", BLOCK), ("cc", CCODE)], "j{cc} {dst}")

LEAVE = TargetInstruction(f"Leave", [], [], "leave")

RET = TargetInstruction(f"Ret", [], [], "ret")
