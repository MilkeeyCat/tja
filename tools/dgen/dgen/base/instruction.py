from enum import Enum

from .register import Register
from .constraints import *


class Operand:
    operands_len: int = 1
    emit_method: str = "emit_operand"


class RegisterClass(Operand):
    name: str
    registers: list[Register]

    def __init__(self, name: str, type: Type, registers: list[Register]):
        self.name = name
        self.registers = registers

        REGISTER_CLASSES.append(self)


REGISTER_CLASSES: list[RegisterClass] = []


class Immediate(Operand):
    type: Type

    def __init__(self, type: Type):
        self.type = type


I8IMM = Immediate(i8)
I16IMM = Immediate(i16)
I32IMM = Immediate(i32)
I64IMM = Immediate(i64)


class Instruction:
    name: str
    operands: list[tuple[str, Operand]]
    asm: str

    def __init__(self, name: str, operands: list[tuple[str, Operand]], asm: str):
        self.name = name
        self.operands = operands
        self.asm = asm

        INSTRUCTIONS.append(self)

    def get_operand(self, name: str) -> tuple[int, Operand]:
        offset = 0

        for op_name, operand in self.operands:
            if name == op_name:
                return (offset, operand)
            else:
                offset += operand.operands_len

        assert False


INSTRUCTIONS: list[Instruction] = []


class TokenType(Enum):
    STRING = 0
    IDENT = 1


def parse_asm_string(input: str) -> list[tuple[TokenType, str]]:
    result: list[tuple[TokenType, str]] = []
    in_braces = False
    pos = 0
    cur_pos = 0

    while cur_pos < len(input):
        match input[cur_pos]:
            case "{":
                assert not in_braces
                in_braces = True

                if input[pos:cur_pos] != "":
                    result.append((TokenType.STRING, input[pos:cur_pos]))

                pos = cur_pos + 1
            case "}":
                assert in_braces
                in_braces = False

                if input[pos:cur_pos] != "":
                    result.append((TokenType.IDENT, input[pos:cur_pos]))

                pos = cur_pos + 1
            case _:
                pass

        cur_pos += 1

    assert not in_braces

    if input[pos:cur_pos] != "":
        result.append((TokenType.STRING, input[pos:cur_pos]))

    return result
