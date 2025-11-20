from enum import Enum
from typing import ClassVar

from .operands import Operand
from .type import *


class Instruction:
    enum: ClassVar[str]
    name: str
    outs: list[tuple[str, Operand]]
    ins: list[tuple[str, Operand]]

    def __init__(
        self, name: str, outs: list[tuple[str, Operand]], ins: list[tuple[str, Operand]]
    ):
        self.name = name
        self.outs = outs
        self.ins = ins


class TargetInstruction(Instruction):
    enum = "Opcode"
    asm: str
    tied_operands: tuple[str, str] | None

    def __init__(
        self,
        name: str,
        outs: list[tuple[str, Operand]],
        ins: list[tuple[str, Operand]],
        asm: str,
        tied_operands: tuple[str, str] | None = None,
    ):
        super().__init__(name, outs, ins)

        self.asm = asm

        if tied_operands is not None:
            if not any([name == tied_operands[0] for (name, _) in outs]):
                raise ValueError("first `tied_operands` element should be a def")
            if not any([name == tied_operands[1] for (name, _) in ins]):
                raise ValueError("second `tied_operands` element should be a use")

        self.tied_operands = tied_operands

        TARGET_INSTRUCTIONS.append(self)

    def get_operands_before(self, name: str) -> list[Operand]:
        operands: list[Operand] = []

        for op_name, operand in [*self.outs, *self.ins]:
            if name == op_name:
                return operands
            elif self.tied_operands is not None and self.tied_operands[1] == op_name:
                pass
            else:
                operands.append(operand)

        assert False

    def get_operand_idx(self, name: str):
        idx = 0

        for op_name, _ in [*self.outs, *self.ins]:
            if name == op_name:
                return idx
            elif self.tied_operands is not None and self.tied_operands[1] == op_name:
                pass
            else:
                idx += 1

        assert False

    def get_operand(self, name: str) -> Operand:
        return next(
            operand for op_name, operand in [*self.outs, *self.ins] if op_name == name
        )


TARGET_INSTRUCTIONS: list[TargetInstruction] = []


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
