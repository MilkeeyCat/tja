from .operand import Operand


class Instruction:
    name: str
    mnemonic: str
    operands: list[Operand]

    def __init__(self, name: str, mnemonic: str, operands: list[Operand]):
        self.name = name
        self.mnemonic = mnemonic
        self.operands = operands

        INSTRUCTIONS.append(self)


INSTRUCTIONS: list[Instruction] = []
