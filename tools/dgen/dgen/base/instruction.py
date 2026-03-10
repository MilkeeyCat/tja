from .operand import Operand


class Instruction:
    name: str
    outs: list[Operand]
    ins: list[Operand]

    def __init__(self, name: str, outs: list[Operand], ins: list[Operand]):
        self.name = name
        self.outs = outs
        self.ins = ins

        INSTRUCTIONS.append(self)


INSTRUCTIONS: list[Instruction] = []
