from .operand import Operand


class Instruction:
    name: str
    outs: list[tuple[str, Operand]]
    ins: list[tuple[str, Operand]]

    def __init__(
        self, name: str, outs: list[tuple[str, Operand]], ins: list[tuple[str, Operand]]
    ):
        self.name = name
        self.outs = outs
        self.ins = ins

        INSTRUCTIONS.append(self)


INSTRUCTIONS: list[Instruction] = []
