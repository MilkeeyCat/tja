from .operand import Generic, Operand


class Instruction:
    name: str
    mnemonic: str
    operands: list[Operand]

    def __init__(
        self,
        name: str,
        mnemonic: str,
        operands: list[Operand],
    ):
        self.name = name
        self.mnemonic = mnemonic
        self.operands = operands

        INSTRUCTIONS.append(self)

    def generics(self) -> list[Generic]:
        return sorted(
            list(
                set(
                    [
                        generic
                        for operand in self.operands
                        for generic in operand.generics
                    ]
                )
            ),
            key=lambda generic: generic.name,
        )


INSTRUCTIONS: list[Instruction] = []
