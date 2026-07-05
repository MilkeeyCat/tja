from typing import Self


class Register:
    name: str
    bits: int
    subregs: list[Self]

    def __init__(self, name: str, bits: int, subregs: list[Self] | None = None):
        self.name = name
        self.bits = bits
        self.subregs = subregs or []

        REGISTERS.append(self)


REGISTERS: list[Register] = []
