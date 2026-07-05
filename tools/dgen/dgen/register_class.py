from .register import Register


class RegisterClass:
    name: str
    registers: list[Register]

    def __init__(self, name: str, registers: list[Register]):
        self.name = name
        self.registers = registers

        REGISTER_CLASSES.append(self)


REGISTER_CLASSES: list[RegisterClass] = []
