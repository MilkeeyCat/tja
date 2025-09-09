from .register import Register

class Constraint:
    pass

class Type(Constraint):
    pass

class RegisterClass(Constraint):
    name: str
    type: Type
    registers: list[Register]

    def __init__(self, name: str, type: Type, registers: list[Register]):
        self.name = name
        self.type = type
        self.registers = registers

        REGISTER_CLASSES.append(self)

REGISTER_CLASSES: list[RegisterClass] = []

class Operand(Constraint):
    pass
