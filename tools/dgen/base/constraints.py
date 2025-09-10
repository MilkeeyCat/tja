from .register import Register

class Constraint:
    pass

class Type(Constraint):
    type_idx: int

    def __init__(self, type_idx: int):
        self.type_idx = type_idx


i8 = Type(1)
i16 = Type(2)
i32 = Type(3)
i64 = Type(4)

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
