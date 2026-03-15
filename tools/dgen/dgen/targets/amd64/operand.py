from abc import ABC, abstractmethod
from enum import IntFlag

from dgen.base.register import Register as PhysicalRegister


class Generic:
    name: str
    trait_bound: str

    def __init__(self, name: str, trait_bound: str):
        self.name = name
        self.trait_bound = trait_bound


REGISTER = Generic("R", "Register")


class AccessType(IntFlag):
    Read = 1
    Write = 2
    ReadWrite = Read | Write


class Operand(ABC):
    generics: list[Generic]

    def __init__(self, generics: list[Generic]) -> None:
        self.generics = generics

    @abstractmethod
    def print(self, access_type: AccessType) -> str:
        pass


class Explicit(Operand):
    type_name: str
    generics: list[Generic]

    def __init__(self, type_name: str, generics: list[Generic] | None = None) -> None:
        super().__init__(generics or [])

        self.type_name = type_name

    def print(self, access_type: AccessType) -> str:
        return self.type_name


class Register(Explicit):
    def __init__(self) -> None:
        super().__init__("R", [REGISTER])

    def print(self, access_type: AccessType) -> str:
        if access_type == AccessType.ReadWrite:
            return "ReadWrite<" + self.type_name + ">"
        else:
            return self.type_name


class Implicit(Operand):
    reg: PhysicalRegister

    def __init__(self, reg: PhysicalRegister) -> None:
        super().__init__([REGISTER])

        self.reg = reg

    def print(self, access_type: AccessType) -> str:
        if access_type == AccessType.ReadWrite:
            return "ReadWrite<R>"
        else:
            return "R"


class InstructionOperand:
    operand: Operand
    access_type: AccessType

    def __init__(self, operand: Operand, access_type: AccessType):
        self.operand = operand
        self.access_type = access_type


def implicit(reg: PhysicalRegister) -> Implicit:
    return Implicit(reg)


def r(operand: Operand) -> InstructionOperand:
    return InstructionOperand(operand, AccessType.Read)


def w(operand: Operand) -> InstructionOperand:
    return InstructionOperand(operand, AccessType.Write)


def rw(operand: Operand) -> InstructionOperand:
    return InstructionOperand(operand, AccessType.ReadWrite)
