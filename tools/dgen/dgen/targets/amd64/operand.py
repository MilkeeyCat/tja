from abc import ABC, abstractmethod
from enum import IntFlag

from dgen.base.register import Register


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
    access_type: AccessType
    generics: list[Generic]

    def __init__(self, access_type: AccessType, generics: list[Generic]) -> None:
        self.access_type = access_type
        self.generics = generics

    @abstractmethod
    def print(self) -> str:
        pass


class Explicit(Operand):
    type_name: str
    generics: list[Generic]

    def __init__(
        self, access_type: AccessType, type_name: str, generics: list[Generic]
    ) -> None:
        super().__init__(access_type, generics)

        self.type_name = type_name

    def print(self) -> str:
        if self.access_type == AccessType.ReadWrite:
            return "ReadWrite<" + self.type_name + ">"
        else:
            return self.type_name


class Implicit(Operand):
    reg: Register

    def __init__(self, access_type: AccessType, reg: Register) -> None:
        super().__init__(access_type, [REGISTER])

        self.reg = reg

    def print(self) -> str:
        if self.access_type == AccessType.ReadWrite:
            return "ReadWrite<R>"
        else:
            return "R"


class Value(ABC):
    @abstractmethod
    def into_operand(self, access_type: AccessType) -> Operand:
        pass


class ImplicitValue(Value):
    reg: Register

    def __init__(self, reg: Register):
        self.reg = reg

    def into_operand(self, access_type: AccessType) -> Operand:
        return Implicit(access_type, self.reg)


class ExplicitValue(Value):
    type_name: str
    generics: list[Generic]

    def __init__(self, type_name: str, generics: list[Generic] | None = None):
        self.type_name = type_name
        self.generics = generics or []

    def into_operand(self, access_type: AccessType) -> Operand:
        return Explicit(access_type, self.type_name, self.generics)


def implicit(reg: Register) -> ImplicitValue:
    return ImplicitValue(reg)


def r(value: Value) -> Operand:
    return value.into_operand(AccessType.Read)


def w(value: Value) -> Operand:
    return value.into_operand(AccessType.Write)


def rw(value: Value) -> Operand:
    return value.into_operand(AccessType.ReadWrite)
