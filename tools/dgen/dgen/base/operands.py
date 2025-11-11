from abc import ABC, abstractmethod
from typing import Generic, Self, TypeVar

from .register_class import RegisterClass
from .type import *


# it really belongs to pat module but it will cause circular import, let it be
# here for now
class Nameble(ABC):
    @abstractmethod
    def generate(self):
        pass


class Operand(Nameble):
    struct: str
    predicates: list["Predicate[Self]"]
    emit_method: str

    def __init__(
        self, struct: str, predicates: list["Predicate[Self]"], emit_method: str
    ):
        self.struct = struct
        self.predicates = predicates
        self.emit_method = emit_method

    def generate(self):
        assert False, "unimplemented"


class Immediate(Operand):
    def __init__(self, predicates: list["Predicate[Self]"]):
        super().__init__("Immediate", predicates, "emit_operand")


class Register(Operand):
    def __init__(self, predicates: list["Predicate[Self]"]):
        super().__init__("Register", predicates, "emit_operand")


class Any(Operand):
    def __init__(self, predicates: list["Predicate[Self]"]):
        super().__init__("Operand", predicates, "emit_operand")


T = TypeVar("T", bound=Operand)


class Predicate(ABC, Generic[T]):
    @abstractmethod
    def generate(self):
        pass


class HasType(Predicate[Immediate]):
    type: Type

    def __init__(self, type: Type):
        self.type = type

    def generate(self):
        assert False, "unimplemented"


class HasRegisterClass(Predicate[Register]):
    register_class: RegisterClass

    def __init__(self, register_class: RegisterClass):
        self.register_class = register_class

    def generate(self):
        assert False, "unimplemented"


I8IMM = Immediate([HasType(I8)])
I16IMM = Immediate([HasType(I16)])
I32IMM = Immediate([HasType(I32)])
I64IMM = Immediate([HasType(I64)])
