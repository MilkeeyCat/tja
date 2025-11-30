from abc import ABC, abstractmethod
from typing import Generic, Self, TypeVar

from .isel_generator_ctx import IselGeneratorCtx
from .register_class import RegisterClass
from .type import *


# it really belongs to pat module but it will cause circular import, let it be
# here for now
class Nameble(ABC):
    @abstractmethod
    def define(self, name: str, ctx: IselGeneratorCtx):
        pass

    @abstractmethod
    def generate_predicates(self, ctx: IselGeneratorCtx):
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

    def define(self, name: str, ctx: IselGeneratorCtx):
        ctx.writer.writeln(f"let mut {name} = Uninitialized::<{self.struct}>::new();")

    def generate_predicates(self, ctx: IselGeneratorCtx):
        for pred in self.predicates:
            pred.generate(ctx)


class Immediate(Operand):
    def __init__(self, predicates: list["Predicate[Self]"]):
        super().__init__("Immediate", predicates, "emit_operand")


class Register(Operand):
    def __init__(self, predicates: list["Predicate[Self]"]):
        super().__init__("Register", predicates, "emit_operand")


class Block(Operand):
    def __init__(self):
        super().__init__("Block", [], "emit_operand")


class ConditionCode(Operand):
    def __init__(self):
        super().__init__("ConditionCode", [], "")


CCODE = ConditionCode()


class Any(Operand):
    def __init__(self, predicates: list["Predicate[Self]"]):
        super().__init__("Operand", predicates, "emit_operand")


T = TypeVar("T", bound=Operand)


class Predicate(ABC, Generic[T]):
    @abstractmethod
    def generate(self, ctx: IselGeneratorCtx):
        pass


class HasType(Predicate[Immediate]):
    type: Type

    def __init__(self, type: Type):
        self.type = type

    def generate(self, ctx: IselGeneratorCtx):
        ctx.writer.writeln(f"HasType::new({self.type.type_idx}.into()),")


class HasRegisterClass(Predicate[Register]):
    register_class: RegisterClass

    def __init__(self, register_class: RegisterClass):
        self.register_class = register_class

    def generate(self, ctx: IselGeneratorCtx):
        ctx.writer.writeln(
            f"HasRegisterClass::new(RegisterClass::{self.register_class.name}.into()),"
        )


class Def(Predicate[Register]):
    def generate(self, ctx: IselGeneratorCtx):
        ctx.writer.writeln("Def::new(),")


class Use(Predicate[Register]):
    def generate(self, ctx: IselGeneratorCtx):
        ctx.writer.writeln("Use::new(),")


I8IMM = Immediate([HasType(I8)])
I16IMM = Immediate([HasType(I16)])
I32IMM = Immediate([HasType(I32)])
I64IMM = Immediate([HasType(I64)])

BLOCK = Block()
