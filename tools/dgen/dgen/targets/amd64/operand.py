from abc import ABC, abstractmethod
from enum import IntFlag

from dgen.register import Register
from dgen.register_class import RegisterClass


class Operand(ABC):
    pass

    @abstractmethod
    def type(self) -> str:
        pass


class ImmediateOperand(Operand):
    bits: int

    def __init__(self, bits: int) -> None:
        self.bits = bits

    def type(self) -> str:
        return "Immediate"


class MemoryOperand(Operand):
    bits: int

    def __init__(self, bits: int) -> None:
        self.bits = bits

    def type(self) -> str:
        return "Memory"


class EffectiveAddressOperand(Operand):
    def type(self) -> str:
        return "EffectiveAddress"


class BlockOperand(Operand):
    def type(self) -> str:
        return "BlockId"


class AccessType(IntFlag):
    Read = 1
    Write = 2
    ReadWrite = Read | Write


class RegisterOperand(Operand):
    reg_or_rc: Register | RegisterClass
    access_type: AccessType
    implicit: bool

    def __init__(
        self, reg_or_rc: Register | RegisterClass, access_type: AccessType
    ) -> None:
        self.reg_or_rc = reg_or_rc
        self.access_type = access_type
        self.implicit = False

    def type(self) -> str:
        return "Register"


def implicit(reg_op: RegisterOperand) -> RegisterOperand:
    reg_op.implicit = True

    return reg_op


def r(reg_or_rc: Register | RegisterClass) -> RegisterOperand:
    return RegisterOperand(reg_or_rc, AccessType.Read)


def w(reg_or_rc: Register | RegisterClass) -> RegisterOperand:
    return RegisterOperand(reg_or_rc, AccessType.Write)


def rw(reg_or_rc: Register | RegisterClass) -> RegisterOperand:
    return RegisterOperand(reg_or_rc, AccessType.ReadWrite)
