from typing import Self

from dgen.base.operands import HasRegisterClass, Operand, Predicate, Register
from . import register_classes as rcs


class Memory(Operand):
    def __init__(self, predicates: list["Predicate[Self]"], print_method: str):
        super().__init__("AddressMode", predicates, print_method)


I8MEM = Memory([], "emit_byte_memory")
I16MEM = Memory([], "emit_word_memory")
I32MEM = Memory([], "emit_dword_memory")
I64MEM = Memory([], "emit_qword_memory")

GPR8 = Register([HasRegisterClass(rcs.GPR8)])
GPR16 = Register([HasRegisterClass(rcs.GPR16)])
GPR32 = Register([HasRegisterClass(rcs.GPR32)])
GPR64 = Register([HasRegisterClass(rcs.GPR64)])
