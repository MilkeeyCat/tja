from dgen.base.instruction import Operand, I8IMM, I32IMM

from .register_classes import GPR64


class Address(Operand):
    emit_method = "emit_address"
    operands: list[Operand]

    def __init__(self):
        self.operands = [GPR64, GPR64, I8IMM, I32IMM]
        self.operands_len = sum(map(lambda op: op.operands_len, self.operands))


class Memory(Address):
    def __init__(self, emit_method: str):
        super().__init__()

        self.emit_method = emit_method


I8MEM = Memory("emit_byte_memory")
I16MEM = Memory("emit_word_memory")
I32MEM = Memory("emit_dword_memory")
I64MEM = Memory("emit_qword_memory")
