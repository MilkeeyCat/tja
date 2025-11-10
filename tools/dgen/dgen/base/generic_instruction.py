from dgen.base.operands import Operand, Register, Any

from .instruction import Instruction


class GenericInstruction(Instruction):
    enum = "GenericOpcode"
    name: str

    def __init__(
        self, name: str, outs: list[tuple[str, Operand]], ins: list[tuple[str, Operand]]
    ):
        super().__init__(name, outs, ins)

        GENERIC_INSTRUCTIONS.append(self)


GENERIC_INSTRUCTIONS: list[GenericInstruction] = []


# Operand representing a physical or virtual register of any type(i8, i16, i32,
# i64, ptr, etc.). The concrete type is used in isel patterns.
#
# In an instruction definition such as `G_ADD R0, R0`, both operands are of the
# same type.
class GenericRegister(Register):
    pass


R0 = GenericRegister([])
R1 = GenericRegister([])

UNKNOWN = Any([])

G_ADD = GenericInstruction("Add", [("dst", R0)], [("src1", R0), ("src2", R0)])

G_SUB = GenericInstruction("Sub", [("dst", R0)], [("src1", R0), ("src2", R0)])

G_MUL = GenericInstruction("Mul", [("dst", R0)], [("src1", R0), ("src2", R0)])

G_SDIV = GenericInstruction("SDiv", [("dst", R0)], [("src1", R0), ("src2", R0)])

G_UDIV = GenericInstruction("UDiv", [("dst", R0)], [("src1", R0), ("src2", R0)])

G_FRAME_INDEX = GenericInstruction("FrameIndex", [("dst", R0)], [("src", UNKNOWN)])

G_PTR_ADD = GenericInstruction(
    "PtrAdd", [("dst", R0)], [("src1", R0), ("src2", UNKNOWN)]
)

G_LOAD = GenericInstruction("Load", [("dst", R0)], [("src", R1)])

G_STORE = GenericInstruction("Store", [], [("src1", R0), ("src2", UNKNOWN)])

G_BR = GenericInstruction("Br", [], [("src", UNKNOWN)])

G_GLOBAL_VALUE = GenericInstruction("GlobalValue", [("dst", R0)], [("src", UNKNOWN)])

G_COPY = GenericInstruction("Copy", [("dst", R0)], [("src", UNKNOWN)])
