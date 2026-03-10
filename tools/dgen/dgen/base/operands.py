from .operand import Operand

VREG = Operand("I::Register")
FRAME_IDX = Operand("FrameIdx")
BLOCK_IDX = Operand("BlockIdx<Self>")
VREG_OR_IMM = Operand("RegisterOrImmediate<I::Register>")
GLOBAL_OR_FN = Operand("GlobalOrFunction")
CCODE = Operand("ConditionCode")

# Rn represents a physical or virtual register of any type(i8, i16, i32, i64,
# ptr, etc.).
#
# In an instruction definition such as `G_ADD R0, R0`, both operands are of the
# same type.
R0 = VREG
R1 = VREG
