from .operand import Explicit, Memory, REGISTER

R8 = R16 = R32 = R64 = Explicit("R", [REGISTER])
ADDR = Explicit("AddressMode<R>", [REGISTER])
MEM8 = Memory(8)
MEM16 = Memory(16)
MEM32 = Memory(32)
MEM64 = Memory(64)
IMM8 = IMM16 = IMM32 = IMM64 = Explicit("i64")
BLOCK_IDX = Explicit("BlockIdx")
CCODE = Explicit("ConditionCode")
