from .operand import Explicit, Register, REGISTER

R8 = R16 = R32 = R64 = Register()
ADDR = Explicit("AddressMode<R>", [REGISTER])
MEM8 = MEM16 = MEM32 = MEM64 = Explicit("Memory<R>", [REGISTER])
IMM8 = IMM16 = IMM32 = IMM64 = Explicit("i64")
BLOCK_IDX = Explicit("BlockIdx")
CCODE = Explicit("ConditionCode")
