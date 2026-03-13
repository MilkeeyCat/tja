from .operand import ExplicitValue, REGISTER

R8 = R16 = R32 = R64 = ExplicitValue("R", [REGISTER])
ADDR = ExplicitValue("AddressMode<R>", [REGISTER])
MEM8 = MEM16 = MEM32 = MEM64 = ExplicitValue("Memory<R>", [REGISTER])
IMM8 = IMM16 = IMM32 = IMM64 = ExplicitValue("i64")
BLOCK_IDX = ExplicitValue("BlockIdx")
CCODE = ExplicitValue("ConditionCode")
