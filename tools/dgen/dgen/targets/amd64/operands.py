from .operand import (
    EffectiveAddressOperand,
    BlockOperand,
    ImmediateOperand,
    MemoryOperand,
)

IMM8 = ImmediateOperand(8)
IMM16 = ImmediateOperand(16)
IMM32 = ImmediateOperand(32)
IMM64 = ImmediateOperand(64)

MEM8 = MemoryOperand(8)
MEM16 = MemoryOperand(16)
MEM32 = MemoryOperand(32)
MEM64 = MemoryOperand(64)

ADDR = EffectiveAddressOperand()

BLOCK = BlockOperand()
