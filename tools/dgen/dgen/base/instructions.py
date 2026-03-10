from .operands import *

from .instruction import Instruction

Instruction("Add", [R0], [R0, R0])

Instruction("Sub", [R0], [R0, R0])

Instruction("Mul", [R0], [R0, VREG_OR_IMM])

Instruction("SDiv", [R0], [R0, R0])

Instruction("UDiv", [R0], [R0, R0])

Instruction("FrameIndex", [R0], [FRAME_IDX])

Instruction("PtrAdd", [R0], [R0, VREG_OR_IMM])

Instruction("Load", [R1], [R0])

Instruction("Store", [], [R0, R1])

Instruction("Br", [], [BLOCK_IDX])

Instruction("BrCond", [], [R0, BLOCK_IDX])

Instruction("GlobalValue", [R0], [GLOBAL_OR_FN])

Instruction("ICmp", [R0], [CCODE, R1, R1])

Instruction("Copy", [R0], [VREG_OR_IMM])
