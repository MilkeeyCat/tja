from .operands import *

from .instruction import Instruction

Instruction("Add", [("dst", R0)], [("lhs", R0), ("rhs", R0)])

Instruction("Sub", [("dst", R0)], [("lhs", R0), ("rhs", R0)])

Instruction("Mul", [("dst", R0)], [("lhs", R0), ("rhs", R0)])

Instruction("SDiv", [("dst", R0)], [("lhs", R0), ("rhs", R0)])

Instruction("UDiv", [("dst", R0)], [("lhs", R0), ("rhs", R0)])

Instruction("FrameIndex", [("dst", R0)], [("frame_idx", FRAME_IDX)])

Instruction("PtrAdd", [("dst", R0)], [("base", R0), ("offset", VREG_OR_IMM)])

Instruction("Load", [("dst", R1)], [("ptr", R0)])

Instruction("Store", [], [("ptr", R0), ("value", R1)])

Instruction("Br", [], [("target", BLOCK_IDX)])

Instruction("BrCond", [], [("cond", R0), ("target", BLOCK_IDX)])

Instruction("GlobalValue", [("dst", R0)], [("value", GLOBAL_OR_FN)])

Instruction("ICmp", [("dst", R0)], [("ccode", CCODE), ("lhs", R1), ("rhs", R1)])

Instruction("Copy", [("dst", R0)], [("value", VREG_OR_IMM)])
