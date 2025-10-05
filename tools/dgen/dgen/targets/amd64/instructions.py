from dgen.base.instruction import *

from .register_classes import *
from .operands import *

MOV8rr = TargetInstruction(
    "Mov8rr",
    [("dst", GPR8)],
    [("src1", GPR8), ("src2", GPR8)],
    "mov {dst}, {src2}",
    ("dst", "src1"),
)
MOV8ri = TargetInstruction(
    "Mov8ri",
    [("dst", GPR8)],
    [("src1", I8IMM), ("src2", I8IMM)],
    "mov {dst}, {src2}",
    ("dst", "src1"),
)
MOV8rm = TargetInstruction(
    "Mov8rm",
    [("dst", GPR8)],
    [("src1", I8MEM), ("src2", I8MEM)],
    "mov {dst}, {src2}",
    ("dst", "src1"),
)

Add32rr = TargetInstruction(
    "Add32rr",
    [("dst", GPR32)],
    [("src1", GPR32), ("src2", GPR32)],
    "add {dst}, {src2}",
    ("dst", "src1"),
)
