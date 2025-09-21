from dgen.base.instruction import *

from .register_classes import *
from .operands import *

MOV8rr = Instruction("Mov8rr", [("dst", GPR8), ("src", GPR8)], "mov {dst}, {src}")
MOV8ri = Instruction("Mov8ri", [("dst", GPR8), ("src", I8IMM)], "mov {dst}, {src}")
MOV8rm = Instruction("Mov8rm", [("dst", GPR8), ("src", I8MEM)], "mov {dst}, {src}")
