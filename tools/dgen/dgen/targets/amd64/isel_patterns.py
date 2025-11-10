from dgen.base.generic_instruction import *
from dgen.base.pat import *

from .register_classes import *
from .instructions import *


IselPat(
    MatchInstr(
        G_ADD,
        Named("dst", GPR32),
        Named("src1", GPR32),
        Named("src2", GPR32),
    ),
    [ReplacementInstr(Add32rr, Use("dst"), Use("src1"), Use("src2"))],
)
