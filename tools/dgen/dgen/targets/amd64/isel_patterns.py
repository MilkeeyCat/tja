from dgen.base.pat import *
from dgen.base.generic_instruction import *

from .register_classes import *
from .instructions import *

# This line is here becase the import above imports predicate with name `Use` -.-
from dgen.base.pat import Use

IselPat(
    MatchInstr(G_ADD, Named("src1", GPR32), Named("src2", GPR32)),
    ReplacementInstr(ADD32RR, Use("src1"), Use("src2")),
)
