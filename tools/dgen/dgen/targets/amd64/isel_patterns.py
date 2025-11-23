from dgen.base.pat import *
from dgen.base.generic_instruction import *

from .register_classes import *
from .instructions import *

# This line is here becase the import above imports predicate with name `Use` -.-
from dgen.base.pat import Use


def get_instruction(name: str) -> Instruction:
    instruction = globals()[name]

    if not isinstance(instruction, Instruction):
        assert False

    return instruction


# NOTE: can't reuse `add` name because instructions module already declares
# `add` function. Think about better naming?
def add_pats():
    sizes = [8, 16, 32, 64]

    for size in sizes:
        r = get_operand("r", size)
        i = get_operand("i", size)

        IselPat(
            MatchInstr(G_ADD, Named("src1", r), Named("src2", i)),
            ReplacementInstr(get_instruction(f"ADD{size}RI"), Use("src1"), Use("src2")),
        )

        IselPat(
            MatchInstr(G_ADD, Named("src1", r), Named("src2", r)),
            ReplacementInstr(get_instruction(f"ADD{size}RR"), Use("src1"), Use("src2")),
        )


add_pats()
