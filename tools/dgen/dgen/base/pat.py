from .instruction import Instruction
from .constraint import Constraint
from .type import Type


class Pat:
    pass


class Match(Pat):
    pass


class Named(Match):
    name: str
    pat: Match

    def __init__(self, name: str, pat: Match):
        self.name = name
        self.pat = pat


class Replacement(Pat):
    pass


class Use(Replacement):
    name: str

    def __init__(self, name: str):
        self.name = name


class Constrained(Match, Replacement):
    constraint: Constraint

    def __init__(self, constraint: Constraint):
        self.constraint = constraint


class Const(Match, Replacement):
    value: int | float
    type: Type

    def __init__(self, value: int | float, type: Type):
        self.value = value
        self.type = type


class MatchInstr(Match):
    instr: Instruction
    patterns: list[Match]

    def __init__(self, instr: Instruction, *patterns: Match):
        self.instr = instr
        self.patterns = list(patterns)


class ReplacementInstr(Replacement):
    instr: Instruction
    patterns: list[Replacement]

    def __init__(self, instr: Instruction, *patterns: Replacement):
        self.instr = instr
        self.patterns = list(patterns)


class IselPat:
    match: MatchInstr
    replacement: list[ReplacementInstr]

    def __init__(
        self,
        match: MatchInstr,
        replacement: list[ReplacementInstr],
    ):
        self.match = match
        self.replacement = replacement

        ISEL_PATTERNS.append(self)


ISEL_PATTERNS: list[IselPat] = []
