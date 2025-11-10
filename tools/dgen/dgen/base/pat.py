from .instruction import Instruction
from .operands import Nameble


class Pat:
    pass


class Match(Pat):
    pass


class Named(Match):
    name: str
    pat: Nameble

    def __init__(self, name: str, pat: Nameble):
        self.name = name
        self.pat = pat


class Replacement(Pat):
    pass


class Use(Replacement):
    name: str

    def __init__(self, name: str):
        self.name = name


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
    replacement: ReplacementInstr

    def __init__(
        self,
        match: MatchInstr,
        replacement: ReplacementInstr,
    ):
        self.match = match
        self.replacement = replacement

        ISEL_PATTERNS.append(self)


ISEL_PATTERNS: list[IselPat] = []
