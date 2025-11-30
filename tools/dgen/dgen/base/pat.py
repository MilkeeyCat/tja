from abc import ABC, abstractmethod
from copy import deepcopy

from .isel_generator_ctx import IselGeneratorCtx
from .instruction import Instruction, TargetInstruction
from .operands import Def, Nameble, Operand, Register, Use as UsePred


class Pat:
    pass


class Match(ABC, Pat):
    @abstractmethod
    def match(self, ctx: IselGeneratorCtx):
        pass


class Named(Match):
    name: str
    pat: Nameble

    def __init__(self, name: str, pat: Nameble):
        self.name = name
        self.pat = pat

    def define(self, ctx: IselGeneratorCtx):
        self.pat.define(self.name, ctx)

    def match(self, ctx: IselGeneratorCtx):
        writer = ctx.writer

        writer.writeln(f"Box::new(Value::new(Some(&mut {self.name}), [")
        writer.indent()

        self.pat.generate_predicates(ctx)

        writer.dedent()
        writer.writeln("] as [Box<dyn Predicate<_, _>>; _])),")


class _Unnamed(Match):
    operand: Operand

    def __init__(self, operand: Operand):
        self.operand = operand

    def match(self, ctx: IselGeneratorCtx):
        writer = ctx.writer

        writer.writeln(
            f"Box::new(Value::<'_, T, {self.operand.struct}, _>::new(None, ["
        )
        writer.indent()

        self.operand.generate_predicates(ctx)

        writer.dedent()
        writer.writeln("] as [Box<dyn Predicate<_, _>>; _])),")


class Replacement(ABC, Pat):
    @abstractmethod
    def replace(self, ctx: IselGeneratorCtx):
        pass


class Use(Replacement):
    name: str

    def __init__(self, name: str):
        self.name = name

    def replace(self, ctx: IselGeneratorCtx):
        writer = ctx.writer

        writer.writeln(f"builder.extend({self.name}.inner());")


class MatchInstr(Match):
    instr: Instruction
    patterns: list[Match]

    def __init__(self, instr: Instruction, *patterns: Match):
        self.instr = instr
        self.patterns = [deepcopy(pat) for pat in patterns]

    def match(self, ctx: IselGeneratorCtx):
        writer = ctx.writer

        writer.buf.write("Instruction::new(\n")
        writer.indent()
        writer.writeln(f"{self.instr.enum}::{self.instr.name}.into(),")
        writer.writeln("vec![")
        writer.indent()

        for pat in self.patterns:
            pat.match(ctx)

        writer.dedent()
        writer.writeln("],")
        writer.dedent()
        writer.writeln(")")
        writer.writeln(".matches(&ctx, idx)")

    def insert_defs_and_preds(self):
        for pat in self.patterns:
            if isinstance(pat, Named) and isinstance(pat.pat, Register):
                pat.pat.predicates.append(UsePred())

        # TODO: infer the predicates for the operand
        self.patterns.insert(0, _Unnamed(Register([Def()])))

        for pat in self.patterns:
            if isinstance(pat, MatchInstr):
                pat.insert_defs_and_preds()


class ReplacementInstr(Replacement):
    instr: Instruction
    patterns: list[Replacement]

    def __init__(self, instr: Instruction, *patterns: Replacement):
        self.instr = instr
        self.patterns = list(patterns)

    def replace(self, ctx: IselGeneratorCtx):
        writer = ctx.writer

        for pat in self.patterns:
            pat.replace(ctx)

        if (
            isinstance(self.instr, TargetInstruction)
            and self.instr.tied_operands is not None
        ):
            def_idx = self.instr.get_operand_idx(self.instr.tied_operands[0])
            use_idx = self.instr.get_operand_idx(self.instr.tied_operands[1])

            writer.writeln(
                f"builder.set_tied_operands({def_idx}.into(), {use_idx}.into());"
            )


class IselPat:
    match: MatchInstr
    replacement: list[ReplacementInstr]

    def __init__(
        self,
        match: MatchInstr,
        replacement: ReplacementInstr | list[ReplacementInstr],
    ):
        match.insert_defs_and_preds()

        self.match = match

        if isinstance(replacement, ReplacementInstr):
            self.replacement = [replacement]
        else:
            self.replacement = replacement

        # TODO: asserts about replacement instructions:
        # - if match has an explicit out, one of replacements must have one too
        # - at most 1 instruction can have > 0 explicit outs

        ISEL_PATTERNS.append(self)

    def generate_replacement(self, ctx: IselGeneratorCtx):
        writer = ctx.writer
        edit_instr_idx = next(
            idx
            for (idx, replacement) in enumerate(reversed(self.replacement))
            if len(replacement.instr.outs) == len(self.match.instr.outs)
        )
        before = list(range(0, edit_instr_idx))
        after = list(range(edit_instr_idx + 1, len(self.replacement)))

        for idx, instr in enumerate(self.replacement):
            if idx == edit_instr_idx:
                continue

            is_mut = len(instr.patterns) > 0

            writer.writeln(
                f"let{' mut ' if is_mut else ' '}builder = instr_cursor.func.create_instr()"
            )
            writer.indent()
            writer.writeln(
                f".with_opcode({instr.instr.__class__.enum}::{instr.instr.name}.into());"
            )
            writer.dedent()
            writer.writeln("")

            instr.replace(ctx)

            writer.writeln(f"let instr_idx_{idx} = builder.idx();")
            writer.writeln("")

        writer.writeln(
            "let mut builder = InstructionBuilder::new(instr_cursor.func, idx);"
        )
        writer.writeln("")
        writer.writeln(
            f"builder.set_opcode({self.replacement[edit_instr_idx].instr.__class__.enum}::{self.replacement[edit_instr_idx].instr.name}.into());"
        )
        writer.writeln("builder.clear_inputs();")

        self.replacement[edit_instr_idx].replace(ctx)

        if after:
            writer.writeln("")
            writer.writeln("instr_cursor.move_next();")

            for idx in after:
                writer.writeln(f"instr_cursor.insert_before(instr_idx_{idx});")

            writer.writeln("instr_cursor.move_prev();")

        if before:
            writer.writeln("")

            for idx in before:
                writer.writeln(f"instr_cursor.insert_before(instr_idx_{idx});")


ISEL_PATTERNS: list[IselPat] = []
