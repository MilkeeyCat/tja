from io import TextIOWrapper

from dgen.writer import Writer

from .isel_generator_ctx import IselGeneratorCtx
from .pat import IselPat, ISEL_PATTERNS, Named
from .instruction import Instruction


def generate_instruction_selector(buf: TextIOWrapper):
    patterns: dict[Instruction, list[IselPat]] = {}

    for pat in ISEL_PATTERNS:
        patterns.setdefault(pat.match.instr, []).append(pat)

    # TODO: sort patterns by complexity

    writer = Writer(buf)

    writer.writeln("impl InstructionSelection {")
    writer.indent()
    writer.writeln("pub(super) fn select_instr<T: Target>(")
    writer.indent()
    writer.writeln("&self,")
    writer.writeln("ctx: &Context<'_, T>,")
    writer.writeln("instr_cursor: &mut InstructionCursorMut,")
    writer.dedent()
    writer.writeln(") -> bool {")
    writer.indent()
    writer.writeln("let idx = instr_cursor.idx().unwrap();")
    writer.writeln("let ctx = PatternCtx {")
    writer.indent()
    writer.writeln("func: instr_cursor.func,")
    writer.writeln("ctx,")
    writer.dedent()
    writer.writeln("};")
    writer.writeln("")
    writer.writeln("match ctx.func.instructions[idx].opcode {")
    writer.indent()

    for instr, pats in patterns.items():
        writer.writeln(
            f"opcode if opcode == {instr.__class__.enum}::{instr.name}.into() => {{"
        )
        writer.indent()

        for pat in pats:
            ctx = IselGeneratorCtx(writer)

            writer.writeln("{")
            writer.indent()

            for match_pat in pat.match.patterns:
                if isinstance(match_pat, Named):
                    match_pat.define(ctx)

            writer.writeln("")
            writer.write("if ")

            pat.match.match(ctx)

            writer.writeln("{")
            writer.indent()

            pat.generate_replacement(ctx)

            writer.writeln("")
            writer.writeln("return true;")
            writer.dedent()
            writer.writeln("}")

            writer.dedent()
            writer.writeln("}")
            writer.writeln("")

        writer.writeln("false")
        writer.dedent()
        writer.writeln("}")

    writer.writeln("_ => false,")
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
