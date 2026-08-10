import importlib

from io import TextIOWrapper

from dgen.writer import Writer

from .instruction import INSTRUCTIONS


def generate_instruction(buf: TextIOWrapper):
    importlib.import_module(".instructions", __package__)
    writer = Writer(buf)

    writer.writeln(
        '#[allow(non_camel_case_types, reason="easier to read, feels more asm-like")]'
    )
    writer.writeln('#[allow(dead_code, reason="generated code")]')
    writer.writeln("#[derive(Debug)]")
    writer.writeln("pub(crate) enum TargetInstruction {")
    writer.indent()

    for instr in INSTRUCTIONS:
        writer.writeln(f"{instr.name}({instr.name}),")

    writer.dedent()
    writer.writeln("}")
    writer.writeln("")

    generate_instructions(writer)


def generate_instructions(writer: Writer):
    for instr in INSTRUCTIONS:
        writer.writeln(
            '#[allow(non_camel_case_types, reason="easier to read, feels more asm-like")]'
        )
        writer.writeln('#[allow(dead_code, reason="generated code")]')
        writer.writeln("#[derive(Debug)]")
        writer.writeln(f"pub(crate) struct {instr.name} {{")
        writer.indent()

        for idx, operand in enumerate(instr.operands):
            writer.writeln(f"op{idx}: {operand.type()},")

        writer.dedent()
        writer.writeln("}")
        writer.writeln("")
        writer.writeln('#[allow(dead_code, reason="generated code")]')
        writer.writeln(f"impl {instr.name} {{")
        writer.indent()
        writer.write("pub(super) fn new(")

        for idx, operand in enumerate(instr.operands):
            writer.buf.write(f"op{idx}: impl Into<{operand.type()}>, ")

        writer.writeln(") -> Self {")
        writer.indent()
        writer.writeln("Self {")
        writer.indent()

        for idx, operand in enumerate(instr.operands):
            writer.writeln(f"op{idx}: op{idx}.into(),")

        writer.dedent()
        writer.writeln("}")
        writer.dedent()
        writer.writeln("}")
        writer.dedent()
        writer.writeln("}")
        writer.writeln("")

        writer.write(f"impl From<{instr.name}> for Instruction {{")
        writer.indent()
        writer.writeln(f"fn from(instr: {instr.name}) -> Self {{")
        writer.indent()
        writer.writeln(f"Self::Target(TargetInstruction::{instr.name}(instr))")
        writer.dedent()
        writer.writeln("}")
        writer.dedent()
        writer.writeln("}")
        writer.writeln("")
