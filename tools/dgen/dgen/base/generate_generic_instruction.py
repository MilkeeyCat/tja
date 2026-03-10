import importlib

from io import TextIOWrapper

from dgen.writer import Writer

from .instruction import INSTRUCTIONS


def generate_generic_instruction(buf: TextIOWrapper):
    importlib.import_module(".instructions", __package__)
    writer = Writer(buf)

    writer.writeln("#[derive(Debug)]")
    writer.writeln(f"pub enum GenericInstruction<I: Instruction> {{")
    writer.indent()

    for instr in INSTRUCTIONS:
        writer.writeln(f"{instr.name} {{")
        writer.indent()

        for i, operand in enumerate(instr.outs + instr.ins):
            writer.writeln(f"op{i}: {str(operand)},")

        writer.dedent()
        writer.writeln("},")

    writer.writeln("Target(I),")
    writer.dedent()
    writer.writeln("}")
