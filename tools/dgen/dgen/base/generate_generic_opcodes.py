from io import TextIOWrapper

from dgen.writer import Writer

from .generic_instruction import GENERIC_INSTRUCTIONS, GenericInstruction


def generate_generic_opcodes(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln("#[derive(Debug)]")
    writer.writeln("#[repr(usize)]")
    writer.writeln(f"pub enum {GenericInstruction.enum} {{")
    writer.indent()

    for instr in GENERIC_INSTRUCTIONS:
        writer.writeln(f"{instr.name},")

    writer.dedent()
    writer.writeln("}")
    buf.write("\n")
    writer.writeln(f"impl {GenericInstruction.enum} {{")
    writer.indent()
    writer.writeln("pub const fn num() -> usize {")
    writer.indent()
    writer.writeln(str(len(GENERIC_INSTRUCTIONS)))
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
