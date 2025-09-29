from io import TextIOWrapper

from dgen.base.register import REGISTERS
from dgen.writer import Writer


def generate_registers(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln("#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]")
    writer.writeln("#[repr(usize)]")
    writer.writeln("pub enum Register {")
    writer.indent()

    for reg in REGISTERS:
        writer.writeln(f"{reg.name.capitalize()},")

    writer.dedent()
    writer.writeln("}")
    buf.write("\n")
    writer.writeln("impl Register {")
    writer.indent()
    writer.writeln("pub const fn num() -> usize {")
    writer.indent()
    writer.writeln(str(len(REGISTERS)))
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
