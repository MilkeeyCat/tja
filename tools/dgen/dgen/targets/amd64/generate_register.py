from io import TextIOWrapper

from dgen.base.register import REGISTERS
from dgen.writer import Writer


def generate_register(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln("#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]")
    writer.writeln("#[repr(usize)]")
    writer.writeln("pub enum Register {")
    writer.indent()

    for reg in REGISTERS:
        writer.writeln(f"{reg.name.capitalize()},")

    writer.dedent()
    writer.writeln("}")
