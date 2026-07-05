from io import TextIOWrapper

from dgen.register import REGISTERS
from dgen.writer import Writer


def generate_register(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln('#[allow(dead_code, reason="generated code")]')
    writer.writeln("impl Register {")
    writer.indent()

    for i, reg in enumerate(REGISTERS):
        writer.writeln(f"pub(super) const {reg.name.upper()}: Self = Self({i});")

    writer.dedent()
    writer.writeln("}")

    writer.writeln("impl std::fmt::Display for Register {")
    writer.indent()
    writer.writeln(
        "fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {"
    )
    writer.indent()
    writer.writeln("match self {")
    writer.indent()

    for i, reg in enumerate(REGISTERS):
        writer.writeln(f'&Self::{reg.name.upper()} => write!(f, "{reg.name}"),')

    writer.writeln(f"_ => unreachable!(),")

    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
