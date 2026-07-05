from io import TextIOWrapper

from dgen.register_class import REGISTER_CLASSES
from dgen.writer import Writer


def generate_register_class(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln('#[allow(dead_code, reason="generated code")]')
    writer.writeln("impl RegisterClass {")
    writer.indent()

    for i, reg_class in enumerate(REGISTER_CLASSES):
        writer.writeln(f"pub(super) const {reg_class.name.upper()}: Self = Self({i});")

    writer.dedent()
    writer.writeln("}")
