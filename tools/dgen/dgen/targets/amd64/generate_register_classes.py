from io import TextIOWrapper

from dgen.base.register_class import REGISTER_CLASSES
from dgen.writer import Writer


def generate_register_classes(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln("#[repr(usize)]")
    writer.writeln("pub enum RegisterClass {")
    writer.indent()

    for reg_class in REGISTER_CLASSES:
        writer.writeln(f"{reg_class.name},")

    writer.dedent()
    writer.writeln("}")
