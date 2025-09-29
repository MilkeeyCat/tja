from io import TextIOWrapper

from dgen.base.register import REGISTERS
from dgen.base.instruction import REGISTER_CLASSES
from dgen.writer import Writer


def generate_register_info(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln("impl RegisterInfo {")
    writer.indent()
    writer.writeln("pub fn new() -> Self {")
    writer.indent()
    writer.writeln("Self {")
    writer.indent()
    writer.writeln("register_descs: HashMap::from([")
    writer.indent()

    for reg in REGISTERS:
        subregs = [f"Register::{reg.name.capitalize()}" for reg in reg.subregs]

        writer.writeln("(")
        writer.indent()
        writer.writeln(f"Register::{reg.name.capitalize()},")
        writer.writeln("RegisterDesc {")
        writer.indent()
        writer.writeln(f'name: "{reg.name}",')
        writer.writeln(f"size: {int(reg.bits /  8)},")
        writer.writeln(f"subregs: &[{', '.join(subregs)}],")
        writer.dedent()
        writer.writeln("}")
        writer.dedent()
        writer.writeln("),")

    writer.dedent()
    writer.writeln("]),")

    writer.writeln("register_classes: HashMap::from([")
    writer.indent()

    for reg_class in REGISTER_CLASSES:
        regs = [
            f"Register::{reg.name.capitalize()}.into()" for reg in reg_class.registers
        ]

        writer.writeln("(")
        writer.indent()
        writer.writeln(f"RegisterClass::{reg_class.name}.into(),")
        writer.writeln(f"vec![{', '.join(regs)}],")
        writer.dedent()
        writer.writeln("),")

    writer.dedent()
    writer.writeln("]),")
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
