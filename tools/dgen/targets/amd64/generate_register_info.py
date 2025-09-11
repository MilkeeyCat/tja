from io import TextIOWrapper

from dgen.base.register import REGISTERS
from dgen.base.constraints import REGISTER_CLASSES


def generate_register_info(buf: TextIOWrapper):
    buf.write(
        """impl RegisterInfo {
    pub fn new() -> Self {
        Self {
            register_descs: HashMap::from([
"""
    )

    for reg in REGISTERS:
        subregs = [f"Register::{reg.name.capitalize()}" for reg in reg.subregs]

        buf.write(
            f"""\t\t\t\t(
                    Register::{reg.name.capitalize()},
                    RegisterDesc {{
                        name: "{reg.name}",
                        size: {int(reg.bits /  8)},
                        subregs: &[{', '.join(subregs)}],
                    }},
                ),
"""
        )

    buf.write("\t\t\t]),\n")

    buf.write(
        """\t\t\tregister_classes: HashMap::from([
"""
    )

    for reg_class in REGISTER_CLASSES:
        regs = [
            f"Register::{reg.name.capitalize()}.into()" for reg in reg_class.registers
        ]

        buf.write(
            f"""\t\t\t\t(
                    RegisterClass::{reg_class.name}.into(),
                    vec![{', '.join(regs)}],
                ),
"""
        )

    buf.write("\t\t\t]),\n")
    buf.write("\t\t}\n")
    buf.write("\t}\n")
    buf.write("}\n")
