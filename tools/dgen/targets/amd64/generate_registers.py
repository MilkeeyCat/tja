from io import TextIOWrapper

from dgen.base.register import REGISTERS


def generate_registers(buf: TextIOWrapper):
    buf.write(
        """#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(usize)]
pub enum Register {
"""
    )

    for reg in REGISTERS:
        buf.write(f"\t{reg.name.capitalize()},\n")

    buf.write("}\n")
    buf.write(
        f"""
impl Register {{
    pub const fn num() -> usize {{
        {len(REGISTERS)}
    }}
}}
"""
    )
