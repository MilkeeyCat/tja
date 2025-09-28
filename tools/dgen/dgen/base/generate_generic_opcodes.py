from io import TextIOWrapper

from .generic_instruction import GENERIC_INSTRUCTIONS


def generate_generic_opcodes(buf: TextIOWrapper):
    buf.write(
        """#[derive(Debug)]
#[repr(usize)]
pub enum GenericOpcode {
"""
    )

    for instr in GENERIC_INSTRUCTIONS:
        buf.write(f"\t{instr.name},\n")

    buf.write("}\n")

    buf.write(
        f"""
impl GenericOpcode {{
    pub const fn num() -> usize {{
        {len(GENERIC_INSTRUCTIONS)}
    }}
}}
"""
    )
