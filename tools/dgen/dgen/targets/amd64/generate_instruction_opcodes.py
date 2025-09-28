from io import TextIOWrapper

from dgen.base.instruction import TARGET_INSTRUCTIONS


def generate_instruction_opcodes(buf: TextIOWrapper):
    buf.write(
        """#[derive(Debug)]
#[repr(usize)]
pub enum Opcode {
	_Dummy = GenericOpcode::Num as usize - 1,
"""
    )

    for instr in TARGET_INSTRUCTIONS:
        buf.write(f"\t{instr.name},\n")

    buf.write("}")
