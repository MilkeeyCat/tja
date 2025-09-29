from io import TextIOWrapper

from dgen.base.instruction import TARGET_INSTRUCTIONS
from dgen.writer import Writer


def generate_instruction_opcodes(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln("#[derive(Debug)]")
    writer.writeln("#[repr(usize)]")
    writer.writeln("pub enum Opcode {")
    writer.indent()
    writer.writeln("_Dummy = GenericOpcode::Num as usize - 1,")

    for instr in TARGET_INSTRUCTIONS:
        writer.writeln(f"{instr.name},")

    writer.dedent()
    writer.writeln("}")
