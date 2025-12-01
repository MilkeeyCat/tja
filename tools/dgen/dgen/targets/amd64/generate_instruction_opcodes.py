from io import TextIOWrapper

from dgen.base.generic_instruction import GenericInstruction
from dgen.base.instruction import TARGET_INSTRUCTIONS, TargetInstruction
from dgen.writer import Writer


def generate_instruction_opcodes(buf: TextIOWrapper):
    writer = Writer(buf)

    writer.writeln("#[derive(Debug, Clone, Copy)]")
    writer.writeln("#[repr(usize)]")
    writer.writeln(f"pub enum {TargetInstruction.enum} {{")
    writer.indent()

    for idx, instr in enumerate(TARGET_INSTRUCTIONS):
        if idx == 0:
            writer.writeln(f"{instr.name} = {GenericInstruction.enum}::num(),")
        else:
            writer.writeln(f"{instr.name},")

    writer.dedent()
    writer.writeln("}")

    writer.writeln("")

    writer.writeln(f"impl {TargetInstruction.enum} {{")
    writer.indent()
    writer.writeln("pub const fn num() -> usize {")
    writer.indent()
    writer.writeln(str(len(TARGET_INSTRUCTIONS)))
    writer.dedent()
    writer.writeln("}")
    writer.dedent()
    writer.writeln("}")
