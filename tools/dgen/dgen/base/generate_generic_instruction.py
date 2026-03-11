import importlib

from io import TextIOWrapper

from dgen.writer import Writer

from .instruction import INSTRUCTIONS, Instruction


def generate_generic_instruction(buf: TextIOWrapper):
    importlib.import_module(".instructions", __package__)
    writer = Writer(buf)

    writer.writeln("#[derive(Debug)]")
    writer.writeln(f"pub enum GenericInstruction<I: Instruction> {{")
    writer.indent()

    for instr in INSTRUCTIONS:
        writer.write(f"{instr.name}({instr.name}")

        if is_instr_generic(instr):
            writer.buf.write("<I::Register>")

        writer.buf.write("),\n")

    writer.writeln("Target(I),")
    writer.dedent()
    writer.writeln("}")
    writer.writeln("")

    generate_instructions(writer)


def generate_instructions(writer: Writer):
    for instr in INSTRUCTIONS:
        is_generic = is_instr_generic(instr)

        writer.writeln("#[derive(Debug)]")
        writer.write(f"pub struct {instr.name}")

        if is_generic:
            writer.write("<R: Register>")

        writer.writeln("{")
        writer.indent()

        for [idx, operand] in enumerate(instr.outs + instr.ins):
            writer.writeln(f"pub op{idx}: {str(operand)},")

        writer.dedent()
        writer.writeln("}")
        writer.writeln("")

        writer.write("impl<I: Instruction")

        if is_generic:
            writer.buf.write("<Register = R>, R: Register")

        writer.buf.write(f"> From<{instr.name}")

        if is_generic:
            writer.buf.write("<R>")

        writer.buf.write("> for GenericInstruction<I> {\n")
        writer.indent()
        writer.write(f"fn from(instr: {instr.name}")

        if is_generic:
            writer.buf.write("<R>")

        writer.buf.write(") -> Self {\n")
        writer.indent()
        writer.writeln(f"Self::{instr.name}(instr)")
        writer.dedent()
        writer.writeln("}")
        writer.dedent()
        writer.writeln("}")
        writer.writeln("")


def is_instr_generic(instr: Instruction) -> bool:
    return any(map(lambda op: op.is_generic, instr.ins + instr.outs))
