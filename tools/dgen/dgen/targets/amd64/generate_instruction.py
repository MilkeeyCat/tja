import importlib

from io import TextIOWrapper

from dgen.writer import Writer

from .operand import REGISTER, Generic
from .instruction import INSTRUCTIONS, Instruction


def generate_instruction(buf: TextIOWrapper):
    importlib.import_module(".instructions", __package__)
    writer = Writer(buf)

    writer.writeln("#[derive(Debug)]")
    writer.writeln(f"pub enum Instruction<R: Register> {{")
    writer.indent()

    for instr in INSTRUCTIONS:
        generics = instr.generics()

        writer.write(f"{instr.name}({instr.name}")

        if len(generics) > 0:
            writer.buf.write("<")

        for generic in instr.generics():
            match generic:
                case r if r is REGISTER:
                    writer.buf.write("R")
                case _:
                    assert False, "unhandled generic type"

        if len(generics) > 0:
            writer.buf.write(">")

        writer.buf.write("),\n")

    writer.dedent()
    writer.writeln("}")
    writer.writeln("")

    generate_instructions(writer)


def generate_instructions(writer: Writer):
    for instr in INSTRUCTIONS:
        generics = instr.generics()

        writer.writeln(f"#[derive(Debug)]")
        writer.write(f"pub struct {instr.name}")
        generic_parameters_list(writer, generics)
        writer.buf.write(" {\n")
        writer.indent()

        for idx, operand in enumerate(instr.operands):
            writer.writeln(f"op{idx}: {operand.print()},")

        writer.dedent()
        writer.writeln("}")
        writer.writeln("")

        writer.write(f"impl")
        generic_parameters_list(writer, generics)
        writer.buf.write(" ")
        instr_type(writer, instr)

        writer.buf.write(" {\n")
        writer.indent()
        writer.write("pub fn new(")

        for idx, operand in enumerate(instr.operands):
            writer.buf.write(f"op{idx}: {operand.print()}, ")

        writer.buf.write(") -> Self {\n")
        writer.indent()
        writer.write("Self { ")

        for idx, operand in enumerate(instr.operands):
            writer.buf.write(f"op{idx}, ")

        writer.buf.write("}\n")
        writer.dedent()
        writer.writeln("}")
        writer.dedent()
        writer.writeln("}")
        writer.writeln("")

        writer.write(f"impl")
        generic_parameters_list(writer, generics or [REGISTER])
        writer.buf.write(f" From<{instr.name}")
        type_arguments_list(writer, generics)
        writer.buf.write("> for Instruction<R> {\n")
        writer.indent()
        writer.write(f"fn from(instr: ")
        instr_type(writer, instr)
        writer.buf.write(") -> Self {\n")
        writer.indent()
        writer.writeln(f"Self::{instr.name}(instr)")
        writer.dedent()
        writer.writeln("}")
        writer.dedent()
        writer.writeln("}")
        writer.writeln("")


def generic_parameters_list(writer: Writer, generics: list[Generic]):
    if len(generics) > 0:
        writer.buf.write("<")

    for generic in generics:
        writer.buf.write(f"{generic.name}: {generic.trait_bound},")

    if len(generics) > 0:
        writer.buf.write(">")


def type_arguments_list(writer: Writer, generics: list[Generic]):
    if len(generics) > 0:
        writer.buf.write("<")

    for generic in generics:
        writer.buf.write(f"{generic.name}, ")

    if len(generics) > 0:
        writer.buf.write(">")


def instr_type(writer: Writer, instr: Instruction):
    writer.buf.write(instr.name)
    type_arguments_list(writer, instr.generics())
